library(tidyverse)
library(fixest)
library(ggthemes)
library(fastDummies)
options(knitr.kable.NA = '')
dropbox <- ""
# set ggplot theme
theme_set(theme_clean() + theme(plot.background = element_blank(),
                                legend.background = element_blank()))

# load the data
# data 1 is the KW data
data1 <-  haven::read_dta(here::here("Data/KW", "maindata.dta"))

# data4 is our data with all changes made
data4 <- read_rds(here::here("Data/COMPILED", "data4.rds"))

# save long covs and short covs in vectors
long_covs <- c("size", "age", "size2", "age2", "gen1", "cs", "dd", 
               "fp", "csXcts", "bcXamanda")

short_covs <- c("size", "age", "size2", "age2")

# save different fes
long_fes <- "| firm^dataset + state_year^dataset + industry_year^dataset"
long_fes_ff <-  "| firm^dataset + state_year^dataset + industry_ff_year^dataset"
short_fes <- "| firm^dataset + year^dataset"

# put the dependent variables into a vector
depvars <- c("roa", "capEx", "ppegrowth", "assetgrowth", "cash", "sga", "leverage")

# function to run event studies and plot the coefficient paths.
make_dt <- function(dt, law) {
  
  # get switches
  switches <- dt %>% 
    group_by(gvkey) %>% 
    mutate(switch = if_else({{law}} == 1 & lag({{law}}) == 0, 1, 0)) %>% 
    filter(switch == 1 & year <= 1995) %>% 
    select(gvkey, treat_year = year)
  
  # merge in
  dt <- dt %>% 
    select(gvkey, year, firm, state, state_year, starts_with("industry"), all_of(long_covs), 
           bc, pp, incorporation, all_of(depvars), bcXmotivatingfirmbc, ppXmotivatingfirmpp) %>% 
    filter(year %>% between(1976, 1995)) %>% 
    left_join(switches, by = "gvkey") %>%
    group_by(gvkey) %>% 
    # replace switch_year to NA if bc = 0 and after last switch
    mutate(treat_year = if_else({{law}} == 0 & year >= max(treat_year), NA_real_, treat_year)) %>% 
    distinct() %>% 
    group_by(gvkey, year) %>%
    filter(case_when(
      {{law}} == 0 ~ is.na(treat_year) | treat_year == min(treat_year[which(treat_year >= year)]),
      {{law}} == 1 ~ is.na(treat_year) | treat_year == max(treat_year[which(treat_year <= year)])
    ))
}

# make datasets for BC and PP laws
data_bc <- make_dt(data4, bc)
data_pp <- make_dt(data4, pp)

# function to make stack dataset
make_stacked <- function(dt, law) {
  
  # get the years for the sub datasets - need at least 10 treated units in a year to be included
  yrs <- sort(unique(dt$treat_year))
  # force there to be at least one post-treated year
  yrs <- yrs[which(yrs <= 1995)]
  
  # remove treat years with fewer than 10 treated firms
  isgood <- function(tyr) {
    # get a list of treated firms to keep
    treats <- dt %>% 
      # treated in that year 
      filter(treat_year == tyr) %>% 
      # needs to have observations for t - 1, t = 0, t + 1
      group_by(gvkey) %>% 
      filter(length(gvkey[which(year %>% between(tyr - 1, tyr + 1))]) == 3) %>% 
      pull(gvkey) %>% 
      unique()
    
    length(treats) >= 10
  }
  
  # keep only greater than or equal to 10 firms
  yrs <- yrs[unlist(map(yrs, isgood))]
  
  # sub function for a given year stack
  stack <- function(tyr) {
    
    # get a list of treated firms to keep
    treats <- dt %>% 
      # treated in that year 
      filter(treat_year == tyr) %>% 
      # needs to have observations for t - 1, t = 0, t + 1
      group_by(gvkey) %>% 
      filter(length(gvkey[which(year %>% between(tyr - 1, tyr + 1))]) == 3) %>% 
      pull(gvkey) %>% 
      unique()
    
    # get a list of potential controls 
    controls <- dt %>% 
      # not treated in that year
      filter(is.na(treat_year) | treat_year != tyr) %>% 
      # needs to have observations for t - 1, t = 0, t + 1 (all controls so law is off)
      group_by(gvkey) %>% 
      filter(length(gvkey[which(year %>% between(tyr - 1, tyr + 1))]) == 3) %>% 
      filter(sum({{law}}[which(year %>% between(tyr - 1, tyr + 1))]) == 0) %>% 
      pull(gvkey)
    
    # combine the data
    bind_rows(
      # keep treated firms, in -5 to + 5, with no treatment changes in that period
      dt %>% filter(gvkey %in% treats & year %>% between(tyr - 5, tyr + 5) & treat_year == tyr) %>% mutate(treat = 1),
      # keep control firms, in -5 to + 5, when the law is 0 in that period
      dt %>% filter(gvkey %in% controls & year %>% between(tyr - 5, tyr + 5) & {{law}} == 0) %>% mutate(treat = 0)
    ) %>% 
      # make a dataset specific variable
      mutate(
        dataset = tyr,
        # make a relative time variable for time indicator dummies - set to NA for controls
        rel_year = if_else(treat == 1, year - tyr, NA_real_)
      )
  }
  
  # run over all the treat_years
  map_dfr(yrs, stack) %>% 
    # make dummy cols and replace missing to 0
    dummy_cols(select_columns = "rel_year", remove_selected_columns = FALSE, ignore_na = TRUE) %>% 
    mutate(across(starts_with("rel_year_"), ~replace_na(., 0)))
}

# make stacked bc and pp law datasets
stacked_bc <- make_stacked(data_bc, bc)
stacked_pp <- make_stacked(data_pp, pp)

# make code for event studies
run_es <- function(law, dt, covs, fes) {
  
  # get the model type
  covtype <- if(identical(covs, "")) "Model 1" else
    if(identical(covs, short_covs)) "Model 2" else "Model 3"
  
  # for long covs need to add in control for bc or pp (opposite of law in Question)
  covs <- if(covtype == "Model 3" & law == "bc") 
    c(covs, "pp", "bcXmotivatingfirmbc") else
      if(covtype == "Model 3" & law == "pp") 
        c(covs, "bc", "ppXmotivatingfirmpp") else
          covs
  
  # get the relative year indicators and make formula
  covs <- c(covs, c(paste0("`", "rel_year_", c(-5:-2, 0:5), "`")))
  
  # get a list of the time dummies we want to keep
  keepvars <- c(paste0("`", "rel_year_", -5:-2, "`"),
                paste0("rel_year_", 0:5))
  
  # make a function to run the model
  run_mod <- function(depvar, dt, covs, fes) {
    broom::tidy(feols(as.formula(paste(c(depvar, " ~ ", paste(covs, collapse = " + "),
                                         fes), collapse = "")), data = dt, cluster = "incorporation^dataset"), 
                se = "cluster", conf.int = TRUE) %>% 
      mutate(var = depvar)
  }
  
  # run model over our dependent variables
  map_dfr(depvars, run_mod, dt = dt, covs = covs, fes = fes) %>%
    # keep just -5 to 5
    filter(term %in% keepvars) %>% 
    # by variable add in the t - 1
    group_by(var) %>%
    # add in a timing variable
    mutate(t = c(-5:-2, 0:5)) %>% 
    # drop unnecessary variables from broom::tidy
    select(var, t, estimate, conf.low, conf.high) %>%
    bind_rows(
      tibble(
        var = depvars,
        t = rep(-1, length(depvars)),
        estimate = rep(0, length(depvars)),
        conf.low = rep(0, length(depvars)),
        conf.high = rep(0, length(depvars))
      )
    ) %>% 
    # add in identifying information
    mutate(law = law,
           covs = covtype)
}

# run the three sets of BC models and save
bcdata <- bind_rows(
  run_es(law = "bc", dt = stacked_bc, covs = "", fes = long_fes_ff),
  run_es(law = "bc", dt = stacked_bc, covs = short_covs, fes = long_fes_ff),
  run_es(law = "bc", dt = stacked_bc, covs = long_covs, fes = long_fes_ff),
)

# matrix with dataset names
depvar_names <- tribble(
  ~var, ~varname,
  "roa", "ROA",
  "capEx", "Capex",
  "ppegrowth", "PPE Growth",
  "assetgrowth", "Asset Growth",
  "cash", "Cash",
  "sga", "SGA Expense",
  "leverage", "Leverage"
)

# plot event study estimates
fig7 <- bcdata %>% 
  left_join(depvar_names, by = "var") %>% 
  mutate(varname = factor(varname, levels = c("ROA", "Capex", "PPE Growth", "Asset Growth",
                                              "Cash", "SGA Expense", "Leverage"))) %>%
  ggplot(aes(x = t, y = estimate)) + 
  geom_point(fill = "white", shape = 21) + geom_line() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                linetype = "longdash") + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "#800000FF") + 
  geom_vline(xintercept = -0.5,  linetype = "longdash", color = "gray") + 
  labs(y = "Estimate", x = "Years Relative to Passage") + 
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
  scale_y_continuous(position = "right") + 
  labs(y = "") + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        strip.background = element_rect(color = "black", linetype = 1),
        axis.text.y = element_text(hjust = 0.95)) + 
  facet_grid(vars(varname), vars(covs), scales = "free", switch = "y")

# save
ggsave(fig7, filename = paste(dropbox, "fig7.png", sep = ""), dpi = 500,
       width = 7.5, height = 9)

## Make the same plot for PP laws
# run the three sets of BC models and save
ppdata <- bind_rows(
  run_es(law = "pp", dt = stacked_pp, covs = "", fes = long_fes_ff),
  run_es(law = "pp", dt = stacked_pp, covs = short_covs, fes = long_fes_ff),
  run_es(law = "pp", dt = stacked_pp, covs = long_covs, fes = long_fes_ff),
)

# plot event study estimates
fig8 <- ppdata %>% 
  left_join(depvar_names, by = "var") %>% 
  mutate(varname = factor(varname, levels = c("ROA", "Capex", "PPE Growth", "Asset Growth",
                                              "Cash", "SGA Expense", "Leverage"))) %>%
  ggplot(aes(x = t, y = estimate)) + 
  geom_point(fill = "white", shape = 21) + geom_line() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                linetype = "longdash") + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "#800000FF") + 
  geom_vline(xintercept = -0.5,  linetype = "longdash", color = "gray") + 
  labs(y = "Estimate", x = "Years Relative to Passage") + 
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
  scale_y_continuous(position = "right") + 
  labs(y = "") + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        strip.background = element_rect(color = "black", linetype = 1),
        axis.text.y = element_text(hjust = 0.95)) + 
  facet_grid(vars(varname), vars(covs), scales = "free", switch = "y")

# save
ggsave(fig8, filename = paste(dropbox, "fig8.png", sep = ""), dpi = 500,
       width = 7.5, height = 9)

#### Now make the versions for the appendix - uses KW data
# make datasets for BC and pp laws
data_bc <- make_dt(data1, bc)
data_pp <- make_dt(data1, pp)

# make stacked bc and pp law datasets
stacked_bc <- make_stacked(data_bc, bc)
stacked_pp <- make_stacked(data_pp, pp)

# run the three sets of BC models and save
bcdata <- bind_rows(
  run_es(law = "bc", dt = stacked_bc, covs = "", fes = long_fes),
  run_es(law = "bc", dt = stacked_bc, covs = short_covs, fes = long_fes),
  run_es(law = "bc", dt = stacked_bc, covs = long_covs, fes = long_fes),
)

# plot event study estimates
figB3 <- bcdata %>% 
  left_join(depvar_names, by = "var") %>% 
  mutate(varname = factor(varname, levels = c("ROA", "Capex", "PPE Growth", "Asset Growth",
                                              "Cash", "SGA Expense", "Leverage"))) %>%
  ggplot(aes(x = t, y = estimate)) + 
  geom_point(fill = "white", shape = 21) + geom_line() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                linetype = "longdash") + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "#800000FF") + 
  geom_vline(xintercept = -0.5,  linetype = "longdash", color = "gray") + 
  labs(y = "Estimate", x = "Years Relative to Passage") + 
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
  scale_y_continuous(position = "right") + 
  labs(y = "") + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        strip.background = element_rect(color = "black", linetype = 1),
        axis.text.y = element_text(hjust = 0.95)) + 
  facet_grid(vars(varname), vars(covs), scales = "free", switch = "y")

# save
ggsave(figB3, filename = paste(dropbox, "figB3.png", sep = ""), dpi = 500,
       width = 7.5, height = 9)

## Make the same plot for PP laws
# run the three sets of BC models and save
ppdata <- bind_rows(
  run_es(law = "pp", dt = stacked_pp, covs = "", fes = long_fes),
  run_es(law = "pp", dt = stacked_pp, covs = short_covs, fes = long_fes),
  run_es(law = "pp", dt = stacked_pp, covs = long_covs, fes = long_fes),
)

# plot event study estimates
figB4 <- ppdata %>% 
  left_join(depvar_names, by = "var") %>% 
  mutate(varname = factor(varname, levels = c("ROA", "Capex", "PPE Growth", "Asset Growth",
                                              "Cash", "SGA Expense", "Leverage"))) %>%
  ggplot(aes(x = t, y = estimate)) + 
  geom_point(fill = "white", shape = 21) + geom_line() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                linetype = "longdash") + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "#800000FF") + 
  geom_vline(xintercept = -0.5,  linetype = "longdash", color = "gray") + 
  labs(y = "Estimate", x = "Years Relative to Passage") + 
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
  scale_y_continuous(position = "right") + 
  labs(y = "") + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        strip.background = element_rect(color = "black", linetype = 1),
        axis.text.y = element_text(hjust = 0.95)) + 
  facet_grid(vars(varname), vars(covs), scales = "free", switch = "y")

# save
ggsave(figB4, filename = paste(dropbox, "figB4.png", sep = ""), dpi = 500,
       width = 7.5, height = 9)