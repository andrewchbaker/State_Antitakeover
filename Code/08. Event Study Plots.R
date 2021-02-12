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
long_fes <- "| firm + state_year + industry_year"
long_fes_ff <-  "| firm + state_year + industry_ff_year"

# function to clean data - bring in relative time indics by law.
make_dt <- function(dt, law) {
  
  # first make relative time dummies
  # get switches
  switches <- dt %>% 
    group_by(gvkey) %>% 
    # switch is either change from 0 - 1 or starting with 1
    mutate(switch = if_else({{law}} == 1 & lag({{law}}) == 0 | 
                              {{law}} == 1 & is.na(lag({{law}})), 1, 0)) %>% 
    # keep only treatments that occur within our data
    filter(switch == 1 & year <= 1995) %>% 
    select(gvkey, treat_year = year)
  
  # merge in and make time dummies
  dt <- dt %>% 
    left_join(switches, by = "gvkey") %>%
    group_by(gvkey) %>% 
    # replace switch_year to NA if bc = 0 and after last switch
    mutate(treat_year = if_else({{law}} == 0 & year >= max(treat_year), NA_real_, treat_year)) %>% 
    distinct() %>% 
    group_by(gvkey, year) %>%
    filter(case_when(
      {{law}} == 0 ~ is.na(treat_year) | treat_year == min(treat_year[which(treat_year >= year)]),
      {{law}} == 1 ~ is.na(treat_year) | treat_year == max(treat_year[which(treat_year <= year)])
    )) %>% 
    # if first observation is already treated drop that firm
    group_by(gvkey) %>% 
    filter({{law}}[year == min(year)] != 1) %>% 
    arrange(gvkey, year) %>% 
    ungroup() %>% 
    # make relative time dummies
    mutate(rel_year = year - treat_year) %>% 
    dummy_cols(select_columns = "rel_year", remove_selected_columns = FALSE,
               ignore_na = TRUE) %>% 
    mutate(across(starts_with("rel_year_"), ~replace_na(., 0)))
  
}

# make datasets for BC and pp laws
data_bc <- make_dt(data4, bc)
data_pp <- make_dt(data4, pp)

# put the dependent variables into a vector
depvars <- c("roa", "capEx", "ppegrowth", "assetgrowth", "cash", "sga", "leverage")

# get a list of the time dummies we want to keep
keepvars <- c(paste0("`", "rel_year_", -5:-2, "`"),
              paste0("rel_year_", 0:5))

# make code for event studies
run_es <- function(law, dt, covs, fes) {
  
  # which model is it?
  covtype <- if(identical(covs, "")) "Model 1" else
      if(identical(covs, short_covs)) "Model 2" else "Model 3"
  
  # for long covs need to add in control for bc or pp (opposite of law in Question)
  covs <- if(covtype == "Model 3" & law == "bc") 
    c(covs, "pp", "bcXmotivatingfirmbc") else
      if(covtype == "Model 3" & law == "pp") 
        c(covs, "bc", "ppXmotivatingfirmpp") else
          covs
  
  # get the relative year indicators - not most negative year and year = -1
  yrs <- sort(unique(dt$rel_year))
  yrs <- yrs[which(yrs != min(yrs) & yrs != -1)]
  
  # make covariate vector
  covs <- c(covs, c(paste0("`", "rel_year_", yrs, "`")))
  
  # filter dataset
  dt <- dt %>% filter(year %>% between(1976, 1995))
  
  # make a function to run the model
  run_mod <- function(depvar, dt, covs, fes) {
    broom::tidy(feols(as.formula(paste(c(depvar, " ~ ", paste(covs, collapse = " + "),
                                         fes), collapse = "")), data = dt), 
                se = "cluster", cluster = dt$incorporation, conf.int = TRUE) %>% 
      mutate(var = depvar)
  }
  
  # run model over our dependent variables
  map_dfr(depvars, run_mod, dt = dt, covs = covs, fes = fes) %>%
    # keep just the time periods we want
    filter(term %in% keepvars) %>% 
    # by variable add in the t - 1
    group_by(var) %>%
    # add in a timing variable
    mutate(t = c(-5:-2, 0:5)) %>% 
    # drop unnecessary variables from broom::tidy
    select(var, t, estimate, conf.low, conf.high) %>%
    # add in the -1 data
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
  run_es(law = "bc", dt = data_bc, covs = "", fes = long_fes_ff),
  run_es(law = "bc", dt = data_bc, covs = short_covs, fes = long_fes_ff),
  run_es(law = "bc", dt = data_bc, covs = long_covs, fes = long_fes_ff),
)

# matrix with dependent variable names to merge in
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
fig5 <- bcdata %>% 
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
  scale_x_continuous(breaks = seq(-5, 10, by = 1)) + 
  scale_y_continuous(position = "right") + 
  labs(y = "") + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        strip.background = element_rect(color = "black", linetype = 1),
        axis.text.y = element_text(hjust = 0.95)) + 
  facet_grid(vars(varname), vars(covs), scales = "free", switch = "y")

# save
ggsave(fig5, filename = paste(dropbox, "fig5.png", sep = ""), dpi = 500,
       width = 7.5, height = 9)

### Now make the plot for pp laws
# run the three sets of BC models and save
ppdata <- bind_rows(
  run_es(law = "pp", dt = data_pp, covs = "", fes = long_fes_ff),
  run_es(law = "pp", dt = data_pp, covs = short_covs, fes = long_fes_ff),
  run_es(law = "pp", dt = data_pp, covs = long_covs, fes = long_fes_ff),
)

# plot event study estimates
fig6 <- ppdata %>% 
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
  scale_x_continuous(breaks = seq(-5, 10, by = 1)) + 
  scale_y_continuous(position = "right") + 
  labs(y = "") + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        strip.background = element_rect(color = "black", linetype = 1),
        axis.text.y = element_text(hjust = 0.95)) + 
  facet_grid(vars(varname), vars(covs), scales = "free", switch = "y")

# save
ggsave(fig6, filename = paste(dropbox, "fig6.png", sep = ""), dpi = 500,
       width = 7.5, height = 9)

### Finally, redo the estimates with the KW data
# make datasets for BC and pp laws
data_bc <- make_dt(data1, bc)
data_pp <- make_dt(data1, pp)

# run the three sets of BC models and save
bcdata <- bind_rows(
  run_es(law = "bc", dt = data_bc, covs = "", fes = long_fes),
  run_es(law = "bc", dt = data_bc, covs = short_covs, fes = long_fes),
  run_es(law = "bc", dt = data_bc, covs = long_covs, fes = long_fes),
)

# plot event study estimates
figB1 <- bcdata %>% 
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
  scale_x_continuous(breaks = seq(-5, 10, by = 1)) + 
  scale_y_continuous(position = "right") + 
  labs(y = "") + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        strip.background = element_rect(color = "black", linetype = 1),
        axis.text.y = element_text(hjust = 0.95)) + 
  facet_grid(vars(varname), vars(covs), scales = "free", switch = "y")

# save
ggsave(figB1, filename = paste(dropbox, "figB1.png", sep = ""), dpi = 500,
       width = 7.5, height = 9)

# run the three sets of PP models and save
ppdata <- bind_rows(
  run_es(law = "pp", dt = data_pp, covs = "", fes = long_fes),
  run_es(law = "pp", dt = data_pp, covs = short_covs, fes = long_fes),
  run_es(law = "pp", dt = data_pp, covs = long_covs, fes = long_fes),
)

# plot event study estimates
figB2 <- ppdata %>% 
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
  scale_x_continuous(breaks = seq(-5, 10, by = 1)) + 
  scale_y_continuous(position = "right") + 
  labs(y = "") + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        strip.background = element_rect(color = "black", linetype = 1),
        axis.text.y = element_text(hjust = 0.95)) + 
  facet_grid(vars(varname), vars(covs), scales = "free", switch = "y")

# save
ggsave(figB2, filename = paste(dropbox, "figB2.png", sep = ""), dpi = 500,
       width = 7.5, height = 9)
