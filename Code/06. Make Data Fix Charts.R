library(tidyverse)
library(kableExtra)
library(patchwork)
library(fixest)
library(ggthemes)
library(ggsci)
options(knitr.kable.NA = '')
dropbox <- ""

# set ggplot theme
theme_set(
  theme_clean() + 
    theme(plot.background = element_blank(),
          legend.background = element_rect(color = "white"))
)

# load our four datasets
data1 <- haven::read_dta(here::here("Data/KW", "maindata.dta")) %>% 
  filter(year %>% between(1976, 1995))

data2 <- read_rds(here::here("Data/COMPILED", "data2.rds")) %>% 
  filter(year %>% between(1976, 1995))

data3 <-  read_rds(here::here("Data/COMPILED", "data3.rds")) %>% 
  filter(year %>% between(1976, 1995))

data4 <-  read_rds(here::here("Data/COMPILED", "data4.rds")) %>% 
  filter(year %>% between(1976, 1995))

# make dataset to merge in column names 
match_name <- tribble(
  ~term, ~name,
  "bc", "Business combination \n law (BC)",
  "gen1", "First-generation law",
  "pp", "Poison Pill law (PP)",
  "cs", "Control share \n acquisition law (CS)",
  "dd", "Directors' duties law \n (DD)",
  "fp", "Fair price law (FP)",
  "csXcts", "CS x CTS",
  "bcXamanda", "BC x Amanda",
  "bcXmotivatingfirmbc", "BC x MF (motivating \n firms)",
  "size", "Size",
  "age", "Age",
  "size2", "Size Squared",
  "age2", "Age Squared"
)

# function to get significance stars
make_stars <- function(b, se, dof) {
  t <- abs(b / se)
  if (2 * pt(-t, df=dof) < 0.01) {
    ptstar <- "***"
  } else if (2 * pt(-t, df=dof) < 0.05) {
    ptstar <- "**"
  } else if (2 * pt(-t, df=dof) < 0.1) {
    ptstar <- "*"
  } else {
    ptstar <- ""
  }
  return(ptstar)
}

# run the models two ways and save the estimates in a tibble
# covariates for short regression
cov1 <- c("bc", "size", "age", "size2", "age2")

# covariates for long regression
cov2 <- c("bc", "size", "age", "size2", "age2", "gen1", "pp", "cs", 
          "dd", "fp", "csXcts", "bcXamanda", "bcXmotivatingfirmbc")

# two different fes
fe1 <- "| firm + state_year + industry_year"
fe2 <- "| firm + state_year + industry_ff_year"

# function to run models based on dep variable and covariate set
runmod <- function(depvar, covs, dt, fes, modname, dataname) {
  mod <- feols(as.formula(paste(c(depvar, " ~ ", paste(covs, collapse = " + "),
                          fes), collapse = "")),
        data = dt, cluster = "incorporation")
  
  # get data from model into a tibble
  broom::tidy(mod, conf.int = TRUE) %>% 
    mutate(model = modname, data = dataname, var = depvar,
           Obs = scales::comma(mod$nobs),
           R2 = format(round(broom::glance(mod)$adj.r.squared, 3), nsmall = 3))
}

# put dependent variables in a vector and make table
depvar <- c("roa", "capEx", "ppegrowth", "assetgrowth", "cash", "sga", "leverage")

# combine all models into one dataset
all_models_data <- bind_rows(
  map_dfr(depvar, .f = runmod, covs = cov1, dt = data1, fes = fe1, 
          modname = "mod1", dataname = "data1"),
  map_dfr(depvar, .f = runmod, covs = cov2, dt = data1, fes = fe1, 
          modname = "mod2", dataname = "data1"),
  map_dfr(depvar, .f = runmod, covs = cov1, dt = data2, fes = fe1, 
          modname = "mod1", dataname = "data2"),
  map_dfr(depvar, .f = runmod, covs = cov2, dt = data2, fes = fe1, 
          modname = "mod2", dataname = "data2"),
  map_dfr(depvar, .f = runmod, covs = cov1, dt = data3, fes = fe1, 
          modname = "mod1", dataname = "data3"),
  map_dfr(depvar, .f = runmod, covs = cov2, dt = data3, fes = fe1, 
          modname = "mod2", dataname = "data3"),
  map_dfr(depvar, .f = runmod, covs = cov1, dt = data4, fes = fe2, 
          modname = "mod1", dataname = "data4"),
  map_dfr(depvar, .f = runmod, covs = cov2, dt = data4, fes = fe2, 
          modname = "mod2", dataname = "data4"),
)

### Make Table 2 - keep only leverage and make table
table2 <- all_models_data %>% 
  filter(var == "leverage") %>% 
  # grab just the variables we need
  select(term, estimate, std.error, model, data) %>% 
  # get stars onto estimates
  rowwise() %>% 
  mutate(stars = make_stars(estimate, std.error, 70000),
         estimate = paste0(format(round(estimate, 3), nsmall = 3), stars),
         std.error = paste0("(", format(round(std.error, 3), nsmall = 3), ")")) %>% 
  ungroup() %>% 
  select(-stars) %>% 
  # drop the control variables
  filter(!(term %in% c("size", "age", "size2", "age2"))) %>% 
  # merge in names
  left_join(., match_name) %>% 
  select(name, estimate, std.error, model, data) %>% 
  # reshape
  pivot_longer(
    cols = c("estimate", "std.error"),
    names_to = "estimate",
    values_to = "value"
  ) %>% 
  arrange(model, data) %>%
  pivot_wider(names_from = c("data", "model"), values_from = "value") %>% 
  # drop some of the names
  mutate(name = ifelse(estimate == "std.error", "", name)) %>% 
  select(-estimate) %>% 
  ## add in Obs and R2
  bind_rows(
    as_tibble(
      rbind(
        c("Obs",
          all_models_data %>% 
            filter(var == "leverage") %>% 
            arrange(model, data) %>%
            group_by(model, data) %>% 
            slice(1) %>% 
            pull(Obs)),
        c("R2", all_models_data %>% 
            filter(var == "leverage") %>% 
            arrange(model, data) %>%
            group_by(model, data) %>% 
            slice(1) %>% 
            pull(R2))
      )
    ) %>% 
      # set the names
      set_names(c("name", paste0("data", 1:4, "_mod1"), paste0("data", 1:4, "_mod2")))
  ) %>% 
  mutate(name = c("Business combination", "law (BC)", "First-generation law", "", "Poison Pill law (PP)", 
                  "", "Control share", "acquisition law (CS)", "Directors' duties law", 
                  "(DD)", "Fair price law (FP)", "", "CS x CTS", "", "BC x Amanda", "", 
                  "BC x MF (motivating ", " firms)", "Observations", "R2")) %>% 
  kable(format = "latex", digits = 3, booktabs = T, align = 'lcccccccc',
        label = "table2", caption = "Comparison of DiD Estimates on Leverage Across Data and Models",
        col.names = linebreak(c(" ", rep(paste0("(", 1:4, ")"), 2)), align = 'c'),
        linesep = "") %>% 
  kable_styling(latex_options = c("HOLD_position"), 
                font_size = 8, full_width = T) %>% 
  column_spec(1, width = "4cm") %>% 
  add_header_above(c(" " = 1, "Short Regression Model" = 4, "Full Model" = 4)) %>% 
  add_header_above(c(" " = 1, "Leverage" = 8))

# save
write_lines(table2, file = paste(dropbox, "table2.tex", sep = ""))

## Make Figure 3 - compare results on the BC statute across models
# dataset for dependent variable names
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

# make plot for short model
fig3a <- all_models_data %>%
  # keep just short regression models and just the bc variable we are going to plot
  filter(model == "mod1" & term == "bc") %>% 
  # bring in dependent variable names
  left_join(depvar_names, by = "var") %>% 
  mutate(name = -parse_number(data)) %>% 
  # refactor name so they stay in order
  mutate(varname = factor(varname, levels = c("ROA", "Capex", "PPE Growth", "Asset Growth",
                                              "Cash", "SGA Expense", "Leverage"))) %>%
  # plot
  ggplot() +
  geom_pointrange(aes(x = name, y = estimate, ymin = conf.low, ymax = conf.high),
                  color = "#767676FF") + 
  geom_hline(yintercept = 0, color = "#800000FF", linetype = "longdash") + 
  labs(x = "Data", y = "") + 
  coord_flip() + 
  facet_wrap(~varname, scales = "free_x", nrow = 1) + 
  ggtitle("Short Regression Model") + 
  scale_x_continuous(
    labels =  scales::dollar_format(negative_parens = TRUE, prefix = "")
  ) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        plot.title = element_text(hjust = 0.5, size = 14)) +
  guides(x = guide_axis(n.dodge = 2))

# Same thing but for full regression model
fig3b <- all_models_data %>% 
  filter(model == "mod2" & term == "bc") %>% 
  left_join(depvar_names, by = "var") %>% 
  mutate(name = -parse_number(data)) %>% 
  mutate(varname = factor(varname, levels = c("ROA", "Capex", "PPE Growth", "Asset Growth",
                                      "Cash", "SGA Expense", "Leverage"))) %>%
  ggplot() +
  geom_pointrange(aes(x = name, y = estimate, ymin = conf.low, ymax = conf.high),
                  color = "#767676FF") + 
  geom_hline(yintercept = 0, color = "#800000FF", linetype = "longdash") + 
  labs(x = "Data", y = "") + 
  coord_flip() + 
  facet_wrap(~varname, scales = "free_x", nrow = 1) + 
  ggtitle("Full Model") + 
  scale_x_continuous(
    labels =  scales::dollar_format(negative_parens = TRUE, prefix = "")
  ) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        plot.title = element_text(hjust = 0.5, size = 14)) +
  guides(x = guide_axis(n.dodge = 2))

# combine the two panels
fig3 <- fig3a + fig3b + plot_layout(nrow = 2)

# save
ggsave(fig3, filename = paste(dropbox, "fig3.png", sep = ""), dpi = 500,
       width = 11, height = 5.5)

# Make tables for Data 2-4 for the appendix
# function to run models based on dep variable and covariate set
runmod <- function(depvar, covs, dt, fe) {
  feols(as.formula(paste(c(depvar, " ~ ", paste(covs, collapse = " + "),
                           fe), collapse = "")),
        data = dt, cluster = "incorporation")
}

# function to run by dep var
maketable <- function(depvar, dt, fe) {
    
    # estimate the models on our data and the KW data
    mod1 <- runmod(depvar, cov1, dt, fe)
    mod2 <- runmod(depvar, cov2, dt, fe)
    
    # combine the data estimates together and report the table
    bind_rows(
      broom::tidy(mod1) %>% mutate(mod = "mod1"),
      broom::tidy(mod2) %>% mutate(mod = "mod2")) %>% 
      # grab just the variables we need
      select(term, estimate, std.error, mod) %>% 
      # get stars onto estimates
      rowwise() %>% 
      mutate(stars = make_stars(estimate, std.error, 70000),
             estimate = paste0(format(round(estimate, 3), nsmall = 3), stars),
             std.error = paste0("(", format(round(std.error, 3), nsmall = 3), ")")) %>% 
      ungroup() %>% 
      select(-stars) %>% 
      # drop the control variables
      filter(!(term %in% c("size", "age", "size2", "age2"))) %>% 
      # merge in names
      left_join(., match_name) %>% 
      select(name, estimate, std.error, mod) %>% 
      # reshape
      pivot_longer(
        cols = c("estimate", "std.error"),
        names_to = "estimate",
        values_to = "value"
      ) %>% 
      pivot_wider(names_from = "mod", values_from = "value") %>% 
      # drop some of the names
      mutate(name = ifelse(estimate == "std.error", "", name)) %>% 
      select(-estimate) %>% 
      # add in the N and r2
      bind_rows(
        tibble(
          name = c("Observations", "R2"),
          mod1 = c(scales::comma(mod1$nobs), format(round(broom::glance(mod1)$adj.r.squared, 3), nsmall = 3)),
          mod2 = c(scales::comma(mod2$nobs), format(round(broom::glance(mod2)$adj.r.squared, 3), nsmall = 3))
        )
      ) %>% 
      # keep name only if first depvar
      { if (depvar != "roa") select(., -name) else . }
}

# make table using data 2
tbl <- map_dfc(depvar, maketable, dt = data2, fe = fe1)

# fix up some weird naming issues manually
tbl <- tbl %>% 
  mutate(name = c("Business combination", "law (BC)", "First-generation law", "", "Poison Pill law (PP)", 
                  "", "Control share", "acquisition law (CS)", "Directors' duties law", 
                  "(DD)", "Fair price law (FP)", "", "CS x CTS", "", "BC x Amanda", "", 
                  "BC x MF (motivating ", " firms)", "Observations", "R2"))

# save a latex table and export
tableA2 <- tbl %>% 
  kable(format = "latex", digits = 3, booktabs = T, align = 'l',
        label = "tableA2", caption = "Replication of DiD Results With Data (2)",
        col.names = linebreak(c("", rep(c("Short Regres- sion", "Full Model"), 7)), 
                              align = 'c'),
        linesep = "") %>% 
  kable_styling(latex_options = c("scale_down"),
                font_size = 6) %>% 
  column_spec(2:15, width = "1.1cm") %>% 
  column_spec(1, width = "2.75cm") %>% 
  row_spec(0, align = "c") %>% 
  add_header_above(c(" " = 1, "(1) ROA" = 2, "(2) Capex" = 2, "(3) PPE Growth" = 2,
                     "(4) Asset Growth" = 2, "(5) Cash" = 2, "(6) SGA Expense" = 2,
                     "(7) Leverage" = 2)) %>% 
  add_header_above(c(" " = 1, "Dependent Variable" = 14))

# save
write_lines(tableA2, file = paste(dropbox, "tableA2.tex", sep = ""))
  
## Data 3
# make table using dataset 3
tbl <- map_dfc(depvar, maketable, dt = data3, fe = fe1)

# fix up some weird naming issues manually
tbl <- tbl %>% 
  mutate(name = c("Business combination", "law (BC)", "First-generation law", "", "Poison Pill law (PP)", 
                  "", "Control share", "acquisition law (CS)", "Directors' duties law", 
                  "(DD)", "Fair price law (FP)", "", "CS x CTS", "", "BC x Amanda", "", 
                  "BC x MF (motivating ", " firms)", "Observations", "R2"))

# save a latex table and export
tableA3 <- tbl %>% 
  kable(format = "latex", digits = 3, booktabs = T, align = 'l',
        label = "tableA3", caption = "Replication of DiD Results With Data (3)",
        col.names = linebreak(c("", rep(c("Short Regres- sion", "Full Model"), 7)), 
                              align = 'c'),
        linesep = "") %>% 
  kable_styling(latex_options = c("scale_down"),
                font_size = 6) %>% 
  column_spec(2:15, width = "1.1cm") %>% 
  column_spec(1, width = "2.75cm") %>% 
  row_spec(0, align = "c") %>% 
  add_header_above(c(" " = 1, "(1) ROA" = 2, "(2) Capex" = 2, "(3) PPE Growth" = 2,
                     "(4) Asset Growth" = 2, "(5) Cash" = 2, "(6) SGA Expense" = 2,
                     "(7) Leverage" = 2)) %>% 
  add_header_above(c(" " = 1, "Dependent Variable" = 14))

# save
write_lines(tableA3, file = paste(dropbox, "tableA3.tex", sep = ""))

## Data 4
# make table with dataset 4
tbl <- map_dfc(depvar, maketable, dt = data4, fe = fe2)

# fix up some weird naming issues manually
tbl <- tbl %>% 
  mutate(name = c("Business combination", "law (BC)", "First-generation law", "", "Poison Pill law (PP)", 
                  "", "Control share", "acquisition law (CS)", "Directors' duties law", 
                  "(DD)", "Fair price law (FP)", "", "CS x CTS", "", "BC x Amanda", "", 
                  "BC x MF (motivating ", " firms)", "Observations", "R2"))

# save a latex table and export
tableA4 <- tbl %>% 
  kable(format = "latex", digits = 3, booktabs = T, align = 'l',
        label = "tableA4", caption = "Replication of DiD Results With Data (4)",
        col.names = linebreak(c("", rep(c("Short Regres- sion", "Full Model"), 7)), 
                              align = 'c'),
        linesep = "") %>% 
  kable_styling(latex_options = c("scale_down"),
                font_size = 6) %>% 
  column_spec(2:15, width = "1.1cm") %>% 
  column_spec(1, width = "2.75cm") %>% 
  row_spec(0, align = "c") %>% 
  add_header_above(c(" " = 1, "(1) ROA" = 2, "(2) Capex" = 2, "(3) PPE Growth" = 2,
                     "(4) Asset Growth" = 2, "(5) Cash" = 2, "(6) SGA Expense" = 2,
                     "(7) Leverage" = 2)) %>% 
  add_header_above(c(" " = 1, "Dependent Variable" = 14))

# save
write_lines(tableA4, file = paste(dropbox, "tableA4.tex", sep = ""))
