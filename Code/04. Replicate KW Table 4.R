library(tidyverse)
library(RPostgres)
library(lubridate)
library(lfe)
library(kableExtra)
library(lubridate)
library(fixest)
library(data.table)
library(dataverse)
library(RCurl)
library(tictoc)
options(knitr.kable.NA = '')
dropbox <- ""

# load the cleaned KW data
KW <- haven::read_dta(here::here("Data/KW", "maindata.dta")) %>% 
  # filter years to between 76 and 95
  filter(year %>% between(1976, 1995))

# dataset to merge in column names 
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
  "age2", "Age Squared")

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

# function to run models based on dep variable and covariate set
runmod <- function(depvar, covs) {
  feols(as.formula(paste(c(depvar, " ~ ", paste(covs, collapse = " + "),
                           "| firm + state_year + industry_year"), collapse = "")),
        data = KW, cluster = "incorporation")
}

# function to run model by dep var
maketable <- function(depvar) {
  
  # estimate the models with both covariate sets
  mod1 <- runmod(depvar, cov1)
  mod2 <- runmod(depvar, cov2)
  
  # combine the data estimates together and report the table
  bind_rows(
    broom::tidy(mod1, se = "cluster", cluster = KW %>% pull(incorporation)) %>% mutate(mod = "mod1"),
    broom::tidy(mod2, se = "cluster", cluster = KW %>% pull(incorporation)) %>% mutate(mod = "mod2")) %>% 
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
    # drop the names for the second row with standard errors
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

# put dependent variables in a vector and make table
depvar <- c("roa", "capEx", "ppegrowth", "assetgrowth", "cash", "sga", "leverage")

# make table by vectorizing over variable
tbl <- map_dfc(depvar, maketable)

# fix up some weird naming issues manually
tbl <- tbl %>% 
  mutate(name = c("Business combination", "law (BC)", "First-generation law", "", "Poison Pill law (PP)", 
                  "", "Control share", "acquisition law (CS)", "Directors' duties law", 
                  "(DD)", "Fair price law (FP)", "", "CS x CTS", "", "BC x Amanda", "", 
                  "BC x MF (motivating ", " firms)", "Observations", "R2"))

# save a latex table and export
table1 <- tbl %>% 
  kable(format = "latex", digits = 3, booktabs = T, align = 'l',
        label = "table1", caption = "Replication of Table IV from Karpoff-Wittry (2018)",
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
write_lines(table1, file = paste(dropbox, "table1.tex", sep = ""))