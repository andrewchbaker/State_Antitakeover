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

# Connect to WRDS Server --------------------------------------------------
wrds <- dbConnect(Postgres(),
                  host = 'wrds-pgdata.wharton.upenn.edu',
                  port = 9737,
                  user = '',
                  password = '',
                  dbname = 'wrds',
                  sslmode = 'require')

# load the enactment dates
enactment_dates <- haven::read_dta(here::here("Data/KW", "Enactment Dates.dta"))

# Download Compustat data
comp <- tbl(wrds, sql("SELECT * FROM comp.funda")) %>%
  collect() %>% 
  # filter the data
  filter(indfmt == 'INDL' & datafmt == 'STD' & popsrc == 'D' & consol == 'C' & !is.na(fyear))

# download comp header which has more info on location etc
comp_header <- tbl(wrds, sql("SELECT * FROM comp.company")) %>%
  collect()

# merge in state of incorporation from most recent header file
comp <- comp %>% 
  left_join(comp_header %>% select(gvkey, incorp, sic, state), by = "gvkey")

# * Drop if not in US or missing state of incorporation data
comp <- comp %>% 
  filter(fic == "USA" & !is.na(incorp) & !(incorp %in% c("AS", "TT", "DC", "PR")))

# * Drop financial companies and utilities
comp <- comp %>% 
  filter(!(sic %>% between(6000, 6999)) & !(sic %>% between(4000, 4949)))

# * Generate numbers for firm, HQ, and incorp.
comp <- comp %>% 
  rename(year = fyear) %>% 
  mutate(gvkey = as.numeric(gvkey)) %>% 
  # six digit cusip
  mutate(cusip = str_sub(cusip, 1, 6),
         firm = group_indices(., gvkey),
         hq = group_indices(., state),
         incorporation = group_indices(., incorp),
         sic = as.character(sic)) %>% 
  rowwise() %>% 
  # make 3 digit industry code
  mutate(sic = ifelse(!is.na(sic), paste(rep("0", 4 - nchar(sic)), sic, sep = ""), NA_character_),
         industry = str_sub(sic, 1, 3)) %>% 
  ungroup()

# drop if industry == .
# drop if hq ==.
comp <- comp %>%
  filter(!is.na(sic) & !is.na(state))

# * Generate industry and state years
comp <- comp %>% 
  mutate(industry_year = group_indices(., industry, year),
         state_year = group_indices(., state, year))

# drop missing or negative assets and sales
comp <- comp %>% 
  filter(!is.na(at) & at >= 0) %>% 
  filter(!is.na(sale) & at >= 0)

# * Generate the number of years company has been in compustat for age control age = log(1+time in compustat)
comp <- comp %>% 
  group_by(gvkey) %>% 
  # get age as KW do it - add 2 years to difference between year and first year and take log.
  mutate(first_year = min(year),
         age = log(year - first_year + 2),
         age2 = age^2) %>% 
  ungroup()

# make a winsorize function
wins <- function(x, c1, c2) {
  # winsorize and return
  case_when(
    is.na(x) ~ NA_real_,
    x < quantile(x, c1, na.rm = TRUE) ~ quantile(x, c1, na.rm = TRUE),
    x > quantile(x, c2, na.rm = TRUE) ~ quantile(x, c2, na.rm = TRUE),
    TRUE ~ x
  )
}

# * Log of Total Assets (size)
comp <- comp %>% 
  mutate(size = log(at), 
         size2 = size^2)

# * PPE
comp <- comp %>% 
  mutate(ppe = ifelse(at > 0, ppent/at, NA_real_)) %>% 
  mutate(across(ppe, wins, c1 = 0.005, c2 = 0.995))

# * PPE growth
comp <- comp %>% 
  arrange(gvkey, year) %>% 
  group_by(gvkey) %>% 
  mutate(ppegrowth = if_else(!is.na(lag(ppe)) & lag(ppe) > 0, (ppe - lag(ppe))/lag(ppe), NA_real_)) %>% 
  ungroup() %>% 
  mutate(across(ppegrowth, wins, c1 = 0.005, 0.995))

# * Asset growth
comp <- comp %>% 
  group_by(gvkey) %>% 
  mutate(assetgrowth = if_else(!is.na(lag(at)) & lag(at) > 0, (at - lag(at))/lag(at), NA_real_)) %>% 
  ungroup() %>% 
  mutate(across(assetgrowth, wins, c1 = 0.005, c2 = 0.995))

# * Leverage Ratio
comp <- comp %>% 
  mutate(leverage = if_else(at > 0, (dltt + dlc)/at, NA_real_)) %>% 
  mutate(across(leverage, wins, c1 = 0.005, c2 = 0.995))

# * Capital Expenditure
comp <- comp %>% 
  mutate(capEx = if_else(at > 0, capx/at, NA_real_)) %>% 
  mutate(across(capEx, wins, c1 = 0.005, c2 = 0.995))

# * Selling expense
comp <- comp %>% 
  mutate(sga = if_else(at > 0, xsga/at, NA_real_)) %>% 
  mutate(across(sga, wins, c1 = 0.005, c2 = 0.995))

# * Return on Assets 
comp <- comp %>% 
  mutate(roa = if_else(at > 0, ebitda/at, NA_real_)) %>% 
  mutate(across(roa, wins, c1 = 0.005, c2 = 0.995)) %>% 
  group_by(gvkey) %>% 
  mutate(lagroa = lag(roa)) %>% 
  ungroup()

# * Cash
comp <- comp %>% 
  mutate(cash = if_else(at > 0, che / at, NA_real_)) %>% 
  mutate(across(cash, wins, c1 = 0.005, c2 = 0.995))

# Merge in enactment dates
comp <- comp %>% 
  left_join(enactment_dates, by = "incorp")

# * Generate law dummies
# * Tender Offer (Note we follow KW in all these processes)
edgar_date <- ymd(19820623)
comp <- comp %>% 
  mutate(gen1 = if_else((year >= year(to_date) & !is.na(to_date)) & 
                          (year < year(to_repeal) | is.na(to_repeal)) & 
                          year < year(edgar_date), 1, 0))

# * Business Combination
comp <- comp %>% 
  mutate(bc = if_else(year >= year(bc_date) & !is.na(bc_date), 1, 0))

# * Poison Pill
comp <- comp %>% 
  mutate(pp = if_else((year >= year(pp_date) & !is.na(pp_date)) | 
                        (year >= 1985 & incorp == "DE"), 1, 0))

# * Fair Price
comp <- comp %>% 
  mutate(fp = if_else(year >= year(fp_date) & !is.na(fp_date), 1, 0))

# * Director's Duties
comp <- comp %>% 
  mutate(dd = if_else(year >= year(dd_date) & !is.na(dd_date), 1, 0))

# * Control Share Acquisition
# gen cs = 0
comp <- comp %>% 
  mutate(cs = if_else(year >= year(cs_date) & !is.na(cs_date), 1, 0))

# *************************************************************************************************************************
#   * Legal cases
# *************************************************************************************************************************
comp <- comp %>% 
  mutate(cts = if_else(year >= 1987, 1, 0),
         amanda = if_else(year >= 1989, 1, 0),
         bcXamanda = amanda*bc,
         csXcts = cts*cs)

# *************************************************************************************************************************
#   * Merge Optouts
# *************************************************************************************************************************
# download in legacy and new governance database from ISS
governance_legacy <- tbl(wrds, sql("SELECT * FROM risk.gset")) %>% 
  collect()

# the old dataset
governance <- tbl(wrds, sql("SELECT * FROM risk.rmgovernance")) %>% 
  collect()

# grab variables and clean 
# legacy data
governance_legacy <- governance_legacy %>% 
  select(year, cusip = cn6, coname, oo_bc = oo_buscomp, 
         oo_cs = oo_csa, oo_fp = oo_fairprice, oo_dd = oo_duties)

# new data
governance <- governance %>% 
  filter(year >= 2008) %>% 
  select(year, cusip, coname, oo_bc = oo_buscomp, oo_cs = oo_csa, oo_fp = oo_fairprice, 
         oo_pp = oo_pp, oo_dd = oo_duties) %>% 
  # six digit CUSIP
  mutate(cusip = str_sub(cusip, 1, 6))

# function to clean up and variables and turn binary in governance dataset
change_weird_stuff <- function(x) {
  x = case_when(
    x == "NO" ~ 0,
    x == "YES" ~ 1,
    TRUE ~ NA_real_
  )
}

# run across variables
governance <- governance %>% 
  mutate(across(starts_with("oo_"), ~replace_na(., 0))) %>% 
  mutate(across(starts_with("oo_"), ~change_weird_stuff(.)))

# get a list of firms that have any opt outs in either the legacy or governance datasets
# first combine the data
combined_data <- bind_rows(governance_legacy, governance) %>% 
  mutate(across(starts_with("oo_"), ~replace_na(., 0)))

# get list of firms
firms <- combined_data %>% 
  group_by(cusip) %>% 
  filter(sum(oo_bc) + sum(oo_cs) + sum(oo_fp) + sum(oo_dd) + sum(oo_pp) > 0) %>% 
  pull(cusip) %>% 
  unique() %>% 
  sort()

# make dataset
newoptout <- expand_grid(cusip = firms, year = 1990:2010) %>% 
  left_join(combined_data %>% select(cusip, year, coname)) %>% 
  # pull names down then up
  group_by(cusip) %>% 
  fill(coname, .direction = "downup") %>%
  ungroup() %>% 
  # add in the optout data
  left_join(combined_data %>% select(cusip, year, starts_with("oo_")))

# function to fill down the observations
# the ISS data only cocmes out for certain years and the literature essentially 
# fills down the observations
fill_down <- function(x, year) {
  case_when(
    year %in% 1991:1992 ~ x[which(year == 1990)],
    year == 1994 ~ x[which(year == 1993)],
    year %in% 1996:1997 ~ x[which(year == 1995)],
    year == 1999 ~ x[which(year == 1998)],
    year == 2001 ~ x[which(year == 2000)],
    year == 2003 ~ x[which(year == 2002)],
    year == 2005 ~ x[which(year == 2004)],
    year == 2007 ~ x[which(year == 2006)],
    TRUE ~ x
  )
}

# fill down the new opt out
newoptout <- newoptout %>% 
  group_by(cusip) %>% 
  mutate(across(starts_with("oo_"), ~fill_down(., year = year)))

# merge in the data
comp <- comp %>% 
  left_join(newoptout, by = c("cusip", "year"))

# replace opt outs
comp <- comp %>% 
  mutate(bc = if_else(oo_bc == 1 & !is.na(bc) & !is.na(oo_bc), 0, bc),
         cs = if_else(oo_cs == 1 & !is.na(cs) & !is.na(oo_cs), 0, cs),
         pp = if_else(oo_pp == 1 & !is.na(pp) & !is.na(oo_pp), 0, pp),
         dd = if_else(oo_dd == 1 & !is.na(dd) & !is.na(oo_dd), 0, dd),
         fp = if_else(oo_fp == 1 & !is.na(fp) & !is.na(oo_fp), 0, fp))

# *************************************************************************************************************************
#   *Opt-in Laws
# *************************************************************************************************************************
comp <- comp %>% 
  mutate(bc = if_else(incorp == "GA" & bc == 1, 0, bc),
         fp = if_else(incorp == "GA" & fp == 1, 0, fp),
         cs = if_else(incorp == "TN" & cs == 1, 0, cs))
# 
# *************************************************************************************************************************
#   * Motivating firms
# *************************************************************************************************************************
comp <- comp %>% 
  mutate(motivatingfirmbc = 0, motivatingfirmcs = 0, motivatingfirmall = 0,
         motivatingfirmpp = 0, motivatingfirmdd = 0, motivatingfirmfp = 0)

# function to replace motivating firms with 1 in the indicator variable.
# note this could be done much more seamlessly but I copied the Stata code and too lazy to replace 
# now
replacefun <- function(x, cp) {
  if_else(comp$cusip == cp, 1, x)
}

# * Greyhound CS BC FP DD cusip 398048 
comp <- comp %>% 
  mutate(across(c(motivatingfirmbc, motivatingfirmcs, motivatingfirmdd, motivatingfirmfp, motivatingfirmall),
                replacefun, cp = "398048"))

# * KN Energy PP 49455P
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmpp),
                replacefun, cp = "49455P"))

# * Singer BC FP 82930F
comp <- comp %>% 
  mutate(across(c(motivatingfirmbc, motivatingfirmpp, motivatingfirmall),
                replacefun, cp = "82930F"))

# * Aetna FP 811710
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmfp),
                replacefun, cp = "811710"))

# * Texaco BC 881694
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmbc),
                replacefun, cp = "881694"))

# * Harcourt Brace Jovanovich CS FP 411631
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmcs, motivatingfirmfp),
                replacefun, cp = "411631"))

# * Ashland Oil BC FP 445401
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmbc, motivatingfirmfp),
                replacefun, cp = "445401"))

# * Amfac CS 031141
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmcs),
                replacefun, cp = "031141"))

# * Abott Labs BC FP
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmfp, motivatingfirmbc),
                replacefun, cp = "002824"))

# * Sears BC FP 812370
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmfp, motivatingfirmbc),
                replacefun, cp = "812370"))

# * Roebuck BC FP 812387
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmfp, motivatingfirmbc),
                replacefun, cp = "812387"))

# * Walgreens BC FP 931422
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmfp, motivatingfirmbc),
                replacefun, cp = "931422"))

# * Arvin Industries CS BC FP PP 043339
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmfp, motivatingfirmbc,
                  motivatingfirmpp, motivatingfirmcs),
                replacefun, cp = "043339"))

# * Cummins Engine DD 231021
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmdd),
                replacefun, cp = "231021"))

# * United Telecommunications BC 852061
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmbc),
                replacefun, cp = "852061"))

# * Centel BC 151334
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmbc),
                replacefun, cp = "151334"))

# * Coleman BC 193559
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmbc),
                replacefun, cp = "193559"))

# * Martin Marietta BC CS 572900
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmbc, motivatingfirmcs),
                replacefun, cp = "572900"))

# * McCormick BC CS FP 579780
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmbc, motivatingfirmcs, motivatingfirmfp),
                replacefun, cp = "579780"))

# * PHH Group BC CS FP 693320
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmbc, motivatingfirmcs, motivatingfirmfp),
                replacefun, cp = "693320"))

# * Foremost-McKesson FP 581556
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmfp),
                replacefun, cp = "581556"))

# * Gillette CS DD PP 375766
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmcs, motivatingfirmdd,
                  motivatingfirmpp),
                replacefun, cp = "375766"))

# * Stop & Shop DD PP 862097
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmdd, motivatingfirmpp),
                replacefun, cp = "862099"))

# * Polaroid DD PP 731095
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmdd, motivatingfirmpp),
                replacefun, cp = "731095"))

# * Prime Computer DD PP 741555
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmdd, motivatingfirmpp),
                replacefun, cp = "741555"))

# * Dayton Hudson BC CS DD 87612E
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmdd, motivatingfirmbc, motivatingfirmcs),
                replacefun, cp = "87612E"))

# * TWA CS 893349
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmcs),
                replacefun, cp = "893349"))

# * Schering-Plough BC FP 806605
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmbc),
                replacefun, cp = "806605"))

# * CBS BC FP 124845
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmbc, motivatingfirmfp),
                replacefun, cp = "124845"))

# * Champion International BC FP PP 158525 HUGE BC effect
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmbc, motivatingfirmfp, motivatingfirmpp),
                replacefun, cp = "158525"))

# * GE BC FP 369604
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmbc, motivatingfirmfp),
                replacefun, cp = "369604"))

# * Ogilvy Group PP 676601
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmpp),
                replacefun, cp = "676601"))

# * Avon PP 054303
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmpp),
                replacefun, cp = "054303"))

# * International Paper PP 460146
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmpp),
                replacefun, cp = "460146"))

# * Xerox PP 984121
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmpp),
                replacefun, cp = "984121"))

# * Burlington CS 121691
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmcs),
                replacefun, cp = "121691"))

# * PepsiCo FP 713448
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmfp),
                replacefun, cp = "713448"))

# * Goodyear PP 382550
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmpp),
                replacefun, cp = "382550"))

# * Mellon Bank BC FP 58551A
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmfp, motivatingfirmbc),
                replacefun, cp = "58551A"))

# * PPG BC FP 693506 Huge BC effect
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmfp, motivatingfirmbc),
                replacefun, cp = "693506"))

# * Westinghouse BC FP 12490K
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmfp, motivatingfirmbc),
                replacefun, cp = "12490K"))

# * Amrstrong World Industries CS DD 04247X
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmcs, motivatingfirmdd),
                replacefun, cp = "04247X"))

# * Boeing BC FP 0987023
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmfp, motivatingfirmbc),
                replacefun, cp = "097023"))

# * G. Heileman Brewing BC PP 422884
comp <- comp %>% 
  mutate(across(c(motivatingfirmall, motivatingfirmpp, motivatingfirmbc),
                replacefun, cp = "422884"))

# interact the motivating firms with when the law is in place
comp <- comp %>% 
  mutate(bcXmotivatingfirmall = bc*motivatingfirmall,
         bcXmotivatingfirmbc = bc*motivatingfirmbc,
         ppXmotivatingfirmpp = pp*motivatingfirmpp,
         csXmotivatingfirmcs = cs*motivatingfirmcs,
         fpXmotivatingfirmfp = fp*motivatingfirmfp,
         ddXmotivatingfirmdd = dd*motivatingfirmdd)

# save the data
saveRDS(comp, here::here("Data/COMPILED", "data2.rds"))