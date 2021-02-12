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

# Load or Download Data ---------------------------------------------------
# load McDonald data, downloaded from website https://sraf.nd.edu/data/augmented-10-x-header-data/
mcd <- read_csv(here::here("Data/MCDONALD", "LM_EDGAR_10X_Header_1994_2018.csv"))

## Download the HS data from dataversee
# load the main data
Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")
dt <- get_file("state_inc.tab", "doi:10.7910/DVN/KBPZ5V")
tmp <- tempfile(fileext = ".csv")
writeBin(as.vector(dt), tmp)
hs <- read_csv(tmp)

# load the crosswalk
dt <- get_file("CIK_CUSIP_crosswalk.tab", "doi:10.7910/DVN/KBPZ5V")
tmp <- tempfile(fileext = ".dta")
writeBin(as.vector(dt), tmp)
crosswalk <- haven::read_dta(tmp)

# load law enactment dates
enactment_dates <- haven::read_dta(here::here("Data/KW", "Enactment Dates.dta"))

# Download Compustat data
comp <- tbl(wrds, sql("SELECT * FROM comp.funda")) %>%
  # filter as per usual
  filter(indfmt == 'INDL' & datafmt == 'STD' & popsrc == 'D' & consol == 'C' & !is.na(fyear)) %>% 
  # sort by firm year
  arrange(gvkey, fyear) %>% 
  collect() %>%
  # drop missing or negative values of assets or sales
  filter(!is.na(at) & at >= 0) %>% 
  filter(!is.na(sale) & sale >= 0)

# set everyone's cik to numeric 
mcd <- mcd %>% mutate(cik = as.numeric(cik))
comp <- comp %>% mutate(cik = as.numeric(cik))
hs <- hs %>% mutate(cik = as.numeric(cik))

# Resolve Changing CIKs ---------------------------------------------------

# first, resolve changes in CIK as this is used for the merging variable for MCD and HS
# use the CIK within the begdate enddate window. But also set the first begdate to be 1900 so that 
# we use the most recently available estimate from the historical header file
ciks <- tbl(wrds, sql("SELECT gvkey, begdate, enddate, cik FROM wrds_cs_names")) %>% 
  collect() %>% 
  group_by(gvkey) %>% 
  mutate(begdate = if_else(begdate == min(begdate), ymd(19000101), begdate),
         cik = as.numeric(cik)) %>% 
  fill(cik, .direction = "up") %>% 
  ungroup()

# merge data into compustat
# first just merge in historical header information to firm/year obs
ciks_merge <- comp %>% 
  # get the firm, year, reporting period data from compustat
  select(gvkey, fyear, datadate) %>% 
  # bring in the ciks info
  left_join(ciks, by = "gvkey") %>% 
  # keep hits within begdate / enddate
  filter(datadate >= begdate & datadate <= enddate) %>% 
  select(gvkey, fyear, datadate, h_header_cik = cik)

# merge this file into the main compustat file to complete
comp <- comp %>% 
  left_join(ciks_merge, by = c("gvkey", "fyear", "datadate")) %>% 
  # make one cik variable which is the first non-missing obs 
  # of the cik from the historical header file or the cik from the 
  # as of today header file
  rowwise() %>% 
  mutate(cik = coalesce(h_header_cik, cik)) %>% 
  ungroup() %>% 
  select(-h_header_cik)

# Merge in information from MCD and HS ------------------------------------
# in McDonlad data, get one observation per cik/reporting period
mcd <- mcd %>% 
  # keep only annual filings
  filter(str_detect(f_ftype, "10-K")) %>% 
  # subset the variables we want
  select(cik, datadate_mcd = conf_per_rpt, mcd_incorp = state_of_incorp, 
         mcd_state = ba_state, mcd_sic = sic_num, file_date = f_fdate) %>% 
  # set incorp or state to missing if not in a state
  mutate(mcd_incorp = if_else(mcd_incorp %in% state.abb, mcd_incorp, NA_character_),
         mcd_state = if_else(mcd_incorp %in% state.abb, mcd_state, NA_character_)) %>% 
  # reformat
  mutate(datadate_mcd = ymd(datadate_mcd), 
         file_date = ymd(file_date)) %>% 
  # drop duplicates by firm/reporting date
  group_by(cik, datadate_mcd) %>% 
  # keep the last filing date
  filter(file_date == max(file_date)) %>% 
  distinct() %>%
  ungroup() %>% 
  # drop filing date for merge
  select(-file_date) %>% 
  setDT()

# first merge in the mcd data - give one week on either side
# get the identifying info from compustat to merge into
comp_base <- comp %>% 
  select(gvkey, cik, cusip, fyear, datadate) %>% 
  setDT()

# merge in mcd data to the basic frame of compustat
comp_base_mcd <- merge(comp_base, mcd, by = "cik", allow.cartesian = TRUE) %>%
  # keep just obs within one week of filing date
  .[datadate >= datadate_mcd - weeks(1) & datadate <= datadate_mcd + weeks(1)] %>% 
  # if still multiple, keep the one closest to the datadate
  .[, .SD[abs(datadate - datadate_mcd) == min(abs(datadate - datadate_mcd))], keyby = .(gvkey, fyear)] %>% 
  as_tibble() %>% 
  select(gvkey, fyear, mcd_incorp, mcd_state, mcd_sic) %>% 
  distinct()

# merge back in
comp <- comp %>% 
  left_join(comp_base_mcd, by = c("gvkey", "fyear"))

## Now do the HS data - this only has state of incorporation information 
# first merge in hs by cik
hs_merge <- hs %>% 
  # rename datadate variable so we can allow a week before and after
  rename(datadate_hs = filing_period) %>% 
  # drop missing cik or datadates
  filter(!is.na(cik) & !is.na(datadate_hs)) %>% 
  select(cik, hs_incorp = state, datadate_hs) %>% 
  distinct() %>% 
  setDT()

# merge in - keep within one week on either side
comp_base_hs <- merge(comp_base, hs_merge, by = "cik", allow.cartesian = TRUE) %>%
  # keep just obs within one week of filing date
  .[datadate >= datadate_hs - weeks(1) & datadate <= datadate_hs + weeks(1)] %>% 
  # if still multiple, keep the one closest to the datadate
  .[, .SD[abs(datadate - datadate_hs) == min(abs(datadate - datadate_hs))], keyby = .(gvkey, fyear)] %>% 
  as_tibble() %>% 
  select(gvkey, fyear, hs_incorp) %>% 
  distinct()

# merge back in to compustat
comp <- comp %>% 
  left_join(comp_base_hs, by = c("gvkey", "fyear"))

# bring in the crosswalk for the observations without a cik so we can merge on cusip instead
hs_merge <- hs %>% 
  # join
  left_join(crosswalk, by = "cik") %>%
  # drop missing cusips
  filter(!is.na(cusip)) %>% 
  select(cusip, datadate_hs = filing_period, hs_incorp_c = state) %>% 
  # make cusip 9 digits bc this is annoying
  rowwise() %>% 
  mutate(cusip = if_else(nchar(cusip) < 9, 
                         paste0(as.character(paste0(rep(0, 9-nchar(cusip)), collapse = "")), cusip), cusip)) %>% 
  ungroup()

# merge in based on cusip 
# merge in - keep within one week on either side
comp_base_hs <- merge(comp_base, hs_merge, by = "cusip", allow.cartesian = TRUE) %>%
  # keep just obs within one week of filing date
  .[datadate >= datadate_hs - weeks(1) & datadate <= datadate_hs + weeks(1)] %>% 
  # if still multiple, keep the one closest to the datadate
  .[, .SD[abs(datadate - datadate_hs) == min(abs(datadate - datadate_hs))], keyby = .(gvkey, fyear)] %>% 
  as_tibble() %>% 
  select(gvkey, fyear, hs_incorp_c) %>% 
  distinct()

# merge in - and make one hs_incorp variable
comp <- comp %>% 
  left_join(comp_base_hs, by = c("gvkey", "fyear")) %>% 
  mutate(hs_incorp = if_else(is.na(hs_incorp), hs_incorp_c, hs_incorp)) %>% 
  select(-hs_incorp_c)

# Download legacy header information for all of the variables
hist_header <- tbl(wrds, sql("SELECT gvkey, begdate, enddate, fic, incorp, state, sic, cik, conm, cusip FROM wrds_cs_names")) %>% 
  collect() %>% 
  group_by(gvkey) %>% 
  ungroup()

# merge data into compustat
# first just merge in historical header information
hist_header_info <- comp %>% 
  select(gvkey, fyear, datadate) %>% 
  left_join(hist_header, by = "gvkey") %>% 
  filter(datadate >= begdate & datadate <= enddate) %>% 
  select(gvkey, fyear, datadate, h_header_fic = fic, h_header_incorp = incorp, 
         h_header_state = state, h_header_sic = sic, h_header_cik = cik, h_header_cusip = cusip)

# merge back into compustat
comp <- comp %>% 
  left_join(hist_header_info, by = c("gvkey", "fyear", "datadate"))

# finally add the header incorp data from now
header <- tbl(wrds, sql("SELECT * FROM comp.company")) %>%
  select(gvkey, header_incorp = incorp, header_sic = sic, header_state = state, header_fic = fic, header_cik = cik) %>% 
  collect()

# merge in
comp <- comp %>% left_join(header, by = "gvkey")

# Fix State of Incorporation ----------------------------------------------
# first make an incorporation variable which is the fist nonmissing obs from MCD, HS, and the
# historical header information
comp <- comp %>% 
  # make a new variable which is first non missing obs in MCD, HS, historical header info
  mutate(incorp = coalesce(mcd_incorp, hs_incorp, h_header_incorp))

# next add in variables for the most recent non-missing observation of incorporation 
# and the date for each data source besides header
# first mcd - drop missing
mcd_most_recent <- mcd %>% 
  .[!is.na(mcd_incorp) & !is.na(cik) & !is.na(datadate_mcd)]

# merge into comp base dataset - just identifying information
mcd_most_recent <- merge(comp_base, mcd_most_recent, by = "cik", allow.cartesian = TRUE) %>%
  # keep just most recent obs
  .[datadate <= datadate_mcd] %>% 
  # if still multiple, keep the one closest to the datadate
  .[, .SD[datadate - datadate_mcd == max(datadate - datadate_mcd)], keyby = .(gvkey, fyear)] %>% 
  as_tibble() %>% 
  select(gvkey, fyear, mr_mcd_incorp = mcd_incorp, mr_mcd_state = mcd_state, mr_mcd_sic = mcd_sic, datadate_mcd) %>% 
  distinct()

# merge in
comp <- comp %>% left_join(mcd_most_recent, by = c("gvkey", "fyear"))

# Now do the same thing for HS data
hs_most_recent <- hs %>% 
  # keep just the variables we need
  select(cik, state, datadate_hs = filing_period) %>% 
  # drop missing observations on any dimension bc not useful
  drop_na() %>% 
  setDT()

# merge into comp base dataset - just identifying information
hs_most_recent <- merge(comp_base, hs_most_recent, by = "cik", allow.cartesian = TRUE) %>%
  # keep just most recent obs
  .[datadate <= datadate_hs] %>% 
  # if still multiple, keep the one closest to the datadate
  .[, .SD[datadate - datadate_hs == max(datadate - datadate_hs)], keyby = .(gvkey, fyear)] %>% 
  as_tibble() %>% 
  select(gvkey, fyear, mr_hs_incorp = state, datadate_hs) %>% 
  distinct()

# merge in
comp <- comp %>% left_join(hs_most_recent, by = c("gvkey", "fyear"))

### Finally do the same thing with the historical header info
hist_header_most_recent <- hist_header %>% 
  filter(!is.na(incorp)) %>% 
  select(gvkey, datadate_h_hist = begdate, mr_h_hist_incorp = incorp,
         mr_h_hist_fic = fic, mr_h_hist_state = state, mr_h_hist_sic = sic,
         mr_h_hist_cusip = cusip) %>% 
  setDT()

# merge into comp base dataset - just identifying information
hist_header_most_recent <- merge(comp_base[, .(gvkey, fyear, datadate)], 
                                 hist_header_most_recent, by = "gvkey", allow.cartesian = TRUE) %>%
  # keep just most recent obs
  .[datadate <= datadate_h_hist] %>% 
  # if still multiple, keep the one closest to the datadate
  .[, .SD[datadate - datadate_h_hist == max(datadate - datadate_h_hist)], keyby = .(gvkey, fyear)] %>% 
  as_tibble() %>% 
  distinct()

# merge in
comp <- comp %>% left_join(hist_header_most_recent, by = c("gvkey", "fyear", "datadate"))

### Finally, make one measure of incorporation
# 1) If a nonmissing entry in MCD, HS, Historical Header info - use that (preference for MCD, HS, then Hist Header)
# 2) If still missing use most recent version of MCD, HS, hist header - in that order
# 3) If still missing use the most recent header file info

comp <- comp %>% 
  mutate(incorp = case_when(
    # if nonmissing entry in mcd, hs, historical then use
    !is.na(incorp) ~ incorp,
    # if the next most recent nonmissing obs in mcd then use
    !is.na(datadate_mcd) & (datadate_mcd <= datadate_hs | is.na(datadate_hs)) & 
      (datadate_mcd <= datadate_h_hist | is.na(datadate_h_hist)) ~ mr_mcd_incorp,
    # then move to HS
    !is.na(datadate_hs) & (datadate_hs <= datadate_h_hist | is.na(datadate_h_hist)) ~ mr_hs_incorp,
    # finally use the next most recent historically incorproated 
    !is.na(mr_h_hist_incorp) ~ mr_h_hist_incorp,
    # else use header
    TRUE ~ header_incorp
  ))

# Fix Headquarters State --------------------------------------------------
# do the same thing as above but for headquarter state. Note we don't have HS state of headquarters data
comp <- comp %>% 
  # first nonmissing gin MCD or header
  mutate(state = coalesce(mcd_state, h_header_state)) %>% 
  mutate(state = case_when(
    # if nonmissing entry in mcd, hs, historical then use
    !is.na(state) ~ state,
    # then move to MCD historical datadate
    !is.na(datadate_mcd) & (datadate_mcd <= datadate_h_hist | is.na(datadate_h_hist)) ~ mr_mcd_state,
    # finally use the next most recent historically incorproated 
    !is.na(mr_h_hist_state) ~ mr_h_hist_state,
    # else use header
    TRUE ~ header_state
  ))

# Do FIC ------------------------------------------------------------------
comp <- comp %>% 
  mutate(fic = h_header_fic) %>% 
  mutate(fic = case_when(
    # if nonmissing entry in mcd, hs, historical then use
    !is.na(fic) ~ fic,
    # finally use the next most recent historically incorproated 
    !is.na(mr_h_hist_fic) ~ mr_h_hist_fic,
    # else use header
    TRUE ~ header_fic
  ))

# Do CUSIP ------------------------------------------------------------------
comp <- comp %>% 
  mutate(header_cusip = cusip,
         cusip = h_header_cusip) %>% 
  mutate(cusip = case_when(
    # if nonmissing entry in mcd, hs, historical then use
    !is.na(cusip) ~ cusip,
    # finally use the next most recent historically incorproated 
    !is.na(mr_h_hist_cusip) ~ mr_h_hist_cusip,
    # else use header
    TRUE ~ header_cusip
  ))

# Do SIC ------------------------------------------------------------------
comp <- comp %>%
  # put historical sic code (sich) into four digit string
  mutate(sich = as.character(sich)) %>% 
  rowwise() %>% 
  mutate(sich = ifelse(!is.na(sich),
                       paste(rep("0", 4 - nchar(sich)), sich, sep = ""), NA)) %>% 
  ungroup() %>% 
  # save an old unfilled version of sich, then backfill sich using "downup". If empty until a 
  # certain year, use the first year. If empty after an sich, then fill *down* until the next sich.
  # if all missing use the SIC code from the compustat name file
  mutate(sich_old = sich) %>% 
  arrange(gvkey, fyear) %>% 
  group_by(gvkey) %>% 
  fill(sich, .direction = "downup") %>% 
  mutate(
    # get the last year of sich if not all missing
    lastyear = if_else(length(which(!is.na(sich))) > 0, max(fyear[which(!is.na(sich_old))]), NA_real_),
    # make final sich - if after last sich use sic, or if all missing use sic, otherwise our spliced series
    sich = if_else(is.na(sich) | fyear > lastyear, header_sic, sich)) %>% 
  ungroup() %>% 
  # make three digit, two digit, and one digit sic and 8 digit cusip
  mutate(sic_3 = str_sub(sich, 1, 3),
         sic_2 = str_sub(sich, 1, 2),
         sic_1 = str_sub(sich, 1, 1))

# Do rest of cleaning as per KW -------------------------------------------
# * Drop if not in US or missing state of incorporation data
comp <- comp %>% 
  filter(fic == "USA" & !is.na(incorp) & !(incorp %in% c("AS", "TT", "DC", "PR")))

# * Drop financial companies and utilities (BUT USE HISTORICAL SIC CODE)
comp <- comp %>% 
  filter(!(sich %>% between(6000, 6999)) & !(sich %>% between(4000, 4949)))

# * Generate numbers for firm, HQ, and incorp.
comp <- comp %>% 
  rename(year = fyear) %>% 
  mutate(gvkey = as.numeric(gvkey)) %>% 
  mutate(cusip = str_sub(cusip, 1, 6),
         firm = group_indices(., gvkey),
         hq = group_indices(., state),
         incorporation = group_indices(., incorp)) %>% 
  rowwise() %>% 
  mutate(industry = str_sub(sich, 1, 3)) %>% 
  ungroup()

# Drop missing industry or headquarter state observations
comp <- comp %>%
  filter(!is.na(sich) & !is.na(state))

# Generate industry and state years
comp <- comp %>% 
  mutate(industry_year = group_indices(., industry, year),
         state_year = group_indices(., state, year))

# drop missing assets and sales - fix the knit from KW
comp <- comp %>% 
  filter(!is.na(at) & at >= 0) %>% 
  filter(!is.na(sale) & sale >= 0)

# * Generate the number of years company has been in compustat for age control age = log(1+time in compustat)
#  they add two years, I'm only adding one
comp <- comp %>% 
  group_by(gvkey) %>% 
  mutate(first_year = min(year),
         age = log(year - first_year + 1),
         age2 = age^2) %>% 
  ungroup()

# make a winsorize function - when I winsorize I do it by year
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
  group_by(year) %>% 
  mutate(across(ppe, wins, c1 = 0.005, c2 = 0.995)) %>% 
  ungroup()

# * PPE growth
comp <- comp %>% 
  arrange(gvkey, year) %>% 
  group_by(gvkey) %>% 
  mutate(ppegrowth = if_else(!is.na(lag(ppe)) & lag(ppe) > 0, (ppe - lag(ppe))/lag(ppe), NA_real_)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(across(ppegrowth, wins, c1 = 0.005, 0.995)) %>% 
  ungroup()

# * Asset growth
comp <- comp %>% 
  group_by(gvkey) %>% 
  mutate(assetgrowth = if_else(!is.na(lag(at)) & lag(at) > 0 & year - lag(year) == 1, (at - lag(at))/lag(at), NA_real_)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(across(assetgrowth, wins, c1 = 0.005, c2 = 0.995)) %>% 
  ungroup()

# * Leverage Ratio
comp <- comp %>% 
  mutate(leverage = if_else(at > 0, (dltt + dlc)/at, NA_real_)) %>% 
  group_by(year) %>% 
  mutate(across(leverage, wins, c1 = 0.005, c2 = 0.995)) %>% 
  ungroup()

# * Capital Expenditure
comp <- comp %>% 
  mutate(capEx = if_else(at > 0, capx/at, NA_real_)) %>% 
  group_by(year) %>% 
  mutate(across(capEx, wins, c1 = 0.005, c2 = 0.995)) %>% 
  ungroup()

# * Selling expense
comp <- comp %>% 
  mutate(sga = if_else(at > 0, xsga/at, NA_real_)) %>% 
  group_by(year) %>% 
  mutate(across(sga, wins, c1 = 0.005, c2 = 0.995)) %>% 
  ungroup()

# * Return on Assets 
comp <- comp %>% 
  mutate(roa = if_else(at > 0, ebitda/at, NA_real_)) %>% 
  group_by(year) %>% 
  mutate(across(roa, wins, c1 = 0.005, c2 = 0.995)) %>% 
  ungroup() %>% 
  group_by(gvkey) %>% 
  mutate(lagroa = lag(roa)) %>% 
  ungroup()

# * Cash
comp <- comp %>% 
  mutate(cash = if_else(at > 0, che / at, NA_real_)) %>% 
  group_by(year) %>% 
  mutate(across(cash, wins, c1 = 0.005, c2 = 0.995)) %>% 
  ungroup()

# merge m:m incorp using "Enactment Dates"
comp <- comp %>% 
  left_join(enactment_dates, by = "incorp")

# * Generate law dummies
# * Tender Offer
edgar_date <- ymd(19820623)
comp <- comp %>% 
  mutate(gen1 = if_else((datadate >= to_date & !is.na(to_date)) & 
                          (datadate < to_repeal | is.na(to_repeal)) & 
                          datadate < edgar_date, 1, 0))

# * Business Combination
comp <- comp %>% 
  mutate(bc = if_else(datadate >= bc_date & !is.na(bc_date), 1, 0))

# * Poison Pill
comp <- comp %>% 
  mutate(pp = if_else((datadate >= pp_date & !is.na(pp_date)) | 
                        (datadate >= ymd(19851119) & incorp == "DE"), 1, 0))

# ***** Change PP date for DE for lead lags
comp <- comp %>% 
  mutate(pp_date = if_else(incorp == "DE", ymd(19851119), pp_date))

# * Fair Price
comp <- comp %>% 
  mutate(fp = if_else(datadate >= fp_date & !is.na(fp_date), 1, 0))

# * Director's Duties
comp <- comp %>% 
  mutate(dd = if_else(datadate >= dd_date & !is.na(dd_date), 1, 0))

# * Control Share Acquisition
# * Repeal
comp <- comp %>% 
  mutate(cs = if_else(datadate >= cs_date & !is.na(cs_date), 1, 0)) %>% 
  mutate(cs = if_else(incorp == "WI" & datadate >= ymd(19860422), 0, cs))

# *************************************************************************************************************************
#   * Legal cases
# *************************************************************************************************************************
comp <- comp %>% 
  mutate(cts = if_else(datadate >= ymd(19870421), 1, 0),
         amanda = if_else(datadate >= ymd(19890524), 1, 0),
         bcXamanda = amanda*bc,
         csXcts = cts*cs)

# *************************************************************************************************************************
#   * Merge Optouts
# *************************************************************************************************************************
# first save a version of these without optouts
comp <- comp %>% 
  mutate(bc_nooptout = bc, cs_nooptout = cs, pp_nooptout = pp,
         dd_nooptout = dd, fp_nooptout = fp)

# download in legacy and new governance database from ISS
governance_legacy <- tbl(wrds, sql("SELECT * FROM risk.gset")) %>% 
  collect()

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
  mutate(cusip = str_sub(cusip, 1, 6))

# function to clean up and variables and turn binary in governance dataset
change_weird_stuff <- function(x) {
  x = case_when(
    x == "NO" ~ 0,
    x == "YES" | x %in% as.character(3:5) ~ 1,
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

# merge in
comp <- comp %>% 
  left_join(newoptout, by = c("cusip", "year"))

# swap out values for the dummy indicators when there is an opt out
comp <- comp %>% 
  mutate(bc = if_else(oo_bc == 1 & !is.na(bc) & !is.na(oo_bc), 0, bc),
         cs = if_else(oo_cs == 1 & !is.na(cs) & !is.na(oo_cs), 0, cs),
         pp = if_else(oo_pp == 1 & !is.na(pp) & !is.na(oo_pp), 0, pp),
         dd = if_else(oo_dd == 1 & !is.na(dd) & !is.na(oo_dd), 0, dd),
         fp = if_else(oo_fp == 1 & !is.na(fp) & !is.na(oo_fp), 0, fp))

# ***** drop date obs if opted out for lead and lags
comp <- comp %>% 
  mutate(bc_date = if_else(oo_bc == 1 & !is.na(bc), ymd(NA), bc_date),
         pp_date = if_else(oo_pp == 1 & !is.na(pp), ymd(NA), pp_date))

# *************************************************************************************************************************
#   *Opt-in Laws
# *************************************************************************************************************************
# ***** CHANGE EFFECTIVE DATES TO MISSING FOR THESE OBS FOR LEAD/LAGS
comp <- comp %>% 
  mutate(bc = if_else(incorp == "GA" & bc == 1, 0, bc),
         fp = if_else(incorp == "GA" & fp == 1, 0, fp),
         cs = if_else(incorp == "TN" & cs == 1, 0, cs)) %>% 
  mutate(bc_date = if_else(incorp == "GA", ymd(NA), bc_date),
         fp_date = if_else(incorp == "GA", ymd(NA), fp_date),
         cs_date = if_else(incorp == "TN", ymd(NA), cs_date))

# *************************************************************************************************************************
#   * Motivating firms
# *************************************************************************************************************************
# set to 0 to start
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

# make the motivating variables - interact with the law being in place
comp <- comp %>% 
  mutate(bcXmotivatingfirmall = bc*motivatingfirmall,
         bcXmotivatingfirmbc = bc*motivatingfirmbc,
         ppXmotivatingfirmpp = pp*motivatingfirmpp,
         csXmotivatingfirmcs = cs*motivatingfirmcs,
         fpXmotivatingfirmfp = fp*motivatingfirmfp,
         ddXmotivatingfirmdd = dd*motivatingfirmdd)

# save the data
saveRDS(comp, here::here("Data/COMPILED", "data3.rds"))
