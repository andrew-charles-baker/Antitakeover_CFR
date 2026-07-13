library(tidyverse)
library(RPostgres)
library(lubridate)
library(kableExtra)
library(lubridate)
library(fixest)
library(data.table)
library(dataverse)
library(RCurl)
library(tictoc)
options(knitr.kable.NA = '')

# source passwords
source(here::here("Passwords", "Password.R"))

# Connect to WRDS Server --------------------------------------------------
wrds <- dbConnect(Postgres(),
                  host = 'wrds-pgdata.wharton.upenn.edu',
                  port = 9737,
                  user = user,
                  password = password,
                  dbname = 'wrds',
                  sslmode = 'require')

# Load or Download Data ---------------------------------------------------
# load McDonald data, downloaded from website https://sraf.nd.edu/data/augmented-10-x-header-data/
mcd <- read_csv(here::here("Data/MCDONALD", "LM_EDGAR_10X_Header_1994_2018.csv")) 

## Download the Spammann data from dataverse
# load the main data
Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")
dt <- get_file("state_inc.tab", "doi:10.7910/DVN/KBPZ5V")
tmp <- tempfile(fileext = ".csv")
writeBin(as.vector(dt), tmp)
hs <- read_csv(tmp)

# load the crosswalk file
dt <- get_file("CIK_CUSIP_crosswalk.tab", "doi:10.7910/DVN/KBPZ5V")
tmp <- tempfile(fileext = ".dta")
writeBin(as.vector(dt), tmp)
crosswalk <- haven::read_dta(tmp)

# load law enactment dates
enactment_dates <- haven::read_dta(here::here("Data/KW", "Enactment Dates.dta"))

# put variables in a string
vars <- c("gvkey", "fyear", "datadate", "indfmt", "datafmt", "popsrc", "consol",
          "cusip", "at", "sale", "ppent", "dltt", "dlc", "capx", "xsga", 
          "ebitda", "che", "fic", "cik", "sich")

vars <- paste(vars, collapse = ", ")

# Download Compustat data
comp <-tbl(wrds, sql(glue::glue("SELECT {vars} FROM comp.funda"))) %>%
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

# Merge in information from MCD and HS ------------------------------------
# in McDonlad data, get one observation per cik/reporting period
mcd <- mcd %>% 
  # keep only annual filings
  filter(str_detect(f_ftype, "10-K")) %>% 
  # subset the variables we want. Rename weird ba_state issue.
  select(cik, datadate_mcd = conf_per_rpt, mcd_incorp = state_of_incorp, 
         mcd_state = ba_state...32, mcd_sic = sic_num, file_date = f_fdate) %>% 
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

# merge in the mcd data - give one week on either side
# get mcd hits
comp_mcd <- merge(comp %>% setDT(), mcd, by = "cik", allow.cartesian = TRUE) %>%
  # keep just obs within one week of filing date
  .[datadate >= datadate_mcd - weeks(1) & datadate <= datadate_mcd + weeks(1)] %>% 
  # if still multiple, keep the one closest to the datadate
  .[, .SD[abs(datadate - datadate_mcd) == min(abs(datadate - datadate_mcd))], keyby = .(gvkey, fyear)] %>% 
  as_tibble()

# merge in
comp <- comp %>% left_join(comp_mcd %>% select(gvkey, fyear, mcd_incorp, mcd_state), 
                           by = c("gvkey", "fyear"))

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
comp_hs <- merge(comp %>% setDT(), hs_merge, by = "cik", allow.cartesian = TRUE) %>%
  # keep just obs within one week of filing date
  .[datadate >= datadate_hs - weeks(1) & datadate <= datadate_hs + weeks(1)] %>% 
  # if still multiple, keep the one closest to the datadate
  .[, .SD[abs(datadate - datadate_hs) == min(abs(datadate - datadate_hs))], keyby = .(gvkey, fyear)] %>% 
  as_tibble() %>% 
  select(gvkey, fyear, hs_incorp) %>% 
  distinct()

# merge back in to compustat
comp <- comp %>% left_join(comp_hs %>% select(gvkey, fyear, hs_incorp), 
                           by = c("gvkey", "fyear"))

# bring in the crosswalk for the observations without a cik so we can merge on cusip instead
hs_merge <- hs %>% 
  # join into crosswalk
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
comp_hs <- merge(comp %>% setDT(), hs_merge, by = "cusip", allow.cartesian = TRUE) %>%
  # keep just obs within one week of filing date
  .[datadate >= datadate_hs - weeks(1) & datadate <= datadate_hs + weeks(1)] %>% 
  # if still multiple, keep the one closest to the datadate
  .[, .SD[abs(datadate - datadate_hs) == min(abs(datadate - datadate_hs))], keyby = .(gvkey, fyear)] %>% 
  as_tibble() %>% 
  select(gvkey, fyear, hs_incorp_c) %>% 
  distinct()

# merge in - and make one hs_incorp variable
comp <- comp %>% 
  left_join(comp_hs, by = c("gvkey", "fyear")) %>% 
  mutate(hs_incorp = if_else(is.na(hs_incorp), hs_incorp_c, hs_incorp)) %>% 
  select(-hs_incorp_c)

# Download legacy header information for all of the other variables
# note this begins in 2007 so won't do that much work
hist_header <-  tbl(wrds, sql("SELECT gvkey, hchgdt, hchgenddt, hincorp FROM crsp.comphist")) %>% 
  collect() %>% 
  rename(begdate = hchgdt, enddate = hchgenddt, h_header_incorp = hincorp)

# merge data into compustat
# first just merge in historical header information
comp_hist_header <- comp %>% 
  select(gvkey, datadate) %>% 
  left_join(hist_header, by = "gvkey") %>% 
  filter(datadate >= begdate & datadate <= enddate) %>% 
  select(-c("begdate", "enddate"))

# merge historical header info and cusip back into compustat
comp <- comp %>% 
  left_join(comp_hist_header, by = c("gvkey", "datadate"))

# finally add the most recent header data
header <- tbl(wrds, sql("SELECT * FROM comp.company")) %>%
  select(gvkey, header_incorp = incorp, header_sic = sic, header_state = state, header_fic = fic, header_cik = cik) %>% 
  collect()

# merge into final compustat
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
  .[!is.na(mcd_incorp) & !is.na(cik) & !is.na(datadate_mcd)] %>% 
  setnames(
    old = c("mcd_incorp", "mcd_state", "mcd_sic"),
    new = c("mr_mcd_incorp", "mr_mcd_state", "mr_mcd_sic")
  )

# merge into comp dataset - just identifying information
mcd_most_recent <- merge(comp, mcd_most_recent, by = "cik", allow.cartesian = TRUE) %>%
  # keep just most recent obs
  .[datadate <= datadate_mcd] %>% 
  # if still multiple, keep the one closest to the datadate
  .[, .SD[datadate - datadate_mcd == max(datadate - datadate_mcd)], keyby = .(gvkey, fyear)] %>% 
  as_tibble() %>% 
  select(gvkey, fyear, mr_mcd_incorp, mr_mcd_state, mr_mcd_sic, datadate_mcd) %>% 
  distinct()

# merge in to compustat data
comp <- comp %>% left_join(mcd_most_recent, by = c("gvkey", "fyear"))

# Now do the same thing for HS data
hs_most_recent <- hs %>% 
  # keep just the variables we need
  select(cik, state, datadate_hs = filing_period) %>% 
  # drop missing observations on any dimension bc not useful
  drop_na() %>% 
  setDT()

# merge into comp base dataset - just identifying information
hs_most_recent <- merge(comp, hs_most_recent, by = "cik", allow.cartesian = TRUE) %>%
  # keep just most recent obs
  .[datadate <= datadate_hs] %>% 
  # if still multiple, keep the one closest to the datadate
  .[, .SD[datadate - datadate_hs == max(datadate - datadate_hs)], keyby = .(gvkey, fyear)] %>% 
  as_tibble() %>% 
  select(gvkey, fyear, mr_hs_incorp = state, datadate_hs) %>% 
  distinct()

# merge in to main compustat data
comp <- comp %>% left_join(hs_most_recent, by = c("gvkey", "fyear"))

### Finally do the same thing with the historical header info
hist_header_most_recent <- hist_header %>% 
  filter(!is.na(h_header_incorp)) %>% 
  select(gvkey, datadate_h_hist = begdate, mr_h_hist_incorp = h_header_incorp) %>% 
  setDT()

# merge into comp base dataset - just identifying information
hist_header_most_recent <- merge(comp[, .(gvkey, fyear, datadate)], 
                                 hist_header_most_recent, by = "gvkey", allow.cartesian = TRUE) %>%
  # keep just most recent obs
  .[datadate <= datadate_h_hist] %>% 
  # if still multiple, keep the one closest to the datadate
  .[, .SD[datadate - datadate_h_hist == max(datadate - datadate_h_hist)], keyby = .(gvkey, fyear)] %>% 
  as_tibble() %>% 
  distinct()

# merge in to final compustat data
comp <- comp %>% left_join(hist_header_most_recent, by = c("gvkey", "fyear", "datadate"))

### Finally, make one measure of incorporation
# 1) If a nonmissing entry in MCD, HS, Historical Header info - use that (preference for MCD, HS, then Hist Header)
# 2) If still missing use most recent version of MCD, HS, hist header - in that order
# 3) If still missing use the current header file info

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
# load the data on historical headquarter state from mingze-gao's website here
# https://mingze-gao.com/posts/firm-historical-headquarter-state-from-10k/
url <- "https://mingze-gao.com/data/download/corrected_hist_state_1969_2018.dta.zip"

# load the file
tmp <- tempfile()
download.file(url, tmp) 
corrected_headquarter <- haven::read_dta(unz(tmp, "corrected_hist_state_1969_2018.dta"))

# merge in
comp <- comp %>% 
  left_join(corrected_headquarter %>% select(gvkey, fyear, corrected_state))

# do the same thing as above but for headquarter state. Here we use the corrected state file, as well the mcd data.
comp <- comp %>% 
  # set state == first nonmissing in corrected or mcd
  mutate(state = coalesce(corrected_state, mcd_state)) %>% 
  # keep an old version of this variable to be used later for after the last recorded observation
  mutate(state_old = state) %>% 
  arrange(gvkey, fyear) %>% 
  group_by(gvkey) %>% 
  # fill missing entries "downup"
  fill(state, .direction = "downup") %>% 
  mutate(
    # get the last year of state if not all missing
    lastyear = if_else(length(which(!is.na(state))) > 0, max(fyear[which(!is.na(state_old))]), NA_real_),
    # make final state - if after last state use header state, or if all missing use header state, otherwise our spliced series
    state = if_else(is.na(state) | fyear > lastyear, header_state, state)) %>% 
  ungroup() %>% 
  select(-state_old)

# Do FIC ------------------------------------------------------------------
# base FIC ON state of incorproation
comp <- comp %>% 
  mutate(fic = if_else(incorp %in% state.abb, "USA", NA_character_))

# Do CUSIP ------------------------------------------------------------------
# to get these back in time we have to use crsp
# first we need to get the accompanying permno crsp identifier
# get the CRSP - Compustat link file
link <- tbl(wrds, sql("SELECT * FROM crsp.ccmxpf_lnkhist")) %>% 
  filter(linktype %in% c("LC", "LU", "LS")) %>% 
  collect() %>% 
  # if linkeendt is missing set to today
  mutate(linkenddt = if_else(is.na(linkenddt), 
                             lubridate::today(), linkenddt)) %>% 
  setDT()

# set key in data table to make this go faster
setkey(link, gvkey)

# download compustat data
comp <- comp %>% 
  # make a merge date to link between
  mutate(merge_date = datadate) %>% 
  setDT()

# bring in link variable to compustat and keep just the right hit
comp <- link[comp, 
             on = .(gvkey, linkdt <= merge_date, linkenddt >= merge_date), 
             nomatch = NA] %>% 
  .[, count := .N, by = list(gvkey, datadate, fyear)] %>% 
  .[count == 1 | linkprim == "P" | linkprim == "C"] %>% 
  .[, count := NULL] %>% 
  as_tibble()

# now bring in cusip from the crsp.dsenames file
crsp_cusip <- tbl(wrds, sql("SELECT * FROM crsp.dsenames")) %>% 
  select(permno, namedt, nameendt, ncusip) %>% 
  collect()

# first get the most recent cusip information
hist_cusip_most_recent <- crsp_cusip %>% 
  filter(!is.na(ncusip)) %>% 
  select(permno, datadate_h_hist = namedt, mr_h_hist_cusip = ncusip) %>% 
  setDT() 

# get the identifying info from compustat to merge into
comp_base <- comp %>% 
  select(gvkey, fyear, permno = lpermno, datadate) %>% 
  setDT()

# merge into comp base dataset - just identifying information
hist_cusip_most_recent <- merge(comp_base[, .(gvkey, permno, fyear, datadate)], 
                                hist_cusip_most_recent, by = "permno", allow.cartesian = TRUE) %>%
  # keep just most recent obs
  .[datadate <= datadate_h_hist] %>% 
  # if still multiple, keep the one closest to the datadate
  .[, .SD[datadate - datadate_h_hist == max(datadate - datadate_h_hist)], keyby = .(permno, fyear)] %>% 
  as_tibble() %>% 
  distinct()

# merge in
comp <- comp %>% left_join(hist_cusip_most_recent, by = c("gvkey", "fyear", "datadate"))

# bring in the ncusip that matches within that date range
cusip_merge <- comp_base %>% 
  left_join(
    crsp_cusip,
    by = join_by(
      permno,
      datadate >= namedt, datadate <= nameendt
      )
  ) %>% 
  as_tibble() %>% 
  select(gvkey, fyear, h_cusip = ncusip)
          
# keep just one value
comp <- comp %>% 
  left_join(cusip_merge, by = c("gvkey", "fyear")) %>% 
  mutate(header_cusip = cusip,
         cusip = h_cusip,
         cusip_old = cusip) %>% 
  group_by(gvkey) %>% 
  fill(cusip, .direction = "downup") %>% 
  mutate(
    # get the last year of sich if not all missing
    lastyear = if_else(length(which(!is.na(cusip))) > 0, max(fyear[which(!is.na(cusip_old))]), NA_real_),
    # make final sich - if after last sich use sic, or if all missing use sic, otherwise our spliced series
    cusip = if_else(is.na(cusip) | fyear > lastyear, header_cusip, cusip)) %>% 
  ungroup() %>% 
  select(-cusip_old)

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
  mutate(across(starts_with("oo_"), ~change_weird_stuff(.))) %>% 
  mutate(across(starts_with("oo_"), ~replace_na(., 0)))

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
