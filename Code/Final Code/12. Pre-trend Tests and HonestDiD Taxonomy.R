## 12. Pre-trend Tests and HonestDiD Taxonomy.R
## --------------------------------------------------------------
## For each (outcome x statute) cell under Model 3, this script
## runs BOTH the stacked-regression and Callaway-Sant'Anna event
## studies, performs:
##   1. A joint Wald test on the pre-period coefficients
##      (e in {-5, -4, -3, -2}, with e = -1 omitted).
##   2. Rambachan & Roth (2023) sensitivity analysis on the
##      average post-period effect at M-bar = 1 (also reports M = 0.5).

## side-by-side, written to tables/taxonomy_table.tex.

library(tidyverse)
library(fixest)
library(HonestDiD)
library(kableExtra)
library(lubridate)
library(BMisc)
options(knitr.kable.NA = '')

# seed for the multiplier bootstrap in mboot()
set.seed(20260707)

dropbox <- "~/Dropbox/Apps/Overleaf/Antitakeover/"

## --- Load data and source CS utility functions ---------------
data4 <- read_rds(here::here("Data/COMPILED", "data4.rds"))

source(here::here("Code/utility_fxs", "reg_did_rc.R"))
source(here::here("Code/utility_fxs", "get_agg_inf_func.R"))
source(here::here("Code/utility_fxs", "get_te_dep.R"))
source(here::here("Code/utility_fxs", "getSE.R"))
source(here::here("Code/utility_fxs", "mboot.did.R"))
source(here::here("Code/utility_fxs", "mboot.R"))
source(here::here("Code/utility_fxs", "wif.R"))
source(here::here("Code/utility_fxs", "rambachan_roth.R"))

## --- Setup ----------------------------------------------------
long_covs_stk <- c("size", "age", "size2", "age2", "gen1", "cs", "dd",
                   "fp", "csXcts", "bcXamanda")
long_covs_cs  <- c("size", "age", "size2", "age2", "gen1", "cs", "dd", "fp")

long_fes_ff <- c("firm^dataset", "state_year^dataset",
                 "industry_ff_year^dataset")

depvars <- c("roa", "capEx", "ppegrowth", "assetgrowth",
             "cash", "sga", "leverage")

## Standard agency-theoretic predicted directions
##   ROA           DECREASE  (quiet life / lower productivity)
##   Capex         DECREASE  (quiet life)
##   PPE Growth    INCREASE  (empire building)
##   Asset Growth  INCREASE  (empire building)
##   Cash          INCREASE  (free cash flow hoarding)
##   SG&A          INCREASE  (managerial slack / perks)
##   Leverage      DECREASE  (Garvey & Noel 1999; less debt discipline)
expected_direction <- tribble(
  ~var,           ~direction,
  "roa",          "negative",
  "capEx",        "negative",
  "ppegrowth",    "positive",
  "assetgrowth",  "positive",
  "cash",         "positive",
  "sga",          "positive",
  "leverage",     "negative"
)

## --- Shared classification logic ------------------------------
classify <- function(wald_pval, rr_lb, rr_ub, exp_dir) {
  if (is.na(wald_pval))                        return("(NA)")
  if (wald_pval < 0.05)                        return("(a)")
  if (is.na(rr_lb) | is.na(rr_ub))             return("(NA)")
  if (rr_lb <= 0 & rr_ub >= 0)                 return("(b)")
  sign_eff <- if (rr_lb > 0) "positive" else "negative"
  if (sign_eff == exp_dir)                     return("(c)")
  return("(d)")
}

## ===============================================================
## PART 1: STACKED REGRESSION (mirrors Code 10)
## ===============================================================

## --- Stacking helpers ---------------------------------------
# clean datasets
make_dt <- function(dt, law) {

  switches <- dt %>%
    group_by(gvkey) %>%
    mutate(switch = if_else({{law}} == 1 & lag({{law}}) == 0, 1, 0)) %>%
    filter(switch == 1 & year <= 1995) %>%
    select(gvkey, treat_year = year)

  dt %>%
    select(gvkey, year, firm, state, state_year, starts_with("industry"),
           all_of(long_covs_stk), bc, pp, incorporation, all_of(depvars),
           bcXmotivatingfirmbc, ppXmotivatingfirmpp) %>%
    filter(year %>% between(1976, 1995)) %>%
    left_join(switches, by = "gvkey") %>%
    group_by(gvkey) %>%
    mutate(treat_year = if_else({{law}} == 0 & year >= max(treat_year),
                                NA_real_, treat_year)) %>%
    distinct() %>%
    group_by(gvkey, year) %>%
    filter(case_when(
      {{law}} == 0 ~ is.na(treat_year) |
        treat_year == min(treat_year[which(treat_year >= year)]),
      {{law}} == 1 ~ is.na(treat_year) |
        treat_year == max(treat_year[which(treat_year <= year)])
    ))
}

# make stacked datasets for stacked regressions
make_stacked <- function(dt, law) {

  yrs <- sort(unique(dt$treat_year))
  yrs <- yrs[which(yrs <= 1995)]

  isgood <- function(tyr) {
    treats <- dt %>%
      filter(treat_year == tyr) %>%
      group_by(gvkey) %>%
      filter(length(gvkey[which(year %>% between(tyr - 1, tyr + 1))]) == 3) %>%
      pull(gvkey) %>% unique()
    length(treats) >= 10
  }
  yrs <- yrs[unlist(map(yrs, isgood))]

  stack <- function(tyr) {
    treats <- dt %>%
      filter(treat_year == tyr) %>%
      group_by(gvkey) %>%
      filter(length(gvkey[which(year %>% between(tyr - 1, tyr + 1))]) == 3) %>%
      pull(gvkey) %>% unique()

    controls <- dt %>%
      filter(is.na(treat_year) | treat_year != tyr) %>%
      group_by(gvkey) %>%
      filter(length(gvkey[which(year %>% between(tyr - 1, tyr + 1))]) == 3) %>%
      filter(sum({{law}}[which(year %>% between(tyr - 1, tyr + 1))]) == 0) %>%
      pull(gvkey)

    bind_rows(
      dt %>%
        filter(gvkey %in% treats &
                 year %>% between(tyr - 5, tyr + 5) &
                 treat_year == tyr) %>% mutate(treat = 1),
      dt %>%
        filter(gvkey %in% controls &
                 year %>% between(tyr - 5, tyr + 5) &
                 {{law}} == 0) %>% mutate(treat = 0)
    ) %>%
      mutate(dataset = tyr,
             rel_year = if_else(treat == 1, year - tyr, -1))
  }

  map_dfr(yrs, stack)
}

## --- Stacked taxonomy function ------------------------------
run_categorization_stk <- function(depvar, law, dt) {
  
  # figure out the relevant covariates depending on law
  covs <- if (law == "bc") {
    c(long_covs_stk, "pp", "bcXmotivatingfirmbc")
  } else {
    c(long_covs_stk, "bc", "ppXmotivatingfirmpp")
  }

  # write event study formula
  fml <- as.formula(
    paste0(depvar, " ~ ",
           paste(covs, collapse = " + "),
           " + i(rel_year, ref = -1) | ",
           paste(long_fes_ff, collapse = " + "))
  )
  # estimate the model
  est <- feols(fml, cluster = ~incorporation, data = dt)
  
  # store coefficients and vcov matrix
  coef_all   <- coef(est)
  vcov_all   <- vcov(est)
  es_idx     <- str_detect(names(coef_all), "^rel_year::")
  rel_vals   <- as.numeric(str_extract(names(coef_all)[es_idx], "-?\\d+"))
  
  # keep just the relevant event time indicators
  keep_idx   <- es_idx
  keep_idx[es_idx] <- rel_vals %in% c(-5:-2, 0:5)
  keep_names <- names(coef_all)[keep_idx]
  keep_vals  <- as.numeric(str_extract(keep_names, "-?\\d+"))

  # store all of the relevant info for wald tests and rambachan roth
  ord       <- order(keep_vals)
  ord_names <- keep_names[ord]
  ord_vals  <- keep_vals[ord]
  betas_ord <- coef_all[ord_names]
  vcov_ord  <- vcov_all[ord_names, ord_names]

  pre_pos   <- which(ord_vals < 0)
  post_pos  <- which(ord_vals >= 0)
  numPre    <- length(pre_pos)
  numPost   <- length(post_pos)

  wald_pval <- wald(est, keep = ord_names[pre_pos])$p

  l_vec     <- matrix(rep(1 / numPost, numPost), ncol = 1)
  V_post    <- vcov_ord[post_pos, post_pos]
  agg_att   <- as.numeric(crossprod(l_vec, betas_ord[post_pos]))
  agg_se    <- sqrt(as.numeric(t(l_vec) %*% V_post %*% l_vec))

  # run the rambachan roth method
  rr <- tryCatch(
    createSensitivityResults_relativeMagnitudes(
      betahat        = betas_ord,
      sigma          = vcov_ord,
      numPrePeriods  = numPre,
      numPostPeriods = numPost,
      l_vec          = l_vec,
      Mbarvec        = c(0.5, 1)
    ),
    error = function(e) {
      message("HonestDiD failed (stacked) for ", depvar, " / ", law, ": ", e$message)
      NULL
    }
  )

  # keep lower and upper boudns for M05 and M1
  if (is.null(rr)) {
    rr_lb_05 <- NA_real_; rr_ub_05 <- NA_real_
    rr_lb_1  <- NA_real_; rr_ub_1  <- NA_real_
  } else {
    rr_lb_05 <- rr %>% filter(Mbar == 0.5) %>% pull(lb)
    rr_ub_05 <- rr %>% filter(Mbar == 0.5) %>% pull(ub)
    rr_lb_1  <- rr %>% filter(Mbar == 1)   %>% pull(lb)
    rr_ub_1  <- rr %>% filter(Mbar == 1)   %>% pull(ub)
  }

  exp_dir <- expected_direction %>% filter(var == depvar) %>% pull(direction)
  cat_lbl <- classify(wald_pval, rr_lb_1, rr_ub_1, exp_dir)
  
  # output all of the relevant information
  tibble(
    method       = "Stacked",
    var          = depvar,
    law          = law,
    wald_pval    = wald_pval,
    agg_att      = agg_att,
    agg_se       = agg_se,
    rr_lb_mbar05 = rr_lb_05, rr_ub_mbar05 = rr_ub_05,
    rr_lb_mbar1  = rr_lb_1,  rr_ub_mbar1  = rr_ub_1,
    expected_dir = exp_dir,
    category     = cat_lbl
  )
}

## ===============================================================
## PART 2: CALLAWAY & SANT'ANNA (mirrors Code 11, generalised)
## ===============================================================

# make CS data to run the model
data_cs <- data4 %>%
  select(gvkey, year, datadate, firm, state, incorp, incorporation, state_year,
         ff_ind_num, all_of(long_covs_cs), bc, pp, all_of(depvars),
         bc_date, pp_date) %>%
  filter(year %>% between(1976, 1995))

run_categorization_cs <- function(dep, law) {

  law_sym  <- sym(law)
  law_date <- paste0(law, "_date")
  other    <- if (law == "pp") "bc" else "pp"
  covs     <- c(long_covs_cs, other)
  covtype  <- "Model 3"

  treats <- data_cs %>%
    select(gvkey, year, !!law_sym, !!sym(law_date), datadate) %>%
    arrange(gvkey, year) %>%
    mutate(switch = if_else(
      !!law_sym == 1 & lag(!!law_sym) == 0 & gvkey == lag(gvkey) &
        year - lag(year) == 1 &
        !!sym(law_date) <= datadate &
        !!sym(law_date) > coalesce(lag(datadate), datadate - years(1)),
      1, 0
    )) %>%
    filter(switch == 1 & year < 1995) %>%
    select(gvkey, treat_year = year) %>%
    group_by(treat_year) %>% filter(n() >= 5) %>% ungroup()

  get_te_yr <- function(yr) {

    tt <- data_cs %>%
      filter(gvkey %in% (treats %>% filter(treat_year == yr) %>% pull(gvkey))) %>%
      select(gvkey, year, !!sym(dep), all_of(covs), !!law_sym) %>%
      drop_na() %>%
      filter(year %>% between(yr - 1, yr + 1)) %>%
      group_by(gvkey) %>%
      arrange(year, .by_group = TRUE) %>%
      filter(length(gvkey) == 3) %>%
      filter(!!law_sym == c(0, 1, 1)) %>%
      pull(gvkey) %>% unique()

    cc <- data_cs %>%
      select(gvkey, year, !!sym(dep), all_of(covs), !!law_sym) %>%
      drop_na() %>%
      filter(!(gvkey %in% tt) & year %>% between(yr - 1, yr + 1) & !!law_sym == 0) %>%
      group_by(gvkey) %>%
      arrange(year, .by_group = TRUE) %>%
      filter(length(gvkey) == 3) %>%
      filter(!!law_sym == c(0, 0, 0)) %>%
      pull(gvkey) %>% unique()
    
    # do the relevant restrictions
    dt <- data_cs %>%
      select(gvkey, year, !!sym(dep), all_of(covs), !!law_sym) %>%
      drop_na() %>%
      filter(gvkey %in% c(tt, cc)) %>%
      filter(year >= yr - 5 & year <= yr + 5) %>%
      group_by(gvkey) %>%
      mutate(
        min = if_else(sum((!!law_sym)[which(year < yr)]) > 0,
                      max(year[which(!!law_sym == 1 & year < yr)]), 0),
        max = case_when(
          gvkey %in% tt ~ if_else(length(which(!!law_sym == 0 & year >= yr)) > 0,
                                  min(year[which(!!law_sym == 0 & year >= yr)]), Inf),
          gvkey %in% cc ~ if_else(length(which(!!law_sym == 1 & year >= yr)) > 0,
                                  min(year[which(!!law_sym == 1 & year >= yr)]), Inf)
        )
      ) %>%
      ungroup() %>%
      filter(year > min & year < max) %>%
      select(gvkey, year)

    get_te_dep(dep = dep, yr = yr, tt = tt, dt = dt,
               data = data_cs, covs = covs, covtype = covtype)
  }

  output      <- map(sort(unique(treats$treat_year)), get_te_yr)
  unnested_dt <- do.call(function(...) mapply(bind_rows, ..., SIMPLIFY = F), args = output)
  # arrange so that the pivot_wider column order matches the sorted order of
  # `weights` below -- the aggregation subsets positionally, so misordered
  # cohorts silently apply the wrong weights to the influence functions
  ATT         <- unnested_dt$ATT %>% arrange(depvar, treat_year, rel_year)
  influence   <- unnested_dt$influence %>% arrange(depvar, treat_year, rel_year, gvkey)

  weights <- influence %>%
    group_by(depvar, rel_year) %>%
    mutate(total_count = sum(D)) %>%
    group_by(depvar, treat_year, rel_year) %>%
    summarize(wt = sum(D) / mean(total_count), .groups = "drop")

  att_coef <- ATT %>%
    left_join(weights, by = c("depvar", "treat_year", "rel_year")) %>%
    mutate(wt_att = ATT * wt) %>%
    group_by(depvar, rel_year) %>%
    summarize(att = sum(wt_att), .groups = "drop") %>%
    arrange(rel_year)

  norm_inf <- function(x) {
    scalar <- length(x) / sum(!is.na(x))
    newx <- x * scalar
    newx[which(is.na(newx))] <- 0
    newx
  }

  inf_mat <- influence %>%
    filter(depvar == dep) %>%
    select(gvkey, post, treat_year, rel_year, influence) %>%
    pivot_wider(id_cols = c("gvkey", "post"),
                names_from = c("treat_year", "rel_year"),
                values_from = "influence") %>%
    arrange(gvkey, post) %>% select(-c(gvkey, post)) %>%
    mutate(across(everything(), ~ norm_inf(.)))

  treat_mat <- influence %>%
    filter(depvar == dep) %>%
    select(gvkey, post, treat_year, rel_year, D) %>%
    pivot_wider(id_cols = c("gvkey", "post"),
                names_from = c("treat_year", "rel_year"),
                values_from = "D") %>%
    arrange(gvkey, post) %>% select(-c(gvkey, post))

  att_vec <- ATT %>% filter(depvar == dep) %>% pull(ATT)

  byyear <- function(e) {
    whiche <- which(weights$rel_year == e)
    pge    <- weights$wt[whiche] %>% as.matrix()
    tt     <- treat_mat[, whiche]
    wif.e  <- wif(tt, pge)
    inf.func.e <- get_agg_inf_func(att = att_vec,
                                   inffunc1   = inf_mat,
                                   whichones  = whiche,
                                   weights.agg = pge,
                                   wifvar     = wif.e)
    list(inf.func = inf.func.e)
  }

  dynamic_inner      <- map(c(-5:-2, 0:5), byyear)
  dynamic_inf_func_e <- simplify2array(BMisc::getListElement(dynamic_inner, "inf.func"))

  beta_ord <- att_coef %>% filter(rel_year %in% c(-5:-2, 0:5)) %>%
    arrange(rel_year) %>% pull(att)

  inf_func_es <- dynamic_inf_func_e[, 1, ] %>% as.matrix()
  n_inf       <- nrow(inf_func_es)
  V_full      <- t(inf_func_es) %*% inf_func_es / n_inf / n_inf

  pre_pos    <- 1:4
  post_pos   <- 5:10
  numPre     <- length(pre_pos)
  numPost    <- length(post_pos)
  l_vec      <- matrix(rep(1 / numPost, numPost), ncol = 1)

  betas_pre  <- beta_ord[pre_pos]
  V_pre      <- V_full[pre_pos, pre_pos]
  wald_stat  <- as.numeric(t(betas_pre) %*% solve(V_pre) %*% betas_pre)
  wald_pval  <- pchisq(wald_stat, df = numPre, lower.tail = FALSE)

  V_post  <- V_full[post_pos, post_pos]
  agg_att <- as.numeric(crossprod(l_vec, beta_ord[post_pos]))
  agg_se  <- sqrt(as.numeric(t(l_vec) %*% V_post %*% l_vec))

  es_honest <- list(
    dynamic.inf.func.e = dynamic_inf_func_e,
    beta = beta_ord,
    egt  = c(-5:-2, 0:5)
  )

  rr <- tryCatch(
    honest_did(es_honest, type = "relative_magnitude")$robust_ci,
    error = function(e) {
      message("HonestDiD failed (CS) for ", dep, " / ", law, ": ", e$message)
      NULL
    }
  )

  if (is.null(rr)) {
    rr_lb_05 <- NA_real_; rr_ub_05 <- NA_real_
    rr_lb_1  <- NA_real_; rr_ub_1  <- NA_real_
  } else {
    rr_lb_05 <- rr %>% filter(Mbar == 0.5) %>% pull(lb)
    rr_ub_05 <- rr %>% filter(Mbar == 0.5) %>% pull(ub)
    rr_lb_1  <- rr %>% filter(Mbar == 1)   %>% pull(lb)
    rr_ub_1  <- rr %>% filter(Mbar == 1)   %>% pull(ub)
  }

  exp_dir <- expected_direction %>% filter(var == dep) %>% pull(direction)
  cat_lbl <- classify(wald_pval, rr_lb_1, rr_ub_1, exp_dir)
  
  # output all of the relevant information
  tibble(
    method       = "CS",
    var          = dep,
    law          = law,
    wald_pval    = wald_pval,
    agg_att      = agg_att,
    agg_se       = agg_se,
    rr_lb_mbar05 = rr_lb_05, rr_ub_mbar05 = rr_ub_05,
    rr_lb_mbar1  = rr_lb_1,  rr_ub_mbar1  = rr_ub_1,
    expected_dir = exp_dir,
    category     = cat_lbl
  )
}

## ===============================================================
## RUN BOTH METHODS FOR ALL 14 CELLS
## ===============================================================

## --- Stacked: build datasets and run --------------------------
data_bc    <- make_dt(data4, bc)
data_pp    <- make_dt(data4, pp)
stacked_bc <- make_stacked(data_bc, bc)
stacked_pp <- make_stacked(data_pp, pp)

results_stk <- bind_rows(
  map_dfr(depvars, run_categorization_stk, law = "bc", dt = stacked_bc),
  map_dfr(depvars, run_categorization_stk, law = "pp", dt = stacked_pp)
)

## --- CS: run --------------------------------------------------
results_cs <- bind_rows(
  map_dfr(depvars, run_categorization_cs, law = "bc"),
  map_dfr(depvars, run_categorization_cs, law = "pp")
)

results_all <- bind_rows(results_stk, results_cs)

## ===============================================================
## BUILD COMBINED LATEX TABLE
## ===============================================================

depvar_names <- tribble(
  ~var,           ~varname,
  "roa",          "ROA",
  "capEx",        "Capex",
  "ppegrowth",    "PPE Growth",
  "assetgrowth",  "Asset Growth",
  "cash",         "Cash",
  "sga",          "SGA Expense",
  "leverage",     "Leverage"
)

## "Yes/No" indicators for the four-column-per-method format
##   pre_viol  : pre-trend Wald test rejects (p < 0.05)
##   post_viol : R-R CI at M-bar = 1 excludes zero

yn <- function(x) ifelse(is.na(x), "", ifelse(x, "Yes", "No"))

build_method_panel <- function(df) {
  df %>%
    left_join(depvar_names, by = "var") %>%
    mutate(
      pre_viol  = wald_pval < 0.05,
      post_viol = !is.na(rr_lb_mbar05) & !is.na(rr_ub_mbar05) &
                  (rr_lb_mbar05 > 0 | rr_ub_mbar05 < 0),
      pre_str  = yn(pre_viol),
      post_str = yn(post_viol)
    ) %>%
    select(law, varname, pre_str, post_str)
}

stk_panel <- build_method_panel(results_stk) %>%
  rename(pre_stk = pre_str, post_stk = post_str)

cs_panel  <- build_method_panel(results_cs) %>%
  rename(pre_cs = pre_str, post_cs = post_str)

combined <- stk_panel %>%
  left_join(cs_panel, by = c("law", "varname")) %>%
  mutate(
    varname = factor(varname,
                     levels = c("ROA", "Capex", "PPE Growth", "Asset Growth",
                                "Cash", "SGA Expense", "Leverage")),
    law = factor(law, levels = c("bc", "pp"))
  ) %>%
  arrange(law, varname) %>%
  select(varname, pre_stk, post_stk, pre_cs, post_cs)

table <- combined %>%
  kable(format    = "latex",
        booktabs  = TRUE,
        align     = c("l", rep("c", 4)),
        escape    = FALSE,
        label     = "comparison",
        caption   = "Results Summary Across Specifications (Model 3)",
        col.names = c("Outcome",
                      "Pre-trend Viol.", "Post-period Effect",
                      "Pre-trend Viol.", "Post-period Effect"),
        linesep   = "") %>%
  kable_styling(latex_options = c("HOLD_position"),
                font_size = 8, full_width = T) %>%
  add_header_above(c(" " = 1,
                     "Stacked Regression" = 2,
                     "Callaway-Sant'Anna" = 2),
                   bold = TRUE) %>%
  pack_rows("Business Combination Statutes", 1, 7, bold = TRUE) %>%
  pack_rows("Poison Pill Laws",              8, 14, bold = TRUE)

# Write latex table
write_lines(table,
            file = paste0(dropbox, "tables/comparison_table.tex"))
