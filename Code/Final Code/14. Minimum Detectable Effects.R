## 14. Minimum Detectable Effects.R
## --------------------------------------------------------------

library(tidyverse)
library(fixest)
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

## --- Setup (mirrors Code 12) ----------------------------------
long_covs_stk <- c("size", "age", "size2", "age2", "gen1", "cs", "dd",
                   "fp", "csXcts", "bcXamanda")
long_covs_cs  <- c("size", "age", "size2", "age2", "gen1", "cs", "dd", "fp")

long_fes_ff <- c("firm^dataset", "state_year^dataset",
                 "industry_ff_year^dataset")

depvars <- c("roa", "capEx", "ppegrowth", "assetgrowth",
             "cash", "sga", "leverage")

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

## MDE multiplier: 5% two-sided test, 80% power (Bloom 1995)
mde_factor <- qnorm(0.975) + qnorm(0.80)   # = 2.8016

## Standardize outcomes over the estimation sample (as in Code 05)
standardize <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

data4 <- data4 %>%
  filter(year %>% between(1976, 1995)) %>%
  mutate(across(all_of(depvars), standardize))

## ===============================================================
## PART 1: KW FULL-MODEL BENCHMARKS (standardized, as in Figure 3)
## ===============================================================

KW <- haven::read_dta(here::here("Data/KW", "maindata.dta")) %>%
  filter(year %>% between(1976, 1995)) %>%
  mutate(across(all_of(depvars), standardize))

## full ("long") model covariates from Code 05
cov_long <- c("bc", "size", "age", "size2", "age2", "gen1", "pp", "cs",
              "dd", "fp", "csXcts", "bcXamanda", "bcXmotivatingfirmbc")

get_benchmark <- function(depvar) {
  mod <- feols(.[depvar] ~ .[cov_long] | firm + state_year + industry_year,
               data = KW, cluster = ~incorporation)
  broom::tidy(mod) %>%
    filter(term %in% c("bc", "pp")) %>%
    transmute(var = depvar, law = term,
              kw_benchmark = estimate, kw_pval = p.value)
}

benchmarks <- map_dfr(depvars, get_benchmark)

## ===============================================================
## PART 2: STACKED REGRESSION AGGREGATE SEs (mirrors Code 12)
## ===============================================================

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

## aggregate post-period effect + SE from the stacked event study
run_mde_stk <- function(depvar, law, dt) {

  covs <- if (law == "bc") {
    c(long_covs_stk, "pp", "bcXmotivatingfirmbc")
  } else {
    c(long_covs_stk, "bc", "ppXmotivatingfirmpp")
  }

  fml <- as.formula(
    paste0(depvar, " ~ ",
           paste(covs, collapse = " + "),
           " + i(rel_year, ref = -1) | ",
           paste(long_fes_ff, collapse = " + "))
  )
  est <- feols(fml, cluster = ~incorporation, data = dt)

  coef_all <- coef(est)
  vcov_all <- vcov(est)
  es_idx   <- str_detect(names(coef_all), "^rel_year::")
  rel_vals <- as.numeric(str_extract(names(coef_all)[es_idx], "-?\\d+"))

  post_names <- names(coef_all)[es_idx][rel_vals %in% 0:5]
  post_names <- post_names[order(as.numeric(str_extract(post_names, "-?\\d+")))]

  numPost <- length(post_names)
  l_vec   <- matrix(rep(1 / numPost, numPost), ncol = 1)
  V_post  <- vcov_all[post_names, post_names]

  agg_att <- as.numeric(crossprod(l_vec, coef_all[post_names]))
  agg_se  <- sqrt(as.numeric(t(l_vec) %*% V_post %*% l_vec))

  tibble(method = "Stacked", var = depvar, law = law,
         agg_att = agg_att, agg_se = agg_se)
}

## ===============================================================
## PART 3: CALLAWAY & SANT'ANNA AGGREGATE SEs (mirrors Code 12)
## ===============================================================

data_cs <- data4 %>%
  select(gvkey, year, datadate, firm, state, incorp, incorporation, state_year,
         ff_ind_num, all_of(long_covs_cs), bc, pp, all_of(depvars),
         bc_date, pp_date)

run_mde_cs <- function(dep, law) {

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

  post_pos <- 5:10
  numPost  <- length(post_pos)
  l_vec    <- matrix(rep(1 / numPost, numPost), ncol = 1)

  V_post  <- V_full[post_pos, post_pos]
  agg_att <- as.numeric(crossprod(l_vec, beta_ord[post_pos]))
  agg_se  <- sqrt(as.numeric(t(l_vec) %*% V_post %*% l_vec))

  tibble(method = "CS", var = dep, law = law,
         agg_att = agg_att, agg_se = agg_se)
}

## ===============================================================
## RUN ALL CELLS
## ===============================================================

## --- Stacked: build datasets and run --------------------------
data_bc    <- make_dt(data4, bc)
data_pp    <- make_dt(data4, pp)
stacked_bc <- make_stacked(data_bc, bc)
stacked_pp <- make_stacked(data_pp, pp)

results_stk <- bind_rows(
  map_dfr(depvars, run_mde_stk, law = "bc", dt = stacked_bc),
  map_dfr(depvars, run_mde_stk, law = "pp", dt = stacked_pp)
)

## --- CS: run --------------------------------------------------
results_cs <- bind_rows(
  map_dfr(depvars, run_mde_cs, law = "bc"),
  map_dfr(depvars, run_mde_cs, law = "pp")
)

## --- Combine, compute MDEs, attach benchmarks -----------------
results_all <- bind_rows(results_stk, results_cs) %>%
  mutate(mde = mde_factor * agg_se) %>%
  left_join(benchmarks, by = c("var", "law")) %>%
  left_join(depvar_names, by = "var") %>%
  mutate(
    ## detectable = the design could have rejected an effect of the
    ## size found in KW's full model at 80% power
    detectable = abs(kw_benchmark) >= mde
  )

## ===============================================================
## BUILD LATEX TABLE — all 14 outcome-by-statute cells
## ===============================================================

stars <- function(p) case_when(
  p < 0.01 ~ "$^{***}$",
  p < 0.05 ~ "$^{**}$",
  p < 0.10 ~ "$^{*}$",
  TRUE     ~ ""
)

var_order <- c("ROA", "Capex", "PPE Growth", "Asset Growth",
               "Cash", "SGA Expense", "Leverage")

mde_table_dt <- results_all %>%
  mutate(
    benchmark = paste0(sprintf("%.3f", kw_benchmark), stars(kw_pval))
  ) %>%
  select(varname, law, method, agg_att, agg_se, mde, benchmark) %>%
  mutate(across(c(agg_att, agg_se, mde), ~ sprintf("%.3f", .))) %>%
  pivot_wider(names_from  = law,
              values_from = c(agg_att, agg_se, mde, benchmark)) %>%
  mutate(
    varname = factor(varname, levels = var_order),
    method  = factor(method, levels = c("Stacked", "CS"))
  ) %>%
  arrange(method, varname) %>%
  select(varname,
         agg_att_bc, agg_se_bc, mde_bc, benchmark_bc,
         agg_att_pp, agg_se_pp, mde_pp, benchmark_pp)

mde_table <- mde_table_dt %>%
  kable(format    = "latex",
        booktabs  = TRUE,
        align     = c("l", rep("c", 8)),
        escape    = FALSE,
        label     = "mde",
        caption   = "Minimum Detectable Effects (Model 3)",
        col.names = c("Outcome",
                      "Est.", "SE", "MDE", "KW Full Model",
                      "Est.", "SE", "MDE", "KW Full Model"),
        linesep   = "") %>%
  kable_styling(latex_options = c("HOLD_position"),
                font_size = 8, full_width = T) %>%
  ## fixed width on the outcome column so names don't wrap
  column_spec(1, width = "1in") %>%
  add_header_above(c(" " = 1,
                     "Business Combination Statutes" = 4,
                     "Poison Pill Laws" = 4),
                   bold = TRUE) %>%
  pack_rows("Stacked Regression",   1, 7,  bold = TRUE) %>%
  pack_rows("Callaway-Sant'Anna",   8, 14, bold = TRUE)

write_lines(mde_table, file = paste0(dropbox, "tables/mde_table.tex"))
