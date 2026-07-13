library(tidyverse)
library(fixest)
library(ggthemes)
library(lubridate)
library(pbapply)
library(parallel)

options(knitr.kable.NA = '')

# seed for the multiplier bootstrap in mboot()
set.seed(20260707)

theme_set(theme_clean() + theme(plot.background = element_blank(),
                                legend.background = element_blank()))

dropbox <- "~/Dropbox/Apps/Overleaf/Antitakeover/"

# ---------------------------------------------------------------------------
# Load data and utility functions (same as Codes 11 and 14)
# ---------------------------------------------------------------------------
data <- read_rds(here::here("Data/COMPILED", "data4.rds"))

source(here::here("Code/utility_fxs", "reg_did_rc.R"))
source(here::here("Code/utility_fxs", "get_agg_inf_func.R"))
source(here::here("Code/utility_fxs", "get_te_dep.R"))
source(here::here("Code/utility_fxs", "getSE.R"))
source(here::here("Code/utility_fxs", "mboot.did.R"))
source(here::here("Code/utility_fxs", "mboot.R"))
source(here::here("Code/utility_fxs", "wif.R"))

# CS uses pre-treatment covariates only, so csXcts and bcXamanda are excluded
moran_cs_covs <- c("size", "age", "size2", "age2", "gen1", "cs", "dd", "fp")
depvars       <- c("roa", "capEx", "ppegrowth", "assetgrowth", "cash", "sga", "leverage")

depvar_names <- tribble(
  ~depvar,       ~varname,
  "roa",         "ROA",
  "capEx",       "Capex",
  "ppegrowth",   "PPE Growth",
  "assetgrowth", "Asset Growth",
  "cash",        "Cash",
  "sga",         "SGA Expense",
  "leverage",    "Leverage"
)

# Trim data to variables needed
data <- data %>%
  select(gvkey, year, datadate, firm, state, incorp, incorporation,
         state_year, ff_ind_num, all_of(moran_cs_covs),
         bc, pp, all_of(depvars), bc_date, pp_date) %>%
  filter(between(year, 1976, 1995))

# ---------------------------------------------------------------------------
# Moran CS estimator (Model 3 pre-treatment covariates, single cohort g = 1985)
# ---------------------------------------------------------------------------
run_moran_cs <- function(dep) {

  yr      <- 1985L
  covtype <- "Model 3"

  tt <- data %>%
    filter(incorp == "DE") %>%
    select(gvkey, year, all_of(c(dep, moran_cs_covs))) %>%
    drop_na() %>%
    filter(between(year, yr - 1, yr + 1)) %>%
    group_by(gvkey) %>%
    filter(n() == 3) %>%
    pull(gvkey) %>%
    unique()

  if (length(tt) == 0) return(NULL)

  cc <- data %>%
    filter(incorp != "DE") %>%
    select(gvkey, year, all_of(c(dep, moran_cs_covs))) %>%
    drop_na() %>%
    filter(between(year, yr - 1, yr + 1)) %>%
    group_by(gvkey) %>%
    filter(n() == 3) %>%
    pull(gvkey) %>%
    unique()

  dt <- data %>%
    select(gvkey, year, all_of(c(dep, moran_cs_covs))) %>%
    drop_na() %>%
    filter(gvkey %in% c(tt, cc)) %>%
    filter(between(year, yr - 5, yr + 5)) %>%
    select(gvkey, year)

  res       <- get_te_dep(dep = dep, yr = yr, tt = tt, dt = dt,
                          data = data, covs = moran_cs_covs, covtype = covtype)
  ATT       <- res$ATT
  influence <- res$influence %>% mutate(year = treat_year + rel_year)

  norm_inf <- function(x) {
    scalar <- length(x) / sum(!is.na(x))
    newx   <- x * scalar
    newx[is.na(newx)] <- 0
    newx
  }

  weights <- influence %>%
    distinct(depvar, treat_year, rel_year) %>%
    group_by(depvar) %>%
    mutate(wt = 1 / n()) %>%
    ungroup()

  att_coef <- ATT %>%
    left_join(weights, by = c("depvar", "treat_year", "rel_year")) %>%
    mutate(wt_att = ATT * wt) %>%
    group_by(depvar, rel_year) %>%
    summarize(att = sum(wt_att), .groups = "drop")

  inf <- influence %>%
    filter(depvar == dep) %>%
    select(gvkey, post, treat_year, rel_year, influence) %>%
    pivot_wider(id_cols     = c("gvkey", "post"),
                names_from  = c("treat_year", "rel_year"),
                values_from = "influence") %>%
    arrange(gvkey, post) %>%
    select(-c(gvkey, post)) %>%
    mutate(across(everything(), norm_inf))

  treat_mat <- influence %>%
    filter(depvar == dep) %>%
    select(gvkey, post, treat_year, rel_year, D) %>%
    pivot_wider(id_cols     = c("gvkey", "post"),
                names_from  = c("treat_year", "rel_year"),
                values_from = "D") %>%
    arrange(gvkey, post) %>%
    select(-c(gvkey, post))

  att <- ATT %>% filter(depvar == dep) %>% pull(ATT)

  byyear <- function(e) {
    whiche     <- which(weights$rel_year == e)
    pge        <- weights$wt[whiche] %>% as.matrix()
    tt_mat     <- treat_mat[, whiche]
    wif.e      <- wif(tt_mat, pge)
    inf.func.e <- get_agg_inf_func(att         = att,
                                   inffunc1    = inf,
                                   whichones   = whiche,
                                   weights.agg = pge,
                                   wifvar      = wif.e)
    se.e <- mboot(inf.func.e, clustervars = NULL)
    list(inf.func = inf.func.e, se = se.e$se)
  }

  dynamic.se.inner <- map(c(-5:-2, 0:5), byyear)
  dynamic.se.e     <- unlist(BMisc::getListElement(dynamic.se.inner, "se"))
  dynamic.se.e[dynamic.se.e <= sqrt(.Machine$double.eps) * 10] <- NA
  dynamic.crit.val <- mboot(
    simplify2array(BMisc::getListElement(dynamic.se.inner, "inf.func")),
    clustervars = NULL
  )$crit.val

  att_coef %>%
    mutate(dynamic.se.e = dynamic.se.e,
           cval         = dynamic.crit.val,
           model        = covtype)
}

moran_cs_results <- map_dfr(depvars, run_moran_cs)

# ---------------------------------------------------------------------------
# Plot: 7 outcomes, single-cohort event study around Moran (1985)
# ---------------------------------------------------------------------------
p_moran_cs <- moran_cs_results %>%
  rowwise() %>%
  mutate(conf.low  = att - min(1.96, cval) * dynamic.se.e,
         conf.high = att + min(1.96, cval) * dynamic.se.e) %>%
  ungroup() %>%
  select(depvar, rel_year, att, conf.low, conf.high) %>%
  bind_rows(
    tibble(depvar = depvars, rel_year = -1, att = 0, conf.low = 0, conf.high = 0)
  ) %>%
  left_join(depvar_names, by = "depvar") %>%
  mutate(varname = factor(varname, levels = c("ROA", "Capex", "PPE Growth",
                                              "Asset Growth", "Cash",
                                              "SGA Expense", "Leverage"))) %>%
  ggplot(aes(x = rel_year, y = att)) +
  geom_point(size = 1.3, color = "black") +
  geom_line(linewidth = 0.5, color = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.25, linewidth = 0.35, color = "black") +
  geom_hline(yintercept = 0,    linetype = "longdash", color = "#800000FF") +
  geom_vline(xintercept = -0.5, linetype = "longdash", color = "gray50") +
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) +
  scale_y_continuous(position = "left") +
  labs(x = "Years Relative to Moran (1985)", y = "") +
  theme(
    axis.title.y     = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
    strip.background = element_rect(color = "black", fill = NA, linetype = 1),
    axis.text.y      = element_text(hjust = 0.95)
  ) +
  facet_wrap(vars(varname), scales = "free_y", ncol = 2)

ggsave(p_moran_cs,
       filename = paste0(dropbox, "figures/moran_cs.pdf"),
       dpi = 500, width = 6, height = 6)
