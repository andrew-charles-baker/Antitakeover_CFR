library(tidyverse)
library(fixest)
library(ggthemes)
options(knitr.kable.NA = '')

# set the dropbox link to save
dropbox <- "~/Dropbox/Apps/Overleaf/Antitakeover/figures/"

# set ggplot theme
theme_set(theme_clean() + theme(plot.background = element_blank(),
                                legend.background = element_blank()))

# load the data
data4 <- read_rds(here::here("Data/COMPILED", "data4.rds"))

# covariate vectors
short_covs <- c("size", "age", "size2", "age2")

# Base long covs for each focal law. Follows the KW pattern:
#   - always include: size/age controls, gen1, bcXamanda, csXcts
#   - include all other second-gen statutes EXCEPT the focal law and pp
#     (pp is added in Model 3, consistent with how the bc and pp models treat each other)
long_covs_base <- list(
  cs = c("size", "age", "size2", "age2", "gen1", "bc", "dd", "fp", "pp", "csXcts", "bcXamanda"),
  fp = c("size", "age", "size2", "age2", "gen1", "bc", "cs", "dd", "pp", "csXcts", "bcXamanda"),
  dd = c("size", "age", "size2", "age2", "gen1", "bc", "cs", "fp", "pp", "csXcts", "bcXamanda")
)

# Model 3 additions for each focal law: pp + law-specific motivating firm interaction
long_covs_m3 <- list(
  cs = c("csXmotivatingfirmcs"),
  fp = c("fpXmotivatingfirmfp"),
  dd = c("ddXmotivatingfirmdd")
)

# fixed effects (stacked designs use dataset-interacted FEs)
long_fes_ff <- c("firm^dataset", "state_year^dataset", "industry_ff_year^dataset")

# dependent variables
depvars <- c("roa", "capEx", "ppegrowth", "assetgrowth", "cash", "sga", "leverage")

depvar_names <- tribble(
  ~var,          ~varname,
  "roa",         "ROA",
  "capEx",       "Capex",
  "ppegrowth",   "PPE Growth",
  "assetgrowth", "Asset Growth",
  "cash",        "Cash",
  "sga",         "SGA Expense",
  "leverage",    "Leverage"
)

# ---------------------------------------------------------------------------
# make_dt: create a dataset with treat_year merged in, dropping firms that
# enter Compustat already covered by the focal law
# ---------------------------------------------------------------------------
make_dt <- function(dt, law) {

  switches <- dt %>%
    group_by(gvkey) %>%
    mutate(switch = if_else({{law}} == 1 & lag({{law}}) == 0, 1, 0)) %>%
    filter(switch == 1 & year <= 1995) %>%
    select(gvkey, treat_year = year)

  dt %>%
    select(
      gvkey, year, firm, state, state_year,
      starts_with("industry"),
      all_of(c(short_covs, "gen1", "bc", "cs", "dd", "fp", "pp",
               "csXcts", "bcXamanda")),
      incorporation,
      all_of(depvars),
      bcXmotivatingfirmbc, ppXmotivatingfirmpp,
      csXmotivatingfirmcs, fpXmotivatingfirmfp, ddXmotivatingfirmdd
    ) %>%
    filter(year %>% between(1976, 1995)) %>%
    left_join(switches, by = "gvkey") %>%
    group_by(gvkey) %>%
    mutate(treat_year = if_else({{law}} == 0 & year >= max(treat_year), NA_real_, treat_year)) %>%
    distinct() %>%
    group_by(gvkey, year) %>%
    filter(case_when(
      {{law}} == 0 ~ is.na(treat_year) | treat_year == min(treat_year[which(treat_year >= year)]),
      {{law}} == 1 ~ is.na(treat_year) | treat_year == max(treat_year[which(treat_year <= year)])
    ))
}

# ---------------------------------------------------------------------------
# make_stacked: build the stacked dataset for a focal law
# ---------------------------------------------------------------------------
make_stacked <- function(dt, law) {

  yrs <- sort(unique(dt$treat_year))
  yrs <- yrs[which(yrs <= 1995)]

  isgood <- function(tyr) {
    treats <- dt %>%
      filter(treat_year == tyr) %>%
      group_by(gvkey) %>%
      filter(length(gvkey[which(year %>% between(tyr - 1, tyr + 1))]) == 3) %>%
      pull(gvkey) %>%
      unique()
    length(treats) >= 10
  }

  yrs <- yrs[unlist(map(yrs, isgood))]

  stack <- function(tyr) {
    treats <- dt %>%
      filter(treat_year == tyr) %>%
      group_by(gvkey) %>%
      filter(length(gvkey[which(year %>% between(tyr - 1, tyr + 1))]) == 3) %>%
      pull(gvkey) %>%
      unique()

    controls <- dt %>%
      filter(is.na(treat_year) | treat_year != tyr) %>%
      group_by(gvkey) %>%
      filter(length(gvkey[which(year %>% between(tyr - 1, tyr + 1))]) == 3) %>%
      filter(sum({{law}}[which(year %>% between(tyr - 1, tyr + 1))]) == 0) %>%
      pull(gvkey)

    bind_rows(
      dt %>% filter(gvkey %in% treats & year %>% between(tyr - 5, tyr + 5) & treat_year == tyr) %>% mutate(treat = 1),
      dt %>% filter(gvkey %in% controls & year %>% between(tyr - 5, tyr + 5) & {{law}} == 0) %>% mutate(treat = 0)
    ) %>%
      mutate(
        dataset  = tyr,
        rel_year = if_else(treat == 1, year - tyr, -1)
      )
  }

  map_dfr(yrs, stack)
}

# ---------------------------------------------------------------------------
# run_es: estimate event study on a stacked dataset
# ---------------------------------------------------------------------------
run_es <- function(law_name, dt, covs, fes) {

  covtype <- if (identical(covs, "")) "Model 1" else
    if (identical(covs, short_covs)) "Model 2" else "Model 3"

  # for Model 3, append the law-specific extras (pp + motivating firm interaction)
  # covs already contains long_covs_base[[law_name]] from the caller
  if (covtype == "Model 3") {
    covs <- c(covs, long_covs_m3[[law_name]])
  }

  run_mod <- function(depvar, dt, covs, fes) {
    broom::tidy(
      feols(.[depvar] ~ .[covs] + i(rel_year, -1) | .[fes],
            cluster = ~incorporation, data = dt),
      conf.int = TRUE
    ) %>%
      mutate(var = depvar)
  }

  map_dfr(depvars, run_mod, dt = dt, covs = covs, fes = fes) %>%
    filter(str_detect(term, "rel_year")) %>%
    mutate(t = parse_number(term)) %>%
    filter(t %>% between(-5, 5)) %>%
    select(var, t, estimate, conf.low, conf.high) %>%
    bind_rows(
      tibble(
        var      = depvars,
        t        = rep(-1, length(depvars)),
        estimate = rep(0, length(depvars)),
        conf.low = rep(0, length(depvars)),
        conf.high = rep(0, length(depvars))
      )
    ) %>%
    mutate(law  = law_name,
           covs = covtype)
}

# ---------------------------------------------------------------------------
# plot helper
# ---------------------------------------------------------------------------
make_plot <- function(data) {
  data %>%
    left_join(depvar_names, by = "var") %>%
    mutate(varname = factor(varname, levels = c("ROA", "Capex", "PPE Growth", "Asset Growth",
                                               "Cash", "SGA Expense", "Leverage"))) %>%
    ggplot(aes(x = t, y = estimate)) +
    geom_point(fill = "white", shape = 21) + geom_line() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), linetype = "longdash") +
    geom_hline(yintercept = 0, linetype = "longdash", color = "#800000FF") +
    geom_vline(xintercept = -0.5, linetype = "longdash", color = "gray") +
    labs(y = "", x = "Years Relative to Passage") +
    scale_x_continuous(breaks = seq(-5, 5, by = 1)) +
    scale_y_continuous(position = "right") +
    theme(
      axis.title.y     = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
      strip.background = element_rect(color = "black", linetype = 1),
      axis.text.y      = element_text(hjust = 0.95)
    ) +
    facet_grid(vars(varname), vars(covs), scales = "free", switch = "y")
}

# ---------------------------------------------------------------------------
# run for CS, FP, DD
# ---------------------------------------------------------------------------
laws <- tribble(
  ~name, ~sym,     ~file_suffix,
  "cs",  quo(cs),  "cs_stacked",
  "fp",  quo(fp),  "fp_stacked",
  "dd",  quo(dd),  "dd_stacked"
)

# run_law: build stacked data, estimate all three models, plot and save
run_law <- function(name, sym, file_suffix) {

  stacked_law <- make_dt(data4, !!sym) %>% make_stacked(!!sym)

  results <- bind_rows(
    run_es(name, stacked_law, "",                       long_fes_ff),
    run_es(name, stacked_law, short_covs,               long_fes_ff),
    run_es(name, stacked_law, long_covs_base[[name]],   long_fes_ff)
  )

  ggsave(make_plot(results),
         filename = paste0(dropbox, file_suffix, ".pdf"),
         dpi = 500, width = 7.5, height = 9)

}

pwalk(laws, run_law)
