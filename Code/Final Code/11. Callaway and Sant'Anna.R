library(tidyverse)
library(fixest)
library(ggthemes)
library(lubridate)
library(pbapply)
library(parallel)
library(kableExtra)
library(HonestDiD)

options(knitr.kable.NA = '')

# seed for the multiplier bootstrap in mboot()
set.seed(20260707)

# set ggplot theme
theme_set(theme_clean() + theme(plot.background = element_blank(),
                                legend.background = element_blank()))

# set output file location
dropbox <- "~/Dropbox/Apps/Overleaf/Antitakeover/"

# load the cleaned data
data <- read_rds(here::here("Data/COMPILED", "data4.rds"))

# save long covs and short covs
long_covs <- c("size", "age", "size2", "age2", "gen1", "cs", "dd", 
               "fp")

short_covs <- c("size", "age", "size2", "age2")

# put the dependent variables into a vector
depvars <- c("roa", "capEx", "ppegrowth", "assetgrowth", "cash", "sga", "leverage")

# keep just the data we need 
data <- data %>% 
  select(gvkey, year, datadate, firm, state, incorp, incorporation, state_year, ff_ind_num, all_of(long_covs), 
         bc, pp, all_of(depvars), bc_date, pp_date) %>% 
  filter(year %>% between(1976, 1995))

#  source all the intermediate functions from the utility function folder to make this thing manageable
source(here::here("Code/utility_fxs", "reg_did_rc.R"))
source(here::here("Code/utility_fxs", "get_agg_inf_func.R"))
source(here::here("Code/utility_fxs", "get_te_dep.R"))
source(here::here("Code/utility_fxs", "getSE.R"))
source(here::here("Code/utility_fxs", "mboot.did.R"))
source(here::here("Code/utility_fxs", "mboot.R"))
source(here::here("Code/utility_fxs", "wif.R"))
source(here::here("Code/utility_fxs", "rambachan_roth.R"))

# function to run CS 
run_cs <- function(dep, covs, law, law_date) {
  
  # get model type
  covtype <- if(identical(covs, NULL)) "Model 1" else
    if(identical(covs, short_covs)) "Model 2" else "Model 3"
  
  # for long covs need to add in control for bc or pp (opposite of law in Question)
  covs <- if(covtype == "Model 3" & law == "bc") 
    c(covs, "pp") else
      if(covtype == "Model 3" & law == "pp") 
        c(covs, "bc") else
          covs

  # First get a list of valid treatment years - in which 1) the law turns on, 2) there is only one year in separation before the prior
  # observation for a given firm, and 3) the law turned on that year because it was passed in the fiscal year
  treats <- data %>%
    select(gvkey, year, {{law}}, {{law_date}}, datadate) %>%
    set_names(c("gvkey", "year", "law", "law_date", "datadate")) %>%
    arrange(gvkey, year) %>%
    # identify years in which 1) the law turns on, 2) there is only one year in separation before the prior
    # observation for a given firm, and 3) the law turned on that year because it was passed in the fiscal year
    mutate(switch = if_else(law == 1 & lag(law) == 0 & gvkey == lag(gvkey) &
                              year - lag(year) == 1 & 
                              law_date <= datadate & law_date > coalesce(lag(datadate), datadate - years(1)), 1, 0)) %>% 
    # keep a list of just the valid treatment years
    filter(switch == 1 & year < 1995) %>%
    select(gvkey, treat_year = year) %>% 
    # require there to be at least 5 firms in a treated year
    group_by(treat_year) %>% 
    filter(n() >= 5) %>% 
    ungroup()

  # Make function to calculate TE by variable, year -------------------------
  get_te_yr <- function(yr) {
    
    # get treat ids for this year
    tt <- treats %>% filter(treat_year == yr) %>% pull(gvkey)
    
    # make a shorter dataset - it has all treated units with data in t - 5 and t + 5
    # and all control units not treated yet within t + 5
    
    # first make sure that treated units have full observations from t - 1 to t + 1
    tt <- data %>% 
      # firms already identified as treateds
      filter(gvkey %in% tt) %>% 
      # keep just variables we need 
      select(gvkey, year, {{dep}}, all_of(covs), {{law}}) %>% 
      drop_na() %>% 
      set_names(paste0(c("gvkey", "year", dep, covs, "law"))) %>% 
      # keep just years in -1 to + 1
      filter(year %>% between(yr - 1, yr + 1)) %>% 
      # make sure you have three obs with {{law}} = 0,1,1
      group_by(gvkey) %>%
      arrange(year, .by_group = TRUE) %>%
      filter(length(gvkey) == 3) %>%
      filter(law == c(0, 1, 1)) %>%
      pull(gvkey) %>% unique()
    
    # get controls - these are all firms that aren't in the treated group, and also have full covs over 
    # t - 1 to t + 1
    cc <- data %>% 
      # keep just variables we need 
      select(gvkey, year, {{dep}}, all_of(covs), {{law}}) %>% 
      drop_na() %>% 
      set_names(paste0(c("gvkey", "year", dep, covs, "law"))) %>% 
      # drop treated and non-control observations
      filter(!(gvkey %in% tt) & year %>% between(yr - 1, yr + 1) & law == 0) %>% 
      # make sure you have three obs with {{law}} = 0,0,0
      group_by(gvkey) %>%
      arrange(year, .by_group = TRUE) %>%
      filter(length(gvkey) == 3) %>%
      filter(law == c(0, 0, 0)) %>%
      pull(gvkey) %>% unique()
    
    # get data from years t - 5 to t + 5
    dt <- data %>% 
      # keep just variables we need 
      select(gvkey, year, {{dep}}, all_of(covs), {{law}}) %>% 
      drop_na() %>% 
      set_names(paste0(c("gvkey", "year", dep, covs, "law"))) %>% 
      # keep only the firms we need
      filter(gvkey %in% c(tt, cc)) %>% 
      # keep within -5 to + 5
      filter(year >= yr - 5 & year <= yr + 5) %>% 
      # finally require that the law variable stays constant - if it turns in the pre period for any 
      # treated units, drop all obs in that year and earlier. If it turns off post-treatment, drop that 
      # obs and all afterwards. We just want 000011111 type treatment paths. For controls if it turns on
      # in the pre period drop all observations beforehand - if it turns on in the post period drop all after, 
      # we just want treatment paths that look like 00000000.
      group_by(gvkey) %>% 
      mutate(min = if_else(sum(law[which(year < yr)]) > 0, max(year[which(law == 1 & year < yr)]), 0),
             max = case_when(
               gvkey %in% tt ~ if_else(length(which(law == 0 & year >= yr)) > 0, min(year[which(law == 0 & year >= yr)]), Inf),
               gvkey %in% cc ~ if_else(length(which(law == 1 & year >= yr)) > 0, min(year[which(law == 1 & year >= yr)]), Inf)
             )) %>% 
      ungroup() %>% 
      filter(year > min & year < max) %>%
      # keep just the variables we need %>% 
      select(gvkey, year)
    
    # estimate the model over our dependent variable
    get_te_dep(dep = dep, yr = yr, tt = tt, dt = dt, data = data, covs = covs, covtype = covtype)
  }
  
  # iterate by year
  # sort cohorts so pivot_wider column order matches the sorted `weights`
  # order used in the positional aggregation below
  output = map(sort(unique(treats$treat_year)), get_te_yr)
  
  # Now unnest all of the treatment year outcomes into separate datasets
  # get ATT estimates and influence functions in matrices
  unnested_dt <- do.call(function(...) mapply(bind_rows, ..., SIMPLIFY=F), args = output)
  ATT <- unnested_dt$ATT %>% arrange(depvar, treat_year, rel_year)
  influence <- unnested_dt$influence %>% arrange(depvar, treat_year, rel_year, gvkey)
  
  # first get the ATT by relative year, weighted by number of treated units.
  weights <- influence %>% 
    group_by(depvar, rel_year) %>% 
    # total number of treated firms
    mutate(total_count = sum(D)) %>% 
    # by treated year - get sum as a weight
    group_by(depvar, treat_year, rel_year) %>% 
    summarize(wt = sum(D)/ mean(total_count)) %>% 
    ungroup() %>% 
    arrange(depvar, treat_year, rel_year)
  
  # get coefficient estimates for total ATT using the weights
  att_coef <- ATT %>% 
    left_join(weights) %>% 
    # weighted average
    mutate(wt_att = ATT*wt) %>% 
    group_by(depvar, rel_year) %>% 
    summarize(att = sum(wt_att))
  
  # get the influence function and add in the year
  influence <- influence %>% 
    mutate(year = treat_year + rel_year)
  
  # function to replace columns by n/n1 * column, where n is the number of rows in 
  # the influence matrix - or the number of gvkeys ever used, and n1 is the number used in 
  # that particular attgt
  norm_inf <- function(x) {
    scalar <- length(x) / sum(!is.na(x))
    newx <- x * scalar
    newx[which(is.na(newx))] <- 0
    newx
  }
  
  # get the influence function into a matrix keeping just those observations
  # matrix is n (number of gvkeys in data) * TGxTN
  inf <- influence %>% 
    filter(depvar == dep) %>% 
    select(gvkey, post, treat_year, rel_year, influence) %>% 
    pivot_wider(id_cols = c("gvkey", "post"),
                names_from = c("treat_year", "rel_year"),
                values_from = "influence") %>% 
    arrange(gvkey, post) %>% 
    select(-c(gvkey, post)) %>% 
    # across columns normalize by sample and set missing to 0
    mutate(across(everything(), ~norm_inf(.)))
  
  # make a matrix that is whether the firm is treated in a given year, rel-year combo
  treat <- influence %>% 
    filter(depvar == dep) %>% 
    select(gvkey, post, treat_year, rel_year, D) %>% 
    # pivot wider the treatment indicator
    pivot_wider(id_cols = c("gvkey", "post"),
                names_from = c("treat_year", "rel_year"),
                values_from = "D") %>% 
    arrange(gvkey, post) %>% 
    select(-c(gvkey, post)) 
  
  # get the att's in a vector
  att = ATT %>% filter(depvar == dep) %>% pull(ATT)
  
  # get the se  and inf function by year
  byyear <- function(e) {
    # which rows of the weight matrix are in that relative year
    whiche <- which(weights$rel_year == e)
    # save the weights as a matrix
    pge <- weights$wt[whiche] %>% as.matrix()
    # get a matrix where columns are cohorts with that e and rows 
    # are whether the firm is treated in that period
    tt <- treat[, whiche]
    # get the wif
    wif.e <- wif(tt, pge)
    
    # get the agg.inf.func
    inf.func.e <- get_agg_inf_func(att = att,
                                   inffunc1 = inf,
                                   whichones = whiche,
                                   weights.agg = pge,
                                   wifvar = wif.e)
    se.e <- mboot(inf.func.e, clustervars = NULL)
    list(inf.func=inf.func.e, se=se.e$se)
  }
  
  # get the dynamic influence functions
  dynamic.se.inner <- map(c(-5:-2, 0:5), byyear)
  
  # extract se's
  dynamic.se.e <- unlist(BMisc::getListElement(dynamic.se.inner, "se"))
  dynamic.se.e[dynamic.se.e <= sqrt(.Machine$double.eps)*10] <- NA
  
  # extract influence functions
  dynamic.inf.func.e <- simplify2array(BMisc::getListElement(dynamic.se.inner, "inf.func"))
  
  # get critical value
  dynamic.crit.val <- mboot(dynamic.inf.func.e, clustervars = NULL)$crit.val
  
  # output all the event study results
  es_estimates <- att_coef %>% 
    mutate(dynamic.se.e = dynamic.se.e,
           cval = dynamic.crit.val,
           model = covtype,
           law = law)
  
  # finally - get estimates that are the average for years 0 to 5
  att_post <- att_coef %>%
    filter(rel_year >= 0) %>% 
    summarize(att = mean(att)) %>% 
    pull(att)
  
  # which columns in influence function matrix to keep
  which_cols <- which(ATT$rel_year >= 0)
  
  # average post-period event-time IFs to get overall post-treatment IF
  post_e_idx <- which(c(-5:-2, 0:5) >= 0)
  dynamic.inf.func <- rowMeans(dynamic.inf.func.e[, 1, post_e_idx])
  
  # get the bootstrapped standard errors
  boot_out <- mboot(dynamic.inf.func, clustervars = NULL)
  
  # get standard errors and critical value
  dynamic.se <- boot_out$se
  crit.value <- boot_out$crit.val
  
  # do the Roth Rambachan aggregation
  es_honest <- list(
    # needs the dynamic influence functions
    dynamic.inf.func.e = dynamic.inf.func.e,
    # needs the beta coefficients
    beta = att_coef$att,
    # relative time indicators
    egt = c(-5:-2, 0:5)
  )
  
  # get the honest_did estimates
  rr_ci <- honest_did(es_honest, type = "relative_magnitude")
  
  # save the aggregate estimates
  agg_estimates <- tibble(
    var = dep,
    att = att_post,
    se = dynamic.se,
    crit.val = crit.value,
    model = covtype,
    law = law
  )
  
  # export both event study and aggregate estimates
  list(
    event_study = es_estimates,
    aggregate = agg_estimates,
    robust_ci = rr_ci$robust_ci %>% 
      mutate(var = dep, 
             model = covtype,
             law = law)
  )
}

# run the CS model for every combination
mod1_bc <- map(depvars, .f = run_cs, covs = NULL, law = "bc", law_date = "bc_date")
mod1_pp <- map(depvars, .f = run_cs, covs = NULL, law = "pp", law_date = "pp_date")
mod2_bc <- map(depvars, .f = run_cs, covs = short_covs, law = "bc", law_date = "bc_date")
mod2_pp <- map(depvars, .f = run_cs, covs = short_covs, law = "pp", law_date = "pp_date")
mod3_bc <- map(depvars, .f = run_cs, covs = long_covs, law = "bc", law_date = "bc_date")
mod3_pp <- map(depvars, .f = run_cs, covs = long_covs, law = "pp", law_date = "pp_date")

# get the event study estimates and aggregate estimates in one dataset
event_study_data <- bind_rows(
  do.call(function(...) mapply(bind_rows, ..., SIMPLIFY=F), args = mod1_bc)$event_study,
  do.call(function(...) mapply(bind_rows, ..., SIMPLIFY=F), args = mod1_pp)$event_study,
  do.call(function(...) mapply(bind_rows, ..., SIMPLIFY=F), args = mod2_bc)$event_study,
  do.call(function(...) mapply(bind_rows, ..., SIMPLIFY=F), args = mod2_pp)$event_study,
  do.call(function(...) mapply(bind_rows, ..., SIMPLIFY=F), args = mod3_bc)$event_study,
  do.call(function(...) mapply(bind_rows, ..., SIMPLIFY=F), args = mod3_pp)$event_study
)

# matrix with dataset names
depvar_names <- tribble(
  ~depvar, ~varname,
  "roa", "ROA",
  "capEx", "Capex",
  "ppegrowth", "PPE Growth",
  "assetgrowth", "Asset Growth",
  "cash", "Cash",
  "sga", "SGA Expense",
  "leverage", "Leverage"
)

# get plot of event study estimates and CI
cs_es_bc <- event_study_data %>% 
  rowwise() %>% 
  # make confidence intervals
  mutate(conf.low = att - min(1.96, cval)*dynamic.se.e, 
         conf.high = att + min(1.96, cval)*dynamic.se.e) %>% 
  ungroup() %>% 
  filter(law == "bc") %>% 
  select(depvar, rel_year, att, model, conf.low, conf.high) %>% 
  # add in -1
  bind_rows(
    expand_grid(depvar = depvars, rel_year = -1, att = 0,
                model = paste0("Model ", 1:3), conf.low = 0, conf.high = 0)
  ) %>% 
  # bring in names
  left_join(depvar_names, by = "depvar") %>% 
  mutate(varname = factor(varname, levels = c("ROA", "Capex", "PPE Growth", "Asset Growth",
                                              "Cash", "SGA Expense", "Leverage"))) %>% 
  ggplot(aes(x = rel_year, y = att)) + 
  geom_point(fill = "white", shape = 21) + geom_line() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                linetype = "longdash") + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "#800000FF") + 
  geom_vline(xintercept = -0.5,  linetype = "longdash", color = "gray") + 
  labs(y = "Estimate", x = "Years Relative to Passage") + 
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
  scale_y_continuous(position = "right") + 
  labs(y = "") + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        strip.background = element_rect(color = "black", linetype = 1),
        axis.text.y = element_text(hjust = 0.95)) + 
  facet_grid(vars(varname), vars(model), scales = "free", switch = "y")

# save
ggsave(cs_es_bc, filename = paste(dropbox, "figures/cs_bc.pdf", sep = ""), dpi = 500,
       width = 7.5, height = 9)

# get plot of event study estimates and CI
cs_es_pp <- event_study_data %>% 
  rowwise() %>% 
  # make confidence intervals
  mutate(conf.low = att - min(1.96, cval)*dynamic.se.e, 
         conf.high = att + min(1.96, cval)*dynamic.se.e) %>% 
  ungroup() %>% 
  filter(law == "pp") %>% 
  select(depvar, rel_year, att, model, conf.low, conf.high) %>% 
  # add in -1
  bind_rows(
    expand_grid(depvar = depvars, rel_year = -1, att = 0,
                model = paste0("Model ", 1:3), conf.low = 0, conf.high = 0)
  ) %>% 
  # bring in names
  left_join(depvar_names, by = "depvar") %>% 
  mutate(varname = factor(varname, levels = c("ROA", "Capex", "PPE Growth", "Asset Growth",
                                              "Cash", "SGA Expense", "Leverage"))) %>% 
  ggplot(aes(x = rel_year, y = att)) + 
  geom_point(fill = "white", shape = 21) + geom_line() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                linetype = "longdash") + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "#800000FF") + 
  geom_vline(xintercept = -0.5,  linetype = "longdash", color = "gray") + 
  labs(y = "Estimate", x = "Years Relative to Passage") + 
  scale_x_continuous(breaks = seq(-5, 5, by = 1)) + 
  scale_y_continuous(position = "right") + 
  labs(y = "") + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        strip.background = element_rect(color = "black", linetype = 1),
        axis.text.y = element_text(hjust = 0.95)) + 
  facet_grid(vars(varname), vars(model), scales = "free", switch = "y")

# save
ggsave(cs_es_pp, filename = paste(dropbox, "figures/cs_pp.pdf", sep = ""), dpi = 500,
       width = 7.5, height = 9)

# Make a table with the aggregated estimates for releant time periods e \in {0, 5}
table_data <- bind_rows(
  do.call(function(...) mapply(bind_rows, ..., SIMPLIFY=F), args = mod1_bc)$aggregate,
  do.call(function(...) mapply(bind_rows, ..., SIMPLIFY=F), args = mod1_pp)$aggregate,
  do.call(function(...) mapply(bind_rows, ..., SIMPLIFY=F), args = mod2_bc)$aggregate,
  do.call(function(...) mapply(bind_rows, ..., SIMPLIFY=F), args = mod2_pp)$aggregate,
  do.call(function(...) mapply(bind_rows, ..., SIMPLIFY=F), args = mod3_bc)$aggregate,
  do.call(function(...) mapply(bind_rows, ..., SIMPLIFY=F), args = mod3_pp)$aggregate
)

# get in the robust confidence intervals
robust_cis <- bind_rows(
  do.call(function(...) mapply(bind_rows, ..., SIMPLIFY=F), args = mod1_bc)$robust_ci,
  do.call(function(...) mapply(bind_rows, ..., SIMPLIFY=F), args = mod1_pp)$robust_ci,
  do.call(function(...) mapply(bind_rows, ..., SIMPLIFY=F), args = mod2_bc)$robust_ci,
  do.call(function(...) mapply(bind_rows, ..., SIMPLIFY=F), args = mod2_pp)$robust_ci,
  do.call(function(...) mapply(bind_rows, ..., SIMPLIFY=F), args = mod3_bc)$robust_ci,
  do.call(function(...) mapply(bind_rows, ..., SIMPLIFY=F), args = mod3_pp)$robust_ci
) %>% 
  # clean up columns for merging
  mutate(
    mbar05 = paste0("[", format(round(lb, 3), nsmall = 3), ", ", 
                    format(round(ub, 3), nsmall = 3), "]"),
    mbar1 = paste0("[", format(round(lb, 3), nsmall = 3), ", ", 
                   format(round(ub, 3), nsmall = 3), "]")
  )

# small auxiliary function
clean_col <- function(x) as.character(format(round(x, 3), nsmall = 3))

# reformat the data for BC laws
cs_table_bc <- table_data %>% 
  filter(law == "bc") %>% 
  # create a range for the ATT based on the critical value and standard error
  rowwise() %>% 
  mutate(lower_ci = att - 1.96 * se,
         upper_ci = att + 1.96 * se,
         range = paste0("[", format(round(lower_ci, 3), nsmall = 3), ", ", 
                        format(round(upper_ci, 3), nsmall = 3), "]")) %>% 
  ungroup() %>% 
  mutate(across(c(att, se), clean_col)) %>% 
  # bring in robust CIs
  left_join(robust_cis %>% filter(Mbar == 0.5 & law == "bc") %>% select(var, model, mbar05),
            by = c("var", "model")) %>% 
  left_join(robust_cis %>% filter(Mbar == 1 & law == "bc") %>% select(var, model, mbar1),
            by = c("var", "model")) %>% 
  # do some munging
  pivot_longer(
    cols = c(att, se, range, mbar05, mbar1),
    names_to = "statistic",
    values_to = "value"
  ) %>% 
  pivot_wider(
    id_cols = c(var, statistic),
    names_from = model,
    values_from = value
  ) %>% 
  # change statistics names
  mutate(statistic = case_match(statistic,
                                "att" ~ "ATT",
                                "se" ~ "se",
                                "range" ~ "Conf. Int.",
                                "mbar05" ~ "$\\bar{M}$ = 0.5",
                                "mbar1" ~ "$\\bar{M}$ = 1"
  )) %>% 
  mutate(blank = NA_character_) %>% 
  select(-c(var)) %>% 
  select(blank, everything()) %>% 
  kable(format = "latex", booktabs = T, align = 'c', escape = F,
        label = "cs_agg_bc", caption = "Aggregate ATT (0, 5) for BC Laws---CS Method",
        col.names = linebreak(c(" ", " ", "Model 1", "Model 2", "Model 3"), align = 'c'),
        linesep = "") %>% 
  kable_styling(latex_options = c("HOLD_position"), 
                font_size = 8, full_width = T) %>% 
  pack_rows("ROA", 1, 5) %>% 
  pack_rows("Capex", 6, 10) %>% 
  pack_rows("PPE Growth", 11, 15) %>% 
  pack_rows("Asset Growth", 16, 20) %>% 
  pack_rows("Cash", 21, 25) %>%
  pack_rows("SGA Expense", 26, 30) %>% 
  pack_rows("Leverage", 31, 35)

# save the table
write_lines(cs_table_bc, file = paste(dropbox, "tables/cs_table_bc.tex", sep = ""))

# reformat the data for PP laws
cs_table_pp <- table_data %>% 
  filter(law == "pp") %>% 
  # create a range for the ATT based on the critical value and standard error
  rowwise() %>% 
  mutate(lower_ci = att - 1.96 * se,
         upper_ci = att + 1.96 * se,
         range = paste0("[", format(round(lower_ci, 3), nsmall = 3), ", ", 
                        format(round(upper_ci, 3), nsmall = 3), "]")) %>% 
  ungroup() %>% 
  mutate(across(c(att, se), clean_col)) %>% 
  # bring in robust CIs
  left_join(robust_cis %>% filter(Mbar == 0.5 & law == "pp") %>% select(var, model, mbar05),
            by = c("var", "model")) %>% 
  left_join(robust_cis %>% filter(Mbar == 1 & law == "pp") %>% select(var, model, mbar1),
            by = c("var", "model")) %>% 
  # do some munging
  pivot_longer(
    cols = c(att, se, range, mbar05, mbar1),
    names_to = "statistic",
    values_to = "value"
  ) %>% 
  pivot_wider(
    id_cols = c(var, statistic),
    names_from = model,
    values_from = value
  ) %>% 
  # change statistics names
  mutate(statistic = case_match(statistic,
                                "att" ~ "ATT",
                                "se" ~ "se",
                                "range" ~ "Conf. Int.",
                                "mbar05" ~ "$\\bar{M}$ = 0.5",
                                "mbar1" ~ "$\\bar{M}$ = 1"
  )) %>% 
  mutate(blank = NA_character_) %>% 
  select(-c(var)) %>% 
  select(blank, everything()) %>% 
  kable(format = "latex", booktabs = T, align = 'c', escape = F,
        label = "cs_agg_pp", caption = "Aggregate ATT (0, 5) for PP Laws---CS Method",
        col.names = linebreak(c(" ", " ", "Model 1", "Model 2", "Model 3"), align = 'c'),
        linesep = "") %>% 
  kable_styling(latex_options = c("HOLD_position"), 
                font_size = 8, full_width = T) %>% 
  pack_rows("ROA", 1, 5) %>% 
  pack_rows("Capex", 6, 10) %>% 
  pack_rows("PPE Growth", 11, 15) %>% 
  pack_rows("Asset Growth", 16, 20) %>% 
  pack_rows("Cash", 21, 25) %>%
  pack_rows("SGA Expense", 26, 30) %>% 
  pack_rows("Leverage", 31, 35)

# save the table
write_lines(cs_table_pp, file = paste(dropbox, "tables/cs_table_pp.tex", sep = ""))