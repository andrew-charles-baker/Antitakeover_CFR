get_te_dep <- function(dep, yr, tt, dt, data, covs, covtype) {
  
# format data
  dt <- dt %>%
    # merge in dependent variable
    left_join(data %>% select(gvkey, year, all_of(dep)), by = c("gvkey", "year")) %>% 
    # make a relative year variable
    mutate(rel_year = year - yr) %>% 
    # identify treated units
    mutate(treat = if_else(gvkey %in% tt, 1, 0))
  
  # # function to get estimates by relative year
  rel_year_fun <- function(i) {
    
    ref_year <- yr - 1
    
    # filter dataset by relative year
    dt_i <- dt %>% 
      # keep the two needed years of data
      filter(year %in% c(yr + i, ref_year)) %>% 
      # merge in the covariates for earliest year
      mutate(match_year = if_else(i < -1, yr + i, yr - 1)) %>% 
      # merge in the covariates by year
      left_join(data %>% select(gvkey, year, ff_ind_num, all_of(covs)), by = c("gvkey", "match_year" = "year")) %>% 
      # keep just the columns we need
      select(gvkey, year, rel_year, treat, all_of(dep), all_of(covs), ff_ind_num) %>% 
      # identify the post-periods
      mutate(post = if_else(rel_year == i, 1, 0)) %>% 
      drop_na()
   
    # make vectors to feed into DRDID function
    # outcome variable
    Y <- dt_i %>% pull(dep)
    
    # save gvkeys for influence function matrix
    ids <- dt_i %>% pull(gvkey)
    
    # indicator for post
    post <- dt_i %>% pull(post)
    
    # treatment assignment
    D <- dt_i %>% pull(treat)

    # # covariates matrix
    covariates <- dt_i %>%
      select(all_of(covs), ff_ind_num) %>%
      mutate(ff_ind_num = as.factor(ff_ind_num))
    
    # we're not doing weighted
    i.weights = NULL
    
    # run the DRDID
    out <- reg_did_rc(y = Y, post = post, D = D, i.weights = NULL, covariates = covariates,
                       boot = FALSE, inffunc = TRUE)
    
    # output the results
    list(ATT = tibble(depvar = dep, treat_year = yr, rel_year = i, ATT = out$ATT, model = covtype),
         se = tibble(depvar = dep, treat_year = yr, rel_year = i, se = out$se, model = covtype),
         influence = tibble(depvar = dep, treat_year = yr, rel_year = i, gvkey = ids, post = post,
                            D = D, influence = as.vector(out$att.inf.func), model = covtype))
    
  }
  
  # run from t = -5 to t = +5
  last_yr <- min(1995 - yr, 5)
  full <- map(c(-5:-2, 0:last_yr), rel_year_fun)  
  
  # remove rows with missing
  full <- full[lengths(full) > 0]
  
  # unnest the data and output the estimates
  unnested_full <- do.call(function(...) mapply(bind_rows, ..., SIMPLIFY=F), args = full)
  
  if (length(unnested_full) > 0) {
    list(ATT = unnested_full$ATT,
         se = unnested_full$se,
         influence = unnested_full$influence)
  }
}





