library(tidyverse)
library(fixest)
library(ggthemes)
options(knitr.kable.NA = '')

# set the dropbox folder
dropbox <- "~/Dropbox/Apps/Overleaf/Antitakeover/"

# set ggplot theme
theme_set(theme_clean() + theme(plot.background = element_blank(),
                                legend.background = element_blank()))

# load the data
# data 1 is the KW data
data1 <-  haven::read_dta(here::here("Data/KW", "maindata.dta"))

# data4 is our data with all changes made
data4 <- read_rds(here::here("Data/COMPILED", "data4.rds"))

# save long covs and short covs in vectors
long_covs <- c("size", "age", "size2", "age2", "gen1", "cs", "dd", 
               "fp", "csXcts", "bcXamanda")

short_covs <- c("size", "age", "size2", "age2")

# save different fes
long_fes <- c("firm", "state_year", "industry_year")
long_fes_ff <-  c("firm", "state_year", "industry_ff_year")

# function to clean data - bring in relative time indicators by law.
make_dt <- function(dt, law) {
  
  # first make relative time dummies
  # get switches - periods when the law switches on
  switches <- dt %>% 
    group_by(gvkey) %>% 
    # switch is either change from 0 - 1 or starting with 1
    mutate(switch = if_else({{law}} == 1 & lag({{law}}) == 0 | 
                              {{law}} == 1 & is.na(lag({{law}})), 1, 0)) %>% 
    # keep only treatments that occur within our data
    filter(switch == 1 & year <= 1995) %>% 
    select(gvkey, treat_year = year)
  
  # merge in and make time dummies
  dt <- dt %>% 
    left_join(switches, by = "gvkey") %>%
    group_by(gvkey) %>% 
    # replace switch_year to NA if bc = 0 and treat year is after last switch
    mutate(treat_year = if_else({{law}} == 0 & year >= max(treat_year), NA_real_, treat_year)) %>% 
    distinct() %>% 
    group_by(gvkey, year) %>%
    # for a given firm-year observation, if there are multiple treatments, look only at the most recent one
    filter(case_when(
      {{law}} == 0 ~ is.na(treat_year) | treat_year == min(treat_year[which(treat_year >= year)]),
      {{law}} == 1 ~ is.na(treat_year) | treat_year == max(treat_year[which(treat_year <= year)])
    )) %>% 
    # if first observation for a firm is already treated drop that firm
    group_by(gvkey) %>% 
    filter({{law}}[year == min(year)] != 1) %>% 
    arrange(gvkey, year) %>% 
    ungroup() %>% 
    # make relative time dummies
    mutate(rel_year = coalesce(year - treat_year, -1))
}

# make datasets for BC and PP laws
data_bc <- make_dt(data4, bc)
data_pp <- make_dt(data4, pp)

# put the dependent variables into a vector
depvars <- c("roa", "capEx", "ppegrowth", "assetgrowth", "cash", "sga", "leverage")

# make code for event studies
run_es <- function(law, dt, covs, fes) {
  
  # which model is it?
  covtype <- if(identical(covs, "")) "Model 1" else
      if(identical(covs, short_covs)) "Model 2" else "Model 3"
  
  # for long covs need to add in control for bc or pp (opposite of law in Question)
  covs <- if(covtype == "Model 3" & law == "bc") 
    c(covs, "pp", "bcXmotivatingfirmbc") else
      if(covtype == "Model 3" & law == "pp") 
        c(covs, "bc", "ppXmotivatingfirmpp") else
          covs

  # filter dataset
  dt <- dt %>% filter(year %>% between(1976, 1995))
  
  # make a function to run the model
  run_mod <- function(depvar, dt, covs, fes) {
    broom::tidy(
      feols(.[depvar] ~ .[covs] + i(rel_year, -1) | .[fes], cluster = ~incorporation, data = dt),
      conf.int = TRUE
    ) %>% 
      # column to identify the dependent variable used
      mutate(var = depvar)
  }
  
  # run model over our dependent variables
  map_dfr(depvars, run_mod, dt = dt, covs = covs, fes = fes) %>%
    # keep just -5 to +5 indicators 
    filter(str_detect(term, "rel_year")) %>% 
    mutate(t = parse_number(term)) %>% 
    filter(t %>% between(-5, 5)) %>% 
    # drop unnecessary variables from broom::tidy
    select(var, t, estimate, conf.low, conf.high) %>%
    # add in the -1 data which is ommited from the model for plotting
    bind_rows(
      tibble(
        var = depvars,
        t = rep(-1, length(depvars)),
        estimate = rep(0, length(depvars)),
        conf.low = rep(0, length(depvars)),
        conf.high = rep(0, length(depvars))
      )
    ) %>%
    # add in identifying information
    mutate(law = law,
           covs = covtype)
}

# run the three sets of BC models and save
bcdata <- bind_rows(
  run_es(law = "bc", dt = data_bc, covs = "", fes = long_fes_ff),
  run_es(law = "bc", dt = data_bc, covs = short_covs, fes = long_fes_ff),
  run_es(law = "bc", dt = data_bc, covs = long_covs, fes = long_fes_ff),
)

# matrix with dependent variable names to merge in
depvar_names <- tribble(
  ~var, ~varname,
  "roa", "ROA",
  "capEx", "Capex",
  "ppegrowth", "PPE Growth",
  "assetgrowth", "Asset Growth",
  "cash", "Cash",
  "sga", "SGA Expense",
  "leverage", "Leverage"
)

# plot event study estimates
bc_es <- bcdata %>% 
  left_join(depvar_names, by = "var") %>% 
  mutate(varname = factor(varname, levels = c("ROA", "Capex", "PPE Growth", "Asset Growth",
                                              "Cash", "SGA Expense", "Leverage"))) %>%
  ggplot(aes(x = t, y = estimate)) + 
  geom_point(fill = "white", shape = 21) + geom_line() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                linetype = "longdash") + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "#800000FF") + 
  geom_vline(xintercept = -0.5,  linetype = "longdash", color = "gray") + 
  labs(y = "Estimate", x = "Years Relative to Passage") + 
  scale_x_continuous(breaks = seq(-5, 10, by = 1)) + 
  scale_y_continuous(position = "right") + 
  labs(y = "") + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        strip.background = element_rect(color = "black", linetype = 1),
        axis.text.y = element_text(hjust = 0.95)) + 
  facet_grid(vars(varname), vars(covs), scales = "free", switch = "y")

# save
ggsave(bc_es, filename = paste(dropbox, "figures/bc_es.pdf", sep = ""), dpi = 500,
       width = 7.5, height = 9)

### Now make the plot for pp laws
# run the three sets of PP models and save
ppdata <- bind_rows(
  run_es(law = "pp", dt = data_pp, covs = "", fes = long_fes_ff),
  run_es(law = "pp", dt = data_pp, covs = short_covs, fes = long_fes_ff),
  run_es(law = "pp", dt = data_pp, covs = long_covs, fes = long_fes_ff),
)

# plot event study estimates
pp_es <- ppdata %>% 
  left_join(depvar_names, by = "var") %>% 
  mutate(varname = factor(varname, levels = c("ROA", "Capex", "PPE Growth", "Asset Growth",
                                              "Cash", "SGA Expense", "Leverage"))) %>%
  ggplot(aes(x = t, y = estimate)) + 
  geom_point(fill = "white", shape = 21) + geom_line() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                linetype = "longdash") + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "#800000FF") + 
  geom_vline(xintercept = -0.5,  linetype = "longdash", color = "gray") + 
  labs(y = "Estimate", x = "Years Relative to Passage") + 
  scale_x_continuous(breaks = seq(-5, 10, by = 1)) + 
  scale_y_continuous(position = "right") + 
  labs(y = "") + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        strip.background = element_rect(color = "black", linetype = 1),
        axis.text.y = element_text(hjust = 0.95)) + 
  facet_grid(vars(varname), vars(covs), scales = "free", switch = "y")

# save
ggsave(pp_es, filename = paste(dropbox, "figures/pp_es.pdf", sep = ""), dpi = 500,
       width = 7.5, height = 9)

### Finally, redo the estimates with the KW data
# make datasets for BC and pp laws
data_bc <- make_dt(data1, bc)
data_pp <- make_dt(data1, pp)

# run the three sets of BC models and save
bcdata <- bind_rows(
  run_es(law = "bc", dt = data_bc, covs = "", fes = long_fes),
  run_es(law = "bc", dt = data_bc, covs = short_covs, fes = long_fes),
  run_es(law = "bc", dt = data_bc, covs = long_covs, fes = long_fes),
)

# plot event study estimates
figB1 <- bcdata %>% 
  left_join(depvar_names, by = "var") %>% 
  mutate(varname = factor(varname, levels = c("ROA", "Capex", "PPE Growth", "Asset Growth",
                                              "Cash", "SGA Expense", "Leverage"))) %>%
  ggplot(aes(x = t, y = estimate)) + 
  geom_point(fill = "white", shape = 21) + geom_line() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                linetype = "longdash") + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "#800000FF") + 
  geom_vline(xintercept = -0.5,  linetype = "longdash", color = "gray") + 
  labs(y = "Estimate", x = "Years Relative to Passage") + 
  scale_x_continuous(breaks = seq(-5, 10, by = 1)) + 
  scale_y_continuous(position = "right") + 
  labs(y = "") + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        strip.background = element_rect(color = "black", linetype = 1),
        axis.text.y = element_text(hjust = 0.95)) + 
  facet_grid(vars(varname), vars(covs), scales = "free", switch = "y")

# save
ggsave(figB1, filename = paste(dropbox, "figures/figB1.pdf", sep = ""), dpi = 500,
       width = 7.5, height = 9)

# run the three sets of PP models and save
ppdata <- bind_rows(
  run_es(law = "pp", dt = data_pp, covs = "", fes = long_fes),
  run_es(law = "pp", dt = data_pp, covs = short_covs, fes = long_fes),
  run_es(law = "pp", dt = data_pp, covs = long_covs, fes = long_fes),
)

# plot event study estimates
figB2 <- ppdata %>% 
  left_join(depvar_names, by = "var") %>% 
  mutate(varname = factor(varname, levels = c("ROA", "Capex", "PPE Growth", "Asset Growth",
                                              "Cash", "SGA Expense", "Leverage"))) %>%
  ggplot(aes(x = t, y = estimate)) + 
  geom_point(fill = "white", shape = 21) + geom_line() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                linetype = "longdash") + 
  geom_hline(yintercept = 0,  linetype = "longdash", color = "#800000FF") + 
  geom_vline(xintercept = -0.5,  linetype = "longdash", color = "gray") + 
  labs(y = "Estimate", x = "Years Relative to Passage") + 
  scale_x_continuous(breaks = seq(-5, 10, by = 1)) + 
  scale_y_continuous(position = "right") + 
  labs(y = "") + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        strip.background = element_rect(color = "black", linetype = 1),
        axis.text.y = element_text(hjust = 0.95)) + 
  facet_grid(vars(varname), vars(covs), scales = "free", switch = "y")

# save
ggsave(figB2, filename = paste(dropbox, "figures/figB2.pdf", sep = ""), dpi = 500,
       width = 7.5, height = 9)
