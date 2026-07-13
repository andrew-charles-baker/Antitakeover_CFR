library(tidyverse)
library(fixest)
library(furrr)

# set file path
dropbox <- "~/Dropbox/Apps/Overleaf/Antitakeover/"

# set ggplot theme
theme_set(
  theme_clean() + 
    theme(plot.background = element_blank(),
          legend.background = element_rect(color = "white"))
)


# load the Karpoff and Wittry Data
KW <- haven::read_dta(here::here("Data/KW", "maindata.dta")) %>% 
  # filter years to between 76 and 95
  filter(year %>% between(1976, 1995))

# run the models two ways and save the estimates in a tibble
# covariates for short regression
cov1 <- c("bc", "size", "age", "size2", "age2")

# covariates for long regression
cov2 <- c("bc", "size", "age", "size2", "age2", "gen1", "pp", "cs", 
          "dd", "fp", "csXcts", "bcXamanda", "bcXmotivatingfirmbc")

# function to run model by dependent variable and keep 
maketable <- function(depvar) {
  
  # estimate the models with both covariate sets
  mod1 <-feols(.[depvar] ~ .[cov1] | firm + state_year +  industry_year,
               data = KW, cluster = ~incorporation)
  mod2 <- feols(.[depvar] ~ .[cov2] | firm + state_year +  industry_year,
                data = KW, cluster = ~incorporation)
  
  # combine the data estimates together and report the table
  bind_rows(
    broom::tidy(mod1, conf.int = TRUE) %>% mutate(mod = "Short", var = depvar),
    broom::tidy(mod2, conf.int = TRUE) %>% mutate(mod = "Long", var = depvar)) %>% 
    # grab just the variables and observations that we need
    filter(term == "bc") %>% 
    select(estimate, conf.low, conf.high, mod, var)
}

# put dependent variables in a vector and make table
depvar <- c("roa", "capEx", "ppegrowth", "assetgrowth", "cash", "sga", "leverage")

# standardization function
standardize <- function(x) (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)

# standardize the dependent variables for comparison purposes
KW <- KW %>% 
  mutate_at(vars(depvar), standardize)

# make table by vectorizing over dependent variables
short_long_out <- map_dfr(depvar, maketable)

# make bootstrap function to run models based on dependent variable and covariate set 
runmod_boot <- function(...) {
  
  # get random weights to do weighted bootstrap
  weights <- rexp(nrow(KW), rate = 1)
  
  # make function to get diff in coefficients between the two models
  get_mod_est_diff <- function(depvar) {
    
    # get estimates using weighted bootstrap
    mod1 <- feols(.[depvar] ~ .[cov1] | firm + state_year + industry_year, data = KW, 
                  weights = weights, cluster = ~incorporation)
    mod2 <- feols(.[depvar] ~ .[cov2] | firm + state_year + industry_year, data = KW, 
                  weights = weights, cluster = ~incorporation)  
    
    # save the difference in the two bc law variable in a dataset
    out = tibble(mod2$coefficients[1] - mod1$coefficients[1])
    names(out) <- depvar
    out
  }
  
  # estimate over the dependent variables
  map_dfc(depvar, get_mod_est_diff)
}

# set seed 
set.seed(20230630)

# parallelize and bootstrap
plan(multisession, workers = 18)

boot_out <- future_map_dfr(1:1000, .f = runmod_boot)

# get the median and center 95% confidence interval
boot_summary <- boot_out %>% 
  summarize_all(.funs = c(median = median,
                          q025 = ~quantile(., 0.025),
                          q975 = ~quantile(., 0.975),
                          q05 = ~quantile(., 0.05),
                          q95 = ~quantile(., 0.95))) %>% 
  # pivot longer
  pivot_longer(
    cols = everything(),
    names_to = c("var", ".value"),
    names_sep = "_") %>% 
  mutate(mod = "Difference") %>% 
  select(estimate = median,
         conf.low.025 = q025,
         conf.high.975 = q975,
         conf.low.05 = q05,
         conf.high.95 = q95,
         mod, var)

# merge in bootstrap differences to the short and long regression model 
plot_data <- bind_rows(short_long_out, boot_summary)

# dataset for dependent variable names
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

# make plot
model_comparison_plot <- plot_data %>% 
  left_join(depvar_names, by = "var") %>% 
  # refactor name so they stay in order
  mutate(varname = factor(varname, levels = rev(c("ROA", "Capex", "PPE Growth", "Asset Growth",
                                                  "Cash", "SGA Expense", "Leverage"))),
         mod = factor(mod, levels = c("Short", "Long", "Difference"))) %>%
  # make two sets of confidence intervals (95% (short) and 90% (long))
  mutate(conf_low_short = if_else(mod %in% c("Short", "Long"), conf.low, conf.low.05),
         conf_high_short = if_else(mod %in% c("Short", "Long"), conf.high, conf.high.95),
         conf_low_long = if_else(mod %in% c("Short", "Long"), conf.low, conf.low.025),
         conf_high_long = if_else(mod %in% c("Short", "Long"), conf.high, conf.high.975)) %>% 
  # plot
  ggplot() +
  geom_pointrange(aes(x = varname, y = estimate, ymin = conf_low_long, ymax = conf_high_long),
                  color = "darkred", linewidth = 1.5) + 
  geom_pointrange(aes(x = varname, y = estimate, ymin = conf_low_short, ymax = conf_high_short),
                  color = "#767676FF", linewidth = 1) + 
  geom_hline(yintercept = 0, color = "black", linetype = "longdash", 
             linewidth = 1) + 
  labs(x = "", y = "") + 
  coord_flip() + 
  facet_wrap(~mod, scales = "free_x", nrow = 1) + 
  theme(axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        axis.text = element_text(size = 14),
        strip.text = element_text(hjust = 0.5, size = 20),
        panel.border = element_rect(color = "black", fill = NA),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        strip.background = element_rect(linetype = 1, size = 1, fill = "white", color = "black")) +
  guides(x = guide_axis(n.dodge = 2))

# save plot
ggsave(model_comparison_plot, filename = paste(dropbox, "figures/model_comparison_plot.pdf", sep = ""), dpi = 500,
       width = 12, height = 6)
