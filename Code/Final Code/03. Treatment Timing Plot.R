library(tidyverse)
library(fixest)
library(ggthemes)
library(ggsci)

# set ggplot theme
theme_set(
  theme_clean() + 
    theme(plot.background = element_blank(),
          legend.background = element_rect(color = "white"))
)

# load the Karpoff and Wittry Data
KW <- haven::read_dta(here::here("Data/KW", "maindata.dta")) %>% 
  # filter to years in our data
  filter(year %>% between(1976, 1995))

# save output folder
dropbox <- "~/Dropbox/Apps/Overleaf/Antitakeover/"

# make the plot
treatment_timing <- KW %>% 
  # keep just the columns we need
  select(year, incorp, bc, pp, cs, fp, dd) %>% 
  # pivot longer so that laws are in one column
  pivot_longer(cols = c(bc, pp, cs, fp, dd),
               names_to = "law",
               values_to = "value") %>% 
  # rename law
  mutate(law = case_when(
    law == "bc" ~ "Business Combination",
    law == "pp" ~ "Poison Pill",
    law == "cs" ~ "Control Share Acquisition",
    law == "dd" ~ "Directors' Duties",
    TRUE ~ "Fair Price"
  )) %>% 
  # get summary stats by year, law, state
  group_by(year, incorp, law) %>% 
  summarize(
    count = n(),
    sum = sum(value, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  # refactor variables, filter years, and plot
  mutate(incorp = fct_reorder(incorp, rank(desc(incorp)))) %>% 
  mutate(post = if_else(sum > 0, "Law", "No Law")) %>% 
  mutate(post = factor(post, levels = c("Law", "No Law"))) %>% 
  # plot
  ggplot(aes(x = year, y = incorp)) + 
  geom_tile(aes(fill = as.factor(post)), alpha = 1/2) + 
  scale_fill_brewer(palette = 'Set1') + 
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        axis.title = element_blank(),
        legend.background = element_rect(color = "white"),
        strip.text = element_text(size = 12),
        strip.background = element_rect(linetype = 1, linewidth = 1, color = "black", fill = "white"),
        legend.key = element_rect(fill = "white", colour = NA)) + 
  facet_wrap(~law)

# save the plot
ggsave(treatment_timing, filename = paste(dropbox, "figures/treatment_timing.pdf", sep = ""), dpi = 500,
       width = 10, height = 10)
