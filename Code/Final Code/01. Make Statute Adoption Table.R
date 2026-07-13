library(tidyverse)
library(haven)
library(kableExtra)
options(knitr.kable.NA = '')
# set dropbox location
dropbox <- "~/Dropbox/Apps/Overleaf/Antitakeover/"

# Download enactment dates from the Karpoff-Wittry file
enact <- haven::read_dta(here::here("Data/KW", "Enactment Dates.dta"))

# smaller table to merge in names of states
names <- tibble(
  incorp = state.abb,
  state = state.name
)

# make the enactment table
enactment_table <- enact %>% 
  # merge in state names
  left_join(names, by = "incorp") %>% 
  # keep  just the state and the dates that wee need
  select(state, cs_date, bc_date, fp_date, dd_date, pp_date) %>% 
  # make and report table
  kable("latex", align = 'lccccc', booktabs = T, longtable = T,
        label = "enactment_table", 
        caption = " Second-Generation State Antitakeover Laws",
        linesep = "",
        col.names = linebreak(c("State", "CS", "BC", "FP", "DD", "PP"))) %>% 
  kable_styling(latex_options = c("repeat_header", "HOLD_position"), 
                font_size = 11, full_width = FALSE) %>% 
  column_spec(1, width = "4cm", latex_valign = "m") %>%
  column_spec(2:6, width = "2.5cm",
              latex_valign = "m")

# save the table to dropbox
write_lines(enactment_table, file = paste(dropbox, "tables/enactment_table.tex", sep = ""))