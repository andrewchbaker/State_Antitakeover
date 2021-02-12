library(tidyverse)
library(haven)
library(kableExtra)
options(knitr.kable.NA = '')
# make folder to save output to for overleaf
dropbox <- ""

# Download enactment dates from the KW file
enact <- haven::read_dta(here::here("Data/KW", "Enactment Dates.dta"))

# table to merge in name
names <- tibble(
  incorp = state.abb,
  state = state.name
)

# make table
tableA1 <- enact %>% 
  # merge in state names
  left_join(names, by = "incorp") %>% 
  # keep  just the state and the dates that wee need
  select(state, cs_date, bc_date, fp_date, dd_date, pp_date) %>% 
  # make and report table
  kable("latex", align = 'lccccc', booktabs = T, longtable = T,
        label = "tableA1", 
        caption = "Second-Generation State Antitakeover Laws",
        col.names = linebreak(c("State", "CS", "BC", "FP", "DD", "PP"))) %>% 
  kable_styling(latex_options = c("repeat_header", "HOLD_position", "striped"), 
                font_size = 11, full_width = T)

# save
write_lines(tableA1, file = paste(dropbox, "tableA1.tex", sep = ""))