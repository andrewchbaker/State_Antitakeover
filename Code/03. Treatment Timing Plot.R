library(tidyverse)
library(fixest)
library(ggthemes)
library(fastDummies)
library(ggsci)

# set ggplot theme
theme_set(
  theme_clean() + 
    theme(plot.background = element_blank(),
          legend.background = element_rect(color = "white"))
)


# load the Karpoff and Wittry Data
KW <- haven::read_dta(here::here("Data/KW", "maindata.dta"))

# save output folder
dropbox <- ""

# plot
fig2 <- KW %>% 
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
    sum = sum(value)
  ) %>% 
  # refactor variables, filter years, and plot
  mutate(incorp = fct_reorder(incorp, rank(desc(incorp)))) %>% 
  mutate(post = if_else(sum > 0, "Law", "No Law")) %>% 
  mutate(post = factor(post, levels = c("Law", "No Law"))) %>% 
  filter(year %>% between(1976, 1995)) %>% 
  ggplot(aes(x = year, y = incorp)) + 
  geom_tile(aes(fill = as.factor(post)), alpha = 3/4) + 
  scale_fill_manual(values = c("#800000", "white")) +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        legend.background = element_rect(color = "white"),
        strip.text = element_text(size = 12),
        legend.key = element_rect(fill = "white", colour = "black")) + 
  facet_wrap(~law)

# save
ggsave(fig2, filename = paste(dropbox, "fig2.png", sep = ""), dpi = 500,
       width = 10, height = 10)
