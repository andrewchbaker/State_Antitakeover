library(tidyverse)
library(kableExtra)
library(ggsci)
library(ggthemes)
library(patchwork)
library(lfe)
library(fastDummies)

# set ggplot theme
theme_set(
  theme_clean() + 
    theme(plot.background = element_blank(),
          legend.background = element_rect(color = "white"))
)

# save output folder
dropbox <- ""

# Panel A - no pretrends looking like chef's kiss
data <- tibble(
  time = rep(-3:3, 2),
  circuit = c(rep("7th Circuit", 7), rep("9th Circuit", 7)),
  cases = c(rep(50, 3), seq(55, 70, by = 5),
            rep(30, 3), seq(40, 70, by = 10))
) %>% 
  mutate(treat = if_else(circuit == "9th Circuit" & time >= 0, 1, 0),
         tt = if_else(circuit == "9th Circuit", time, NA_integer_)) %>% 
  dummy_cols(select_columns = "tt", remove_selected_columns = FALSE, ignore_na = TRUE) %>% 
  mutate(across(starts_with("tt_"), ~replace_na(., 0)))
  

# confirm treatment effect = 12.5
felm(cases ~ treat | circuit + time | 0 | 0, data = data)

# plot count
fig4a <- data %>% 
  ggplot(aes(x = time, y = cases, group = circuit, color = circuit, shape = circuit)) + 
  geom_point() + geom_line() + 
  labs(x = "Time Period", y = "Case \n Count") + 
  scale_color_uchicago(palette = 'dark') +
  scale_x_continuous(breaks = c(-3:3)) + 
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  ggtitle('(1) \n \n Case Trends') + 
  theme(legend.position = "bottom",
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) 

# plot treatment effect
# first estimate model
fig4b <- felm(cases ~ `tt_-3` + `tt_-2` + tt_0 + tt_1 + tt_2 + tt_3 | 
       circuit + time | 0 | 0, data = data) %>% 
  broom::tidy() %>% 
  mutate(t = c(-3:-2, 0:3)) %>% 
  select(t, estimate) %>% 
  bind_rows(tibble(t = -1, estimate = 0)) %>% 
  ggplot(aes(x = t, y = estimate)) + 
  geom_line(color = "#800000FF", size = 2) + 
  labs(x = "Time Period", y = expression(Delta)) + 
  scale_color_uchicago(palette = 'dark') +
  scale_x_continuous(breaks = c(-3:3)) + 
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  ggtitle('Differences in \n Case Trends') + 
  theme(legend.position = "bottom",
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360, size = 14),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

# Panel B - all prior trends, no difference afterwards 
data <- tibble(
  time = rep(-3:3, 2),
  circuit = c(rep("7th Circuit", 7), rep("9th Circuit", 7)),
  cases = c(45, 50, 55, rep(60, 4),
            26.25, 37.5, 48.75, rep(60, 4))
) %>% 
  mutate(treat = if_else(circuit == "9th Circuit" & time >= 0, 1, 0),
         tt = if_else(circuit == "9th Circuit", time, NA_integer_)) %>% 
  dummy_cols(select_columns = "tt", remove_selected_columns = FALSE, ignore_na = TRUE) %>% 
  mutate(across(starts_with("tt_"), ~replace_na(., 0)))

# confirm treatment effect = 12.5
felm(cases ~ treat | circuit + time | 0 | 0, data = data)

# plot counts
fig4c <- data %>% 
  ggplot(aes(x = time, y = cases, group = circuit, color = circuit, shape = circuit)) + 
  geom_point() + geom_line() + 
  labs(x = "Time Period", y = "Case \n Count") + 
  scale_color_uchicago(palette = 'dark') +
  scale_x_continuous(breaks = c(-3:3)) + 
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  ggtitle('(2) \n \n Case Trends') + 
  theme(legend.position = "bottom",
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

# plot treatment effect
# estimate model
fig4d <- felm(cases ~ `tt_-3` + `tt_-2` + tt_0 + tt_1 + tt_2 + tt_3 | 
                circuit + time | 0 | 0, data = data) %>% 
  broom::tidy() %>% 
  mutate(t = c(-3:-2, 0:3)) %>% 
  select(t, estimate) %>% 
  bind_rows(tibble(t = -1, estimate = 0)) %>% 
  ggplot(aes(x = t, y = estimate)) + 
  geom_line(color = "#800000FF", size = 2) + 
  labs(x = "Time Period", y = expression(Delta)) + 
  scale_color_uchicago(palette = 'dark') +
  scale_x_continuous(breaks = c(-3:3)) + 
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  ggtitle('Differences in \n Case Trends') + 
  theme(legend.position = "bottom",
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360, size = 14),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

# Panel C - No break after ruling but different trends
data <- tibble(
  time = rep(-3:3, 2),
  circuit = c(rep("7th Circuit", 7), rep("9th Circuit", 7)),
  cases = c(seq(50 - 20/7, 50 + 5*20/7, length.out = 7),
            seq(25.5 - 45/7, 25.5 + 5*45/7, length.out = 7))
) %>% 
  mutate(treat = if_else(circuit == "9th Circuit" & time >= 0, 1, 0),
         tt = if_else(circuit == "9th Circuit", time, NA_integer_)) %>% 
  dummy_cols(select_columns = "tt", remove_selected_columns = FALSE, ignore_na = TRUE) %>% 
  mutate(across(starts_with("tt_"), ~replace_na(., 0)))

# confirm treatment effect = 12.5
felm(cases ~ treat | circuit + time | 0 | 0, data = data)

# plot data
fig4e <- data %>% 
  ggplot(aes(x = time, y = cases, group = circuit, color = circuit, shape = circuit)) + 
  geom_point() + geom_line() + 
  labs(x = "Time Period", y = "Case \n Count") + 
  scale_color_uchicago(palette = 'dark') +
  scale_x_continuous(breaks = c(-3:3)) + 
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  ggtitle('(3) \n \n Case Trends') + 
  theme(legend.position = "bottom",
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

# plot treatment effect
# estimate model
fig4f <- felm(cases ~ `tt_-3` + `tt_-2` + tt_0 + tt_1 + tt_2 + tt_3 | 
                circuit + time | 0 | 0, data = data) %>% 
  broom::tidy() %>% 
  mutate(t = c(-3:-2, 0:3)) %>% 
  select(t, estimate) %>% 
  bind_rows(tibble(t = -1, estimate = 0)) %>% 
  ggplot(aes(x = t, y = estimate)) + 
  geom_line(color = "#800000FF", size = 2) + 
  labs(x = "Time Period", y = expression(Delta)) + 
  scale_color_uchicago(palette = 'dark') +
  scale_x_continuous(breaks = c(-3:3)) + 
  geom_vline(xintercept = -0.5, linetype = "dashed") + 
  ggtitle('Differences in \n Case Trends') + 
  theme(legend.position = "bottom",
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360, size = 14),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))

# combine plots and save
fig4 <- (fig4a + fig4c + fig4e) / (fig4b + fig4d + fig4f)

ggsave(fig4, filename = paste(dropbox, "fig4.png", sep = ""), dpi = 500,
       width = 12, height = 6)
