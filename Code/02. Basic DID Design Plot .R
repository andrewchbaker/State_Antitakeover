library(tidyverse)
library(kableExtra)
library(ggsci)
library(ggthemes)
library(patchwork)

# set ggplot theme
theme_set(
  theme_clean() + 
    theme(plot.background = element_blank(),
          legend.background = element_rect(color = "white"))
)
# save output folder
dropbox <- ""

# Make the main dataset
data <- tribble(
  ~Circuit, ~Time, ~Cases,
  "7th Circuit", 0, 50,
  "9th Circuit", 0, 20,
  "7th Circuit", 1, 75,
  "9th Circuit", 1, 60)

# Make plot 1 - change in trends over time
plot1 <- data %>% 
  ggplot(aes(x = Time, y = Cases, group = Circuit, color = Circuit)) + 
  geom_point() + 
  geom_line() + 
  geom_label(aes(label = Cases), hjust = 0.5, vjust = -0.5, color = "black") + 
  labs(x = "Time Period", y = "Case \n Count") + 
  scale_color_uchicago(palette = 'dark') +
  ylim(c(0, 80)) + 
  scale_x_continuous(breaks = c(0, 1), 
                     labels = c("Pre", "Post")) + 
  ggtitle('Panel A') + 
  theme(legend.position = "bottom",
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) 

# Make plot 2 - remove baseline differences
# second dataset which removes baseline differences
data_fd <- data %>% 
  group_by(Circuit) %>% 
  mutate(Cases = Cases - Cases[which(Time == 0)])

# make plot
plot2 <- data %>% 
  ggplot(aes(x = Time, y = Cases, group = Circuit, color = Circuit)) + 
  geom_point(alpha = 1/2) + 
  geom_line(alpha = 1/2, linetype = "dashed") + 
  geom_point(data = data_fd, aes(x = Time, y = Cases, group = Circuit, color = Circuit)) +
  geom_line(data = data_fd, aes(x = Time, y = Cases, group = Circuit, color = Circuit)) +
  annotate("label", x = 1, y = 40, label = "40") + 
  annotate("label", x = 1, y = 25, label = "25") + 
  labs(x = "Time Period", y = "Case \n Count") + 
  scale_color_uchicago(palette = 'dark') +
  ylim(c(0, 80)) + 
  scale_x_continuous(breaks = c(0, 1), 
                     labels = c("Pre", "Post")) + 
  ggtitle('Panel B') + 
  theme(legend.position = "bottom",
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360),
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) 

# Make plot 3 - difference in differences
# dataset with the first differences
data_dd <- tribble(
  ~Circuit, ~Diff,
  "7th Circuit", 25, 
  "9th Circuit", 40
)

# plot
plot3 <- data_dd %>% 
  ggplot(aes(x = Circuit, y = Diff, group = Circuit, fill = Circuit)) + 
  geom_col() + 
  scale_fill_uchicago(palette = 'dark') +
  ylim(0, 50) + 
  geom_label(aes(label = Diff), fill = "white", vjust = -0.2) + 
  annotate("segment", x = 2, xend = 2, y = 25, yend = 40, color = "white", 
           arrow = arrow(length = unit(0.1, "inches"))) +
  annotate("segment", x = 2, xend = 2, y = 40, yend = 25, color = "white", 
           arrow = arrow(length = unit(0.1, "inches"))) +
  annotate("segment", x = 1.5, xend = 2.5, y = 25, yend = 25, color = "white") +
  annotate("label", x = 2, y = 33, label = "Treatment Effect \n = 15") + 
  ggtitle('Panel C') + 
  labs(y = expression(Delta), x = "") + 
  theme(legend.position = "bottom",
        axis.title.y = element_text(hjust = 0.5, vjust = 0.5, angle = 360, size = 14),
        legend.title = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5))
  
  
# combine plots and save figure
fig1 <- plot1 + plot2 + plot3

ggsave(fig1, filename = paste(dropbox, "fig1.png", sep = ""), dpi = 500,
       width = 12, height = 5)