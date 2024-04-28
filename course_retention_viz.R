library(tidyverse)
library(ggplot2)

df <- read_csv("../converted_data.csv") %>% 
  select(-engagement_time)

long_df<- df %>% 
  pivot_longer(
    cols = c(EOC, percentage_completed),
    names_to = "Metric",
    values_to = "value"
  ) %>% 
  mutate(value = value/100)

long_df %>%
  ggplot(aes(x=chapter_number, y=value, group=Metric, color=Metric)) +
  geom_line(size = 1.5) +
  scale_color_manual(labels = c("Quiz Score", "Completion"),
                     values = c("#68A357", "#25355A")) +
  scale_x_continuous(breaks = seq(1,13,1)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  theme_classic() +
  theme(legend.position = c(0.8, 0.8),
        text = element_text(size=24)) +
  ylab("Percent") +
  xlab("Chapter")
  