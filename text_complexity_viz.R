library(tidyverse)
library(ggplot2)
library(ggrepel)

df <- read_csv("../scraped_text.csv") %>% 
  select(chapter_number,
         section_number,
         `completion proportion`,
         text_fog_score,) %>% 
  rename(completion_proportion = `completion proportion`) %>% 
  mutate(label = case_when(text_fog_score > 11 ~ paste(chapter_number, ".", section_number),
                           TRUE ~ ""))

df %>%
  ggplot(aes(x=text_fog_score, y=completion_proportion)) +
  geom_point(color = "#25355A", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "#FF9F1C") +
  theme_classic() +
  ylim(c(0,0.75)) +
  theme(text = element_text(size=14)) + 
  # geom_text_repel(aes(label = label), box.padding = 2) +
  ylab("Completion Proportion") +
  xlab("Text Complexity (Fog Score)")
  