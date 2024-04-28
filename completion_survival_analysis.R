library(tidyverse)
library(survival)
library(survminer)
library(lme4)

raw_page <- read_csv("2024 ASA DataFest Data and Documentation-updated-2024-03-04/full_03_04/page_views.csv")
raw_pulse <- read_csv("2024 ASA DataFest Data and Documentation-updated-2024-03-04/full_03_04/checkpoints_pulse.csv")

clean_page <- raw_page %>% 
  filter(book != "College / Advanced Statistics and Data Science (ABCD)") %>% 
  select(student_id,
         institution_id,
         chapter_number,
         section_number,
         was_complete) %>% 
  group_by(student_id, institution_id, chapter_number, section_number) %>% 
  mutate(was_complete = any(was_complete)) %>%
  ungroup()

clean_page <- clean_page[!duplicated(clean_page), ] %>% 
  group_by(student_id,
           institution_id,
           chapter_number) %>% 
  mutate(was_complete = mean(was_complete) > 0.5) %>%
  ungroup() %>%
  na.omit() 

clean_page <- clean_page%>% 
  mutate(event = as.numeric(!was_complete)) %>% 
  select(-section_number,
         -was_complete)

clean_page <- clean_page[!duplicated(clean_page), ] 

clean_pluse <- raw_pulse %>% 
  filter(construct == "Expectancy",
         book != "College / Advanced Statistics and Data Science (ABCD)") %>% 
  select(student_id,
         chapter_number,
         institution_id,
         response) %>% 
  mutate(response = case_when(is.na(response) ~ FALSE,
                              TRUE ~ TRUE))

clean_full <- merge(clean_page, clean_pluse, all.x = TRUE) %>% na.omit()

fit0 <- glmer(event ~ chapter_number + response + (1|institution_id),
              family=binomial(link="logit"),nAGQ=7,data=clean_full)
summary(fit0)


fit1 <- survfit(Surv(chapter_number, event) ~ response, data = clean_full)
print(fit1)

# p-val = 0.0044

ggsurvplot(fit1,
           pval = FALSE, conf.int = FALSE,
           risk.table = FALSE, # Add risk table
           # risk.table.col = "strata", # Change risk table color by groups
           # linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           # ggtheme = theme_minimal(), # Change ggplot2 theme
           xlab = "Chapter",
           ylab = "Completion Probability",
           break.x.by = 1,
           legend = c(0.85,0.85),
           legend.title = "Completed Pulse",
           legend.labs = c("No", "Yes"),
           palette = c("#25355A", "#FF9F1C"))
  
