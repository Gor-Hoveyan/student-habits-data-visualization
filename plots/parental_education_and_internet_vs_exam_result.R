library(ggplot2)
library(dplyr)

student_habits_performance %>%
  group_by(parental_education_level, internet_quality, gender) %>%
  summarise(average_exam_score = mean(exam_score), .groups = 'drop') %>%
  complete(parental_education_level, internet_quality, gender) %>%
  ggplot(aes(
    x = factor(parental_education_level, levels = c( "None", "High School", "Bachelor", "Master")),
    y = factor(internet_quality, levels = c("Poor", "Average", "Good")),
    fill = average_exam_score
  )) +
  geom_tile(color = "white") +
  geom_text(aes(label = ifelse(is.na(average_exam_score), "N/A", round(average_exam_score, 1)))) +
  labs(
    title = "Ghost in the Machine",
    x = NULL,
    y = NULL,
    fill = "Avg. Score"
  ) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey90") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  facet_wrap(~ gender)
