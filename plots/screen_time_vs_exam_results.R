library(ggplot2)
library(dplyr)

student_habits_performance %>%
  
  mutate(total_screen_time = social_media_hours + netflix_hours) %>%
  
  mutate(
    screen_time_category = cut(
      total_screen_time,
      breaks = c(-Inf, 4, 8, Inf),
      labels = c("Low (0-4 hrs)", "Medium (4-8 hrs)", "High (8+ hrs)"),
      right = FALSE
    )
  ) %>%
  
  ggplot(aes(
    x = screen_time_category,
    y = exam_score,
    fill = screen_time_category
  )) +
  geom_boxplot(alpha = 0.8) +
  labs(
    title = "Distribution of Exam Scores by Screen Time",
    subtitle = "Comparing performance across different daily screen time habits",
    x = "Total Screen Time Category",
    y = "Exam Score"
  ) + theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold")
  )
