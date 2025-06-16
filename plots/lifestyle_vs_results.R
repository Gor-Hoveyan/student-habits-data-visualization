library(ggplot2)
library(dplyr)
library(tools)

student_habits_performance %>%
  mutate(diet_quality = toTitleCase(as.character(diet_quality))) %>%
  
  mutate(
    exercise_category = cut(
      exercise_frequency,
      breaks = c(-Inf, 1, 3, Inf), 
      labels = c("Rarely (0-1/wk)", "Sometimes (2-3/wk)", "Regularly (4+/wk)"),
      right = TRUE 
    )
  ) %>%
  
  ggplot(aes(x = sleep_hours, y = exam_score)) +
  

  geom_point(alpha = 0.4, color = "darkcyan") +
  
  geom_smooth(method = "lm", se = FALSE, color = "tomato") +
  
  facet_grid(
    factor(diet_quality, levels = c("Poor", "Fair", "Good")) ~ exercise_category
  ) +
  
  labs(
    title = "Impact of Lifestyle Habits on Exam Scores",
    subtitle = "Relationship between Sleep Hours and Exam Score, grouped by Diet and Exercise",
    x = "Sleep Hours per Night",
    y = "Exam Score"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 20)),
    strip.text = element_text(face = "bold", size = 10), 
    panel.border = element_rect(color = "grey80", fill = NA),
    panel.spacing = unit(1, "lines") 
  )
