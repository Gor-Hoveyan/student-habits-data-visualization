library(ggplot2)
library(dplyr)

student_habits_performance %>%
  ggplot(aes(x = study_hours_per_day, y = attendance_percentage, color = part_time_job)) +
  
  geom_jitter(alpha = 0.4, width = 0.2) +
  
  geom_smooth(method = "lm", se = FALSE, aes(group = part_time_job), size = 1.5) +
  
  facet_wrap(~ part_time_job) +
  
  scale_color_manual(values = c("Yes" = "#fd8d3c", "No" = "#bdd7e7")) +
  
  labs(
    title = "How Part-Time Work Affects Study Hours and Attendance",
    subtitle = "Comparing the relationship between study and attendance for working and non-working students",
    x = "Study Hours Per Day",
    y = "Class Attendance (%)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 13, margin = margin(b = 25)),
    axis.title = element_text(face = "bold"),
    legend.position = "none", 
    strip.text = element_text(face = "bold", size = 12)
  )
