library(ggplot2)
library(dplyr)

student_habits_performance %>%
  mutate(total_screen_time = social_media_hours + netflix_hours) %>%
  mutate(parental_education_level = factor(parental_education_level,
                                           levels = c("None", "High School", "Bachelor", "Master"),
                                           ordered = TRUE)) %>%

  ggplot(aes(x = study_hours_per_day, y = exam_score)) +

  geom_point(aes(color = sleep_hours, size = total_screen_time), alpha = 0.7) +
  
  geom_smooth(method = "loess", se = FALSE, color = "black", size = 1) +
  
  facet_wrap(~ parental_education_level) +
  
  scale_color_gradientn(colors = c("#fde725", "#21918c", "#440154")) +
  scale_size_continuous(range = c(1, 10)) +
  
  labs(
    title = "The Student Success Profile",
    subtitle = "Analyzing the combined effect of Study, Sleep, and Screen Time on Exam Scores",
    x = "Study Hours Per Day",
    y = "Exam Score",
    color = "Sleep Hours",
    size = "Total Screen Time (hrs)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20, color = "black"),
    plot.subtitle = element_text(hjust = 0.5, size = 13, margin = margin(b = 25), color = "gray30"),
    axis.title = element_text(face = "bold", color = "black"),
    axis.text = element_text(color = "gray30"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold", color = "black"),
    legend.text = element_text(color = "gray30"),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.major = element_line(color = "gray90", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey80", fill = NA)
  )
