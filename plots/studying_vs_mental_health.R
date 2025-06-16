library(ggplot2)
library(dplyr)

student_habits_performance %>%
  mutate(
    study_category = cut(
      study_hours_per_day,
      breaks = c(-Inf, 3, 6, Inf), 
      labels = c("Low (0-3 hrs)", "Medium (3-6 hrs)", "High (6+ hrs)"),
      right = FALSE
    )
  ) %>%

  ggplot(aes(x = study_category, y = mental_health_rating, fill = study_category)) +
  
  geom_violin(trim = FALSE, alpha = 0.6) +
  
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.5, outlier.shape = NA) +
  
  geom_jitter(width = 0.15, alpha = 0.2, size = 1) +
  
  facet_wrap(~ extracurricular_participation, labeller = labeller(extracurricular_participation =
                                                                    function(x) paste("Extracurriculars:", x))) +
  
  scale_fill_manual(values = c("#8DA0CB", "#FC8D62", "#66C2A5")) +
    labs(
    title = "Mental Health Score Distribution by Study Hours and Extracurriculars",
    subtitle = "Violin plots showing data density, with box plots for statistical summary",
    x = "Study Hours Per Day",
    y = "Mental Health Score"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 13, margin = margin(b = 25)),
    strip.text = element_text(face = "bold", size = 12, color = "white"),
    strip.background = element_rect(fill = "gray50", color = "gray50"),
    panel.grid.major = element_line(color = "gray90", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray80", fill = NA),
    axis.title = element_text(face = "bold"),
    legend.position = "none"
  )
