library(ggplot2)
library(dplyr)
library(tidyr)

archetype_data <- student_habits_performance %>%
  mutate(
    diet_score = case_when(
      diet_quality == "Poor"    ~ 1,
      diet_quality == "Average" ~ 2,
      diet_quality == "Good"    ~ 3
    ),
    wellness_score = scale(sleep_hours) + scale(diet_score) + scale(exercise_frequency),
    total_screen_time = social_media_hours + netflix_hours
  ) %>%
  filter(is.finite(wellness_score))

clustering_data <- archetype_data %>%
  select(wellness_score, study_hours_per_day, total_screen_time)

set.seed(123)
kmeans_result <- kmeans(clustering_data, centers = 4, nstart = 25)

archetype_data$cluster <- as.factor(kmeans_result$cluster)

archetype_summary <- archetype_data %>%
  group_by(cluster) %>%
  summarise(
    avg_wellness = mean(wellness_score),
    avg_study = mean(study_hours_per_day),
    avg_screen = mean(total_screen_time)
  )

# --- FIX: Use data-driven medians for naming to ensure 4 unique archetypes ---
median_wellness_threshold <- median(archetype_summary$avg_wellness)
median_study_threshold <- median(archetype_summary$avg_study)

archetype_summary <- archetype_summary %>%
  mutate(archetype_name = paste0(
    ifelse(avg_wellness > median_wellness_threshold, "High-Wellness", "Low-Wellness"), ", ",
    ifelse(avg_study > median_study_threshold, "High-Study", "Low-Study")
  ))

archetype_data <- archetype_data %>%
  left_join(select(archetype_summary, cluster, archetype_name), by = "cluster")

ggplot(archetype_data, aes(x = wellness_score, y = study_hours_per_day)) +
  geom_point(aes(color = exam_score, size = total_screen_time), alpha = 0.7) +
  facet_wrap(~ archetype_name) +
  scale_color_gradientn(colors = c("#e66101", "#fdb863", "#b2abd2", "#5e3c99")) +
  scale_size_continuous(range = c(1, 12)) +
  labs(
    title = "Student Archetypes: A Profile of Habits and Success",
    subtitle = "Clusters of students based on wellness, study, and screen time habits",
    x = "Combined Wellness Score (Sleep, Diet, Exercise)",
    y = "Study Hours Per Day",
    color = "Exam Score",
    size = "Total Screen Time"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    plot.subtitle = element_text(hjust = 0.5, size = 13, margin = margin(b = 25)),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal",
    strip.text = element_text(face = "bold", size = 11)
  )
