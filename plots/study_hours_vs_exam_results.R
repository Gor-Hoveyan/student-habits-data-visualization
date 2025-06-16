library(ggplot2)
library(dplyr)

ggplot(student_habits_performance, aes(x = study_hours_per_day, y = exam_score)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Study Hours vs. Exam Scores",
       x = "Daily Study Hours",
       y = "Exam Score") +
  coord_cartesian(ylim = c(0, 100))