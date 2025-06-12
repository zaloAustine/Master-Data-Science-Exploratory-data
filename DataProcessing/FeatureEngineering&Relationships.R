# MRDC 911 Assignment 1: EDA and Data Preprocessing on Kenyan Student Dataset
# Instructor: JAPHETH MURSI | Date: 6th June 2025 | Due: 13th June 2025
# Load required libraries
library(tidyverse)
library(corrplot)
library(ggplot2)


data <- read_csv("/Users/AZalo/Downloads/kenya_student_data.csv")

# Question 14
#Discretize study_hours_weekly into four bins (e.g., Low, Moderate, High, VeryHigh)
#Create a bar plot of the binned variable

hours <- data$study_hours_weekly
hours_bins <- cut(
  hours,
  breaks = c(-Inf, 10, 20, 30, Inf),
  labels = c("Low", "Moderate", "High", "Very High"),
  right = FALSE
)
data$study_hours_binned <- hours_bins
df_hours <- data.frame(study_hours_binned = hours_bins)
ggplot(df_hours, aes(x = study_hours_binned)) +
  geom_bar(fill = "darkgreen", width = 0.6) +
  labs(
    title = "Binned Study Hours",
    x = "Category",
    y = "Count"
  ) +
  theme_minimal()



# Question 15
#Discretize family_income into quartiles (Low, Medium-Low, Medium-High,High).
#How does the binned variable correlate with academic_performance?

inc <- data$family_income
qs <- quantile(inc, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
inc_bins <- cut(
  inc,
  breaks = qs,
  include.lowest = TRUE,
  labels = c("Low", "Medium-Low", "Medium-High", "High")
)
data$income_binned <- inc_bins
ct <- table(inc_bins, data$academic_performance)
print(ct)
print(round(prop.table(ct, 1), 2))



# Question 16
#Create a new feature total_score by averaging math_score, science_score, and english_score.
#Visualize its distribution.

scores <- data.frame(
  math = data$math_score,
  science = data$science_score,
  english = data$english_score
)
data$total_score <- rowMeans(scores, na.rm = TRUE)

df_total <- data.frame(total_score = data$total_score)
ggplot(df_total, aes(x = total_score)) +
  geom_histogram(binwidth = 5, fill = "coral", color = "white") +
  labs(
    title = "Distribution of Total Score",
    x = "Total Score",
    y = "Count"
  ) +
  theme_minimal()

# Question 17
#reate a contingency table for extracurricular_activities
#vs.academic_performance. What patterns suggest about student involvement?

print(table(data$extracurricular_activities, data$academic_performance))


# Question 18
#visualize the relationship between study_hours_weekly and total_score (from
#Q16) using a scatter plot, colored by residency. What trends do you observe?

ggplot(data, aes(x = study_hours_weekly, y = total_score, color = residency)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Study Hours vs Total Score by Residency",
    x = "Study Hours per Week",
    y = "Total Score"
  ) +
  theme_minimal()

# Save preprocessed dataset
write.csv(data, "kenya_student_data_preprocessed.csv", row.names = FALSE)




