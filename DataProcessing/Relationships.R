# MRDC 911 Assignment 1: EDA and Data Preprocessing on Kenyan Student Dataset
# Instructor: JAPHETH MURSI | Date: 6th June 2025 | Due: 13th June 2025
# Load required libraries
library(tidyverse)
library(corrplot)
library(ggplot2)

# Question 1
data <- read_csv("/Users/AZalo/Downloads/kenya_student_data.csv")
#    - str() shows each column’s type and first values
#    - glimpse() is a dplyr-friendly overview
#    - head() prints the first 6 rows
str(data) 
glimpse(data)
head(data)

is_numeric <- sapply(data, is.numeric) # loop through the data and find which on is numeric
numeric_vars   <- names(data)[ is_numeric ] # loop through all names where values is numeric
categorical_vars <- names(data)[ !is_numeric ] # loop through all names where values is not numeric


#Print counts and names
cat("Numeric variables (", length(numeric_vars), "):\n", paste(numeric_vars, collapse=", "), "\n\n")
cat("Categorical variables (", length(categorical_vars), "):\n", paste(categorical_vars, collapse=", "), "\n")



# Question 2
#Identify numeric columns
numeric_data    <- data[ , is_numeric, drop = FALSE]

#Compute summary stats via sapply over each column
var_names <- names(numeric_data)

count_vals   <- sapply(numeric_data, function(x) sum(!is.na(x)))
missing_vals <- sapply(numeric_data, function(x) sum(is.na(x)))
mean_vals    <- sapply(numeric_data, function(x) mean(x, na.rm = TRUE))
median_vals  <- sapply(numeric_data, function(x) median(x, na.rm = TRUE))
min_vals     <- sapply(numeric_data, function(x) min(x, na.rm = TRUE))
max_vals     <- sapply(numeric_data, function(x) max(x, na.rm = TRUE))
sd_vals      <- sapply(numeric_data, function(x) sd(x, na.rm = TRUE))

#Assemble into one data.frame
summary_stats <- data.frame(
  variable = var_names,
  count    = count_vals,
  missing  = missing_vals,
  mean     = round(mean_vals, 2),
  median   = round(median_vals, 2),
  min      = round(min_vals,  2),
  max      = round(max_vals,  2),
  sd       = round(sd_vals,   2),
  row.names = NULL,
  stringsAsFactors = FALSE
)
print(summary_stats)



# Question 3
#Compute counts and proportions 
perf_table <- table(data$academic_performance)

#Convert to a data.frame for plotting
perf_df <- data.frame(
  performance = names(perf_table),
  count       = as.integer(perf_table),
  stringsAsFactors = FALSE
)
perf_df$prop <- perf_df$count / sum(perf_df$count)

#Bar plot of counts vs academic performance
ggplot(perf_df, aes(x = performance, y = count)) +
  geom_col(fill = "steelblue", width = 0.6) +
  geom_text(aes(label = count), vjust = -0.5) +
  labs(
    title = "Distribution of Academic Performance",
    x     = "Performance Category",
    y     = "Number of Students"
  ) +
  theme_minimal()



# Question 4
#Create a faceted histogram
ggplot(data, aes(x = study_hours_weekly)) +
  geom_histogram(
    binwidth = 5,           # each bar spans 5 hours
    fill     = "skyblue",   # bar fill color
    color    = "white"      # bar border color
  ) +
#Split into one panel per residency value
  facet_wrap(~ residency) +
# Add titles and axis labels
  labs(
    title = "Weekly Study Hours: Urban vs. Rural Students",
    x     = "Study Hours per Week",
    y     = "Number of Students"
  ) +
  # d) Apply a clean theme
  theme_minimal()


# Question 5
#Build the boxplot
ggplot(data, aes(
  x    = academic_performance,
  y    = math_score,
  fill = gender
)) +
  geom_boxplot() +
  labs(
    title = "Math Score by Academic Performance and Gender",
    x     = "Academic Performance",
    y     = "Math Score"
  ) +
  theme_minimal()


#Question 6 
#Proportion Extracurricular activities proportions
extra_tab        <- table(data$extracurricular_activities)
extra_prop       <- prop.table(extra_tab)                   # proportions
extra_prop_desc  <- sort(extra_prop, decreasing = TRUE)     # sort high → low
print(extra_prop_desc)

#Proportion Faculty proportions
fac_tab         <- table(data$faculty)
fac_prop        <- prop.table(fac_tab)
fac_prop_desc   <- sort(fac_prop, decreasing = TRUE)
print(fac_prop_desc)

#Question 7
#Extract numeric columns (exclude student_id if present)
is_num      <- sapply(data, is.numeric)
if ("student_id" %in% names(data)) is_num["student_id"] <- FALSE
num_data    <- data[ , is_num, drop = FALSE]

#Compute correlation matrix (pairwise complete observations)
corr_mat <- cor(num_data, use = "pairwise.complete.obs")

#Visualize with a heatmap
corrplot(corr_mat,
         method = "color",    # colored squares
         type   = "upper",    # upper triangle only
         tl.cex = 0.8,        # text label size
         addCoef.col = "black", # show correlation values
         number.cex = 0.7)




