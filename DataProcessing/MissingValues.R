# MRDC 911 Assignment 1: EDA and Data Preprocessing on Kenyan Student Dataset
# Instructor: JAPHETH MURSI | Date: 6th June 2025 | Due: 13th June 2025
# Load required libraries
library(ggplot2)
library(readr)  

data <- read_csv("/Users/AZalo/Downloads/kenya_student_data.csv")

# Question 9
#dentify columns with missing values and report their percentages.
n <- nrow(data)
missing_counts <- sapply(data, function(col) sum(is.na(col)))
missing_pct <- missing_counts / n * 100

vars_with_missing <- names(missing_counts)[missing_counts > 0]

report <- data.frame(
  variable     = vars_with_missing,
  missing_pct  = round(missing_pct[vars_with_missing], 2),
  row.names    = NULL,
  stringsAsFactors = FALSE
)
print(report)

# Question 10
#Impute missing values in family_income and math_score using the median
med_income <- median(data$family_income, na.rm = TRUE)
med_math   <- median(data$math_score,    na.rm = TRUE)

#Replace NAs
data$family_income[is.na(data$family_income)] <- med_income
data$math_score    [is.na(data$math_score)]    <- med_math

# Question 11
#Impute missing values in attendance_rate using the mean. 
#Compare the distributions before and after imputation using histograms.

raw_attend <- data$attendance_rate

#Plot before imputation
hist(raw_attend,
     main    = "Attendance Rate — Before Imputation",
     xlab    = "Attendance Rate",
     ylab    = "Count",
     col     = "lightgray",
     border  = "white",
     breaks  = 20)

#impute
mean_attend <- mean(data$attendance_rate, na.rm = TRUE)
data$attendance_rate[is.na(data$attendance_rate)] <- mean_attend

#Plot after imputation
hist(data$attendance_rate,
     main    = "Attendance Rate — After Imputation",
     xlab    = "Attendance Rate",
     ylab    = "Count",
     col     = "steelblue",
     border  = "white",
     breaks  = 20)

















