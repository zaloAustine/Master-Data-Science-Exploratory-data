# MRDC 911 Assignment 1: EDA and Data Preprocessing on Kenyan Student Dataset
# Instructor: JAPHETH MURSI | Date: 6th June 2025 | Due: 13th June 2025
# Load required libraries
library(tidyverse)
library(corrplot)
library(ggplot2)


data <- read_csv("/Users/AZalo/Downloads/kenya_student_data.csv")

# Question 12
#Detect outliers in family_income using the IQR method.
income <- data$family_income

#Compute IQR bounds 
Q1          <- quantile(income, 0.25, na.rm = TRUE)
Q3          <- quantile(income, 0.75, na.rm = TRUE)
IQR_val     <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR_val
upper_bound <- Q3 + 1.5 * IQR_val

#Count outliers 
num_low  <- sum(income < lower_bound, na.rm = TRUE)
num_high <- sum(income > upper_bound, na.rm = TRUE)
num_outliers <- num_low + num_high

cat("Number of lower-bound outliers: ", num_low, "\n")
cat("Number of upper-bound outliers: ", num_high, "\n")
cat("Total outliers: ", num_outliers, "\n")



# Question 13
#Cap outliers in family_income at the 1.5*IQR bounds
capped_income <- ifelse(
  income < lower_bound,  lower_bound,
  ifelse(income > upper_bound, upper_bound, income)
)

# 6. Plot boxplots before vs. after 
par(mfrow = c(1, 2))  # two plots side by side

boxplot(income,
        main = "Family Income\nBefore Capping",
        ylab = "KES",
        col  = "lightgray")

boxplot(capped_income,
        main = "Family Income\nAfter Capping",
        ylab = "KES",
        col  = "lightblue")

par(mfrow = c(1, 1))  # reset plotting layout

