# EDA & Preprocessing Kenyan Students

## Project Overview and Purpose

This repository contains exploratory data analysis (EDA) and preprocessing steps performed on a dataset of Kenyan university students. The goal is to prepare the data for downstream modeling—such as predicting academic performance—and to surface key insights about student demographics, study habits, and performance drivers.

## Instructions to Run the R Script

1. **Clone or download** this repository and place `kenya_student_data.csv` in the project root.
2. **Open the R project** (`MRDC 911.Rproj`) in RStudio; this sets the working directory automatically.
3. **Install required packages** (if not already installed):

   ```r
   install.packages(c("readr", "corrplot", "ggplot2"), repos = "https://cloud.r-project.org")
   ```
4. **Run scripts in order**:

   1. **Exploratory Data Analysis**:

      * In RStudio, open `EDA/StudentDataProcessing.R` and click **Source**.
      * This generates initial summary stats and plots.
   2. **Handle missing values**:

      * Open `DataProcessing/MissingValues.R` and **Source**.
   3. **Detect and cap outliers**:

      * Open `DataProcessing/Outliers.R` and **Source**.
   4. **Feature engineering & relationships**:

      * Open `DataProcessing/FeatureEngineering&Relationships.R` and **Source**.

5. **View outputs**:

   * Console prints (structure, summary tables).
   * Plots will appear in the R graphics pane or saved in `plots/` if directed.
   * The final preprocessed CSV (`kenya_student_data_preprocessed.csv`) is written to the project root.

## Summary of Key Findings

* **Missing Data**: `family_income`, `math_score`, and `attendance_rate` had 2–10% missing values. Income and scores were imputed with the median (robust to outliers) and attendance with the mean (symmetrically distributed).
* **Distributions & Outliers**:

  * `family_income` exhibited extreme values; 1.5×IQR capping reduced skew.
  * `study_hours_weekly` clustered around 10–20 hours, with few extreme low/high outliers.
* **Target Balance**: `academic_performance` is imbalanced (majority in "Average"/"Good").
* **Correlations**: Numeric features showed very low correlations ,indicating minimal multicollinearity.
* **Associations**: A χ² test revealed a significant link between `internet_access` and performance (p < 2.2e-16); students with home internet are over‑represented among higher performance categories.
* **Feature Engineering**:

  * Binned `study_hours_weekly` into Low/Moderate/High/Very High.
  * Quartiled `family_income` into four income groups and observed higher income correlates with better performance.
  * Created `total_score` as the average of `math_score`, `science_score`, and `english_score`, which follows an approximately normal distribution.

## Repository Structure

```
├── DataProcessing/                     # Data cleaning & feature engineering scripts
│   ├── FeatureEngineering&Relationships.R  # creates new features and examines feature relationships
│   ├── MissingValues.R                    # handles identifying and imputing missing data
│   └── Outliers.R                          # detects and caps outliers
├── EDA/                                # Exploratory Data Analysis
│   └── StudentDataProcessing.R        # initial data loading, summary stats, and plots
├── assignment1_analysis.R              # consolidated EDA & preprocessing script
├── MRDC 911.Rproj                      # RStudio project file
├── kenya_student_data.csv              # Raw dataset (5,000 student records)
├── kenya_student_data_preprocessed.csv # Output after imputation & capping
└── Report FilE 
└── README.md  
                        # Project overview and instructions
```

