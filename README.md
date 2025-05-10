# R Statistical Data Analysis Project

This repository contains a comprehensive collection of statistical data analysis laboratories using R. Each lab focuses on different aspects of data exploration, visualization, analysis techniques, and machine learning models.

## Project Structure

```
.
├── .gitignore                 # Git ignore configuration
├── LICENSE                    # MIT License
├── Lab1/                      # Laboratory 1 - Data Preprocessing & Exploratory Analysis
│   ├── Excel/                 # Directory for Excel files
│   ├── Univariate-Bivariate-Data/
│   │   └── main.R             # R script for univariate/bivariate analysis
│   └── main.R                 # Main script for Wesbrook dataset analysis
├── Lab2/                      # Laboratory 2 - Machine Learning Models
│   └── main.R                 # R script for predictive modeling
├── Lab3/                      # Laboratory 3 - Advanced Analytics
│   ├── main.R                 # R script for correlation analysis with Eggs dataset
│   ├── main_1.R               # R script for market basket analysis (association rules)
│   └── main_2.R               # R script for customer segmentation (clustering)
├── Lab4/                      # Laboratory 4 - Visualization & Statistical Plots
│   └── main.R                 # R script for advanced data visualization
└── data/                      # Data directory
    └── LifeCycleSavings_with_additional_columns.csv  # Extended dataset
```

## Lab Descriptions

### Lab 1: Data Preprocessing & Exploratory Analysis
This lab focuses on data preprocessing and exploratory analysis using two datasets:

**1. LifeCycleSavings dataset:**
- Pair plots to show dependencies between variables
- Histograms for univariate analysis
- Bar plots comparing countries on various metrics
- Age distribution analysis
- Scatter plots to explore relationships between variables

**2. Wesbrook dataset:**
- Data preprocessing and cleaning
- Missing values analysis and imputation
- Exploratory data visualization with boxplots and histograms
- Mosaic plots for categorical variables
- Feature scaling and normalization
- Stratified sampling for train/validation split

### Lab 2: Machine Learning Models
This lab implements and evaluates various machine learning models for predicting donor behavior using the Wesbrook dataset:

- K-Nearest Neighbors (KNN)
- Naive Bayes
- Decision Trees
- Random Forests
- XGBoost
- Support Vector Machines (SVM)

Each model is evaluated using:
- Multiple cross-validation techniques (k-fold, LGOCV)
- Confusion matrices
- ROC curves and AUC metrics
- Parameter optimization

### Lab 3: Advanced Analytics
This lab covers three distinct analytical approaches:

**1. Correlation Analysis (Eggs dataset):**
- Correlation between sales and price variables
- Matrix scatter plots for relationship visualization
- Time-series analysis of egg sales and prices
- Factor analysis affecting egg sales

**2. Market Basket Analysis (Groceries dataset):**
- Association rule mining with the Apriori algorithm
- Frequency analysis of purchased items
- Support, confidence, and lift metrics

**3. Customer Segmentation (Mall Customers dataset):**
- K-means clustering for customer segmentation
- Optimal cluster determination using elbow, silhouette, and gap statistic methods
- Demographic analysis of clusters
- Spending patterns and income distribution analysis

### Lab 4: Statistical Visualization
This lab demonstrates various statistical visualization techniques in R:
- Histogram comparison with different settings
- Density plots and probability distributions
- Strip charts for group comparisons
- Box plots for distribution comparison
- Dual axis plots
- Pie charts for categorical data

## Key Datasets

### LifeCycleSavings Dataset
Contains savings and related economic data for various countries, including:
- Savings rates (sr)
- Population demographics (pop15, pop75)
- Average income (dpi)
- Income growth (ddpi)

### Wesbrook Dataset
Contains donor information for fundraising analysis:
- WESBROOK indicator (donations >$1000 in recent fiscal year)
- TOTLGIVE (total donations)
- Demographic variables (MARITAL, SEX, etc.)
- Geographic and economic indicators (AVE_INC, DWEL_VAL, etc.)

### Eggs Dataset
Analyzes egg sales data with related variables:
- Cases (egg sales)
- Various price indicators (Egg.Pr, Beef.Pr, Chicken.Pr, etc.)
- Seasonal factors (Month, Easter)

### Groceries Dataset
Contains transaction data for market basket analysis:
- Multiple items purchased in each transaction
- Frequencies of item purchases

### Mall Customers Dataset
Contains customer segmentation data:
- Customer demographics (Gender, Age)
- Income
- Spending Score

## Getting Started

### Prerequisites
- R (recommended version 4.0.0 or higher)
- RStudio (recommended for easier workflow)

### Required R Packages
```R
# Data manipulation
install.packages(c("tidyverse", "dplyr"))

# Visualization
install.packages(c("corrplot", "car", "plotly", "ggplot2", "rpart.plot", "factoextra"))

# Machine Learning & Statistics
install.packages(c("caret", "e1071", "ROCR", "rpart", "randomForest", "xgboost", "class"))

# Special Analytics
install.packages(c("arules", "stats"))
```

### Running the Analysis
1. Clone this repository
2. Open the desired lab's main.R file in RStudio
3. Install any required packages
4. Execute the script to see the analysis results

## Output
The scripts generate various visualization files in PNG format that are saved to the respective data directories. The analysis results include:

- Visualization plots (histograms, scatter plots, box plots, etc.)
- Model evaluation metrics (confusion matrices, ROC curves)
- Preprocessed datasets (saved as CSV files)
- Cluster analysis visualizations
- Association rules and patterns

## Notes
- All R scripts use UTF-8 encoding
- For Lab1, the charts are automatically saved in the 'data/lab-1' folder
- Interactive plots in Lab3 require the plotly package
- Some models in Lab2 may take longer to run due to cross-validation

## License
This project is licensed under the MIT License - see the LICENSE file for details.
