# StatisticalAnalysisInR

A comprehensive R package for statistical analysis, hypothesis testing, and exploratory data analysis.

## Installation

```r
# Install from local source
install.packages("devtools")
devtools::install("Statistical Analysis Package")

# Load the package
library(StatisticalAnalysisInR)
```

## Dependencies

- ggplot2
- reshape2
- broom
- car
- effectsize

Install dependencies with:
```r
install.packages(c("ggplot2", "reshape2", "broom", "car", "effectsize"))
```

## Main Functions

### Workflow Functions
- `run_anova_workflow()` - Complete ANOVA analysis with diagnostics
- `run_t_test_workflow()` - Comprehensive t-test workflows
- `run_linear_regression_workflow()` - Linear regression with diagnostics
- `run_chi_square_workflow()` - Chi-square test with effect sizes
- `predict_from_model()` - Generate predictions from regression models

### Helper Functions
- `summary_stats()` - Descriptive statistics
- `frequency_table()` - Frequency tables with plots
- `correlation_matrix()` - Correlation matrices with heatmaps
- `normality_check()` - Normality diagnostics
- `outlier_detection()` - Outlier detection
- `two_sample_ttest()` - Two-sample t-tests
- `chi_square_test()` - Chi-square tests
- `simple_linear_regression()` - Simple linear regression
- `simple_logistic_regression()` - Logistic regression
- `proportion_test()` - Proportion tests
- `pairwise_comparisons()` - Pairwise comparisons

See the main README.md in the repository root for detailed examples and usage.

