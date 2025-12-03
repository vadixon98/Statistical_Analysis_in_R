<div align="center">

# 📊 Statistical Analysis in R

Modern, script-first workflows for the most common hypothesis tests and exploratory analytics — all in pure R.

[![R >= 4.0](https://img.shields.io/badge/R-%3E%3D%204.0-276DC3?logo=r&logoColor=white)](#requirements) [![Made for RStudio](https://img.shields.io/badge/RStudio-friendly-75AADB?logo=rstudio&logoColor=white)](#getting-started) [![MIT License](https://img.shields.io/badge/License-MIT-black.svg)](#license) [![Contributions Welcome](https://img.shields.io/badge/PRs-welcome-4CAF50.svg)](#contributing)

</div>

---

## ✨ Why This Package

- **Comprehensive R package** with all statistical analysis functions organized and documented.
- **Workflow functions** for complete analyses (ANOVA, t-tests, regression, chi-square) with diagnostics.
- **Simple helper functions** for quick EDA, diagnostics, and reporting.
- **Readable outputs**: tables, plots, and effect sizes ready for stakeholders.
- **Well-documented** with roxygen2 documentation and examples.

---

## 🚀 Getting Started

### Install as an R Package

```r
# Install from local source
install.packages("devtools")
devtools::install("Statistical Analysis Package")

# Or install dependencies first
install.packages(c("ggplot2", "reshape2", "broom", "car", "effectsize"))

# Load the package
library(StatisticalAnalysisInR)
```

### Alternative: Use Standalone Scripts

If you prefer to use the standalone scripts directly:

```bash
git clone <repository-url>
cd <repository-directory>
```

```r
# Source individual scripts as needed
source("Anova and Multiple Comparison test.R")
source("Various t.tests.R")
source("Interactive Stats Tools Extended.R")
```

> ✅ Requires R ≥ 4.0. RStudio recommended for plot panes and interactive prompts.

---

## 🧰 Package Functions

### Workflow Functions (Comprehensive Analyses)

| Function | What it does | Highlights |
| --- | --- | --- |
| `run_anova_workflow()` | One-way ANOVA + Tukey HSD | Automatic assumption checks, tidy post-hoc table, effect sizes |
| `run_t_test_workflow()` | One-sample, paired, and two-sample t-tests | Effect sizes, Shapiro-Wilk, variance checks, visualizations |
| `run_linear_regression_workflow()` | Simple linear model | Prediction bands + confidence intervals, diagnostic plots |
| `run_chi_square_workflow()` | Chi-square test of independence | Cramer's V, expected count checks, visualizations |
| `predict_from_model()` | Generate predictions from linear regression | Prediction and confidence intervals for new data |

### Helper Functions (Quick Analyses)

| Function | What it does |
| --- | --- |
| `summary_stats()` | Descriptive statistics for numeric columns |
| `frequency_table()` | Frequency tables with bar plots |
| `correlation_matrix()` | Correlation matrices with heatmaps |
| `proportion_test()` | Two-sample proportion test |
| `normality_check()` | Normality diagnostics (Shapiro-Wilk, skewness, kurtosis, plots) |
| `outlier_detection()` | Outlier detection via IQR & Z-score methods |
| `two_sample_ttest()` | Two-sample t-test with Cohen's d |
| `chi_square_test()` | Chi-square association testing |
| `simple_linear_regression()` | Simple linear regression with diagnostic plots |
| `simple_logistic_regression()` | Logistic regression with odds ratios and confusion matrix |
| `pairwise_comparisons()` | Pairwise t-tests with multiple-comparison corrections |

---

## 📖 Usage Examples

### Workflow Functions

```r
library(StatisticalAnalysisInR)

# ANOVA workflow
results <- run_anova_workflow(
  data = my_data,
  response = "weight",
  group = "diet",
  save_plots = TRUE
)
results$anova_table
results$tukey

# T-test workflow
t_results <- run_t_test_workflow(
  sample1 = group_a,
  sample2 = group_b,
  group_labels = c("Treatment", "Control"),
  save_plots = TRUE
)

# Linear regression workflow
reg_results <- run_linear_regression_workflow(
  data = my_data,
  response = "Fat_Gain",
  predictor = "NEA",
  save_plots = TRUE
)
predict_from_model(reg_results$model, c(-50, 0, 100, 200))

# Chi-square workflow
chi_results <- run_chi_square_workflow(
  contingency = my_contingency_table,
  save_plot = TRUE
)
```

### Helper Functions

```r
# Quick descriptive statistics
summary_stats(my_data)

# Frequency table with plot
frequency_table(my_data, "category", plot = TRUE)

# Correlation matrix with heatmap
correlation_matrix(my_data, method = "pearson", plot = TRUE)

# Normality check
normality_check(my_data, "value")

# Outlier detection
outlier_detection(my_data, "value", method = "iqr")
```

---

## 💡 Quick Tips

- **Workflow functions** provide comprehensive analyses with assumption checks, effect sizes, and visualizations.
- **Helper functions** are great for quick exploratory data analysis.
- All plotting functions support saving plots to files via `save_plots` or `plot_path` parameters.
- Functions use `ggplot2` for visualizations, which can be customized further.
- See function documentation with `?function_name` for detailed parameter descriptions.

---

## 🤝 Contributing

PRs, issues, and ideas are welcome! Fork the repo, create a feature branch, and open a pull request describing the change. Screenshots/GIFs of interactive tools are appreciated.

---

## 📄 License

Released under the MIT License. Include a copy when redistributing and cite this repo if it helps your work.
