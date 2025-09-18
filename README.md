# R Statistical Tests

A collection of R scripts for performing common statistical tests and interactive statistical analyses. Updates oncoming.

---

## Requirements

* **R** (version 4.0 or higher)
* **RStudio** (recommended for interactive features)
* Required packages: `ggplot2`, `reshape2`, `stats`

---

## Installation

1. Download or clone the repository:

   ```bash
   git clone <repository-url>
   cd <repository-directory>
   ```
2. Ensure you have R installed and accessible from your command line or RStudio.
3. Install required R packages:

   ```r
   install.packages(c("ggplot2", "reshape2"))
   ```

---

## Scripts Overview

### 1. `Anova and Multiple Comparison test.R`

Performs one-way ANOVA followed by Tukey’s HSD multiple comparison test.

**Usage example:**

```r
source("Anova and Multiple Comparison test.R")
```

### 2. `Implementing a Chi Square test.R`

Conducts a Chi-square test of independence on a 2×2 contingency table with visualization.

**Usage example:**

```r
source("Implementing a Chi Square test.R")
```

### 3. `Linear regression with prediction and confidence intervals.R`

Performs simple linear regression with prediction and confidence intervals.

**Usage example:**

```r
source("Linear regression with prediction and confidence intervals.R")
```

### 4. `Various t.tests.R`

Demonstrates execution of various t-tests, including paired and one-sample t-tests, with assumption checks.

**Usage example:**

```r
source("Various t.tests.R")
```

### 5. `Interactive Stat Tools Extended.R`

Provides interactive versions of common small statistical analyses for use in RStudio. Includes:

* **Summary statistics**: Count, mean, median, SD, IQR, min, max, missing values.
* **Frequency table**: Counts and relative frequencies with optional bar plots.
* **Correlation matrix**: Computes and visualizes correlation heatmaps.
* **Proportion test**: Two-group proportion test.
* **Normality check**: Shapiro-Wilk test, skewness, kurtosis, histogram, Q-Q plot.
* **Outlier detection**: IQR and Z-score methods.
* **Two-sample t-test**: With Cohen’s d effect size.
* **Chi-square test**: For categorical associations.
* **Simple linear regression**: With diagnostic plots.
* **Simple logistic regression**: With odds ratios and confusion matrix.
* **Pairwise comparisons**: Pairwise t-tests with multiple comparison correction.

**Usage example:**

```r
source("Interactive Stat Tools Extended.R")
```

---

## Usage Tips

* **ANOVA & Multiple Comparisons**: Replace `file.choose()` with a fixed file path for reproducibility.
* **Chi-square Test**: Modify the contingency table to match observed frequencies.
* **Regression Analysis**: Customize plot appearance; compare manual vs. `lm()`-based coefficients.
* **t-tests**: Use `mu` for one-sample tests and `paired=TRUE` for paired t-tests.
* **Interactive Tools**: Most functions print results and generate plots automatically in RStudio.

---

## Contributing

Contributions, bug reports, and enhancements are welcome. Please fork the repository, commit your changes, and submit a pull request.

---

## License

This project is released under the MIT License. Include a copy of the license when distributing.
