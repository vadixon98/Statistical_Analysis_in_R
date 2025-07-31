# R Statistical Tests

A collection of R scripts for performing common statistical tests, including one-way ANOVA with multiple comparisons, Chi-square tests of independence, simple linear regression with prediction and confidence intervals, and various t-tests, using base R functions and graphics.

---

## Requirements

* **R** (version 4.0 or higher)
* No external packages required (uses base R: `stats` and `graphics` libraries).

---

## Installation

1. Download or clone the repository:

   ```bash
   git clone <repository-url>
   cd <repository-directory>
   ```
2. Ensure you have R installed and accessible from your command line or RStudio.

---

## Scripts Overview

### 1. `Anova and Multiple Comparison test.R`

Performs one-way ANOVA followed by Tukey’s Honest Significant Difference (HSD) multiple comparison test:

1. **Data Import**: Load data via `read.table()` (interactive file chooser).
2. **Data Preparation**:

   * Combine group measurements into a single numeric vector.
   * Create a factor vector of group labels.
   * Construct a data frame for analysis.
3. **Exploratory Data Analysis**:

   * Generate boxplots by group to visualize distributions.
4. **Assumption Checks**:

   * Compute standard deviations for each group.
5. **ANOVA Model**:

   * Fit model with `aov()` and display ANOVA table using `anova()`.
6. **Interpretation**:

   * Assess p-value to accept or reject equality of means.
7. **Multiple Comparison**:

   * Apply `TukeyHSD()` for pairwise group comparisons.

**Usage example**:

```r
source("Anova and Multiple Comparison test.R")
```

### 2. `Implementing a Chi Square test.R`

Conducts a Chi-square test of independence on a 2×2 contingency table and visualizes counts:

1. **Contingency Table Definition**:

   * Create a matrix of observed counts with `matrix()` and assign row/column names.
2. **Bar Plot**:

   * Use `barplot()` with grouping to compare category frequencies visually.
3. **Chi-square Test**:

   * Execute `chisq.test()` on the matrix.
4. **Interpretation**:

   * Examine the p-value to determine if there is a significant association between factors.

**Usage example**:

```r
source("Implementing a Chi Square test.R")
```

### 3. `Linear regression with prediction and confidence intervals.R`

Performs simple linear regression, manual coefficient computation, model fitting, and interval estimation:

1. **Data Definition & EDA**:

   * Defines numeric vectors `NEA` and `Fat_Gain`.
   * Plots scatterplot: `plot(NEA, Fat_Gain, pch=17, col="dark red")`.
   * Calculates Pearson correlation coefficient: `cor(NEA, Fat_Gain)`.
2. **Manual Regression Calculations**:

   * Computes slope (`My_b`) and intercept (`My_a`) from means and standard deviations.
   * Verifies that the regression line passes through `(mean_x, mean_y)`.
3. **Model-Based Regression**:

   * Fits linear model via `lm(Fat_Gain ~ NEA)`; retrieves coefficients with `coef()`.
   * Plots regression lines using `abline()` (manual in yellow, model-based in blue).
4. **Interval Estimation**:

   * Generates prediction intervals: `predict(My_lm, newdata=My_df, interval="prediction", level=0.95)`.
   * Generates confidence intervals: `predict(My_lm, newdata=My_df, interval="confidence", level=0.95)`.
   * Defines and uses `plot.add.ci()` to overlay PI/CI bands on existing scatterplots.
5. **Helper Function**:

   * `mr_predictor(x_values, linear_model)`: Computes predicted y-values for a vector of x.

**Usage example**:

```r
source("Linear regression with prediction and confidence intervals.R")
```

### 4. `Various t.tests.R`

Demonstrates execution of various t-tests, data visualization, and assumption checks:

1. **Data Definition**:

   * Defines numeric vectors `Group_A` and `Group_B`.
2. **Exploratory Data Analysis**:

   * Generates boxplot: `boxplot(Group_A, Group_B, names=c("Group A", "Group B"))`.
   * Plots histograms: `hist(Group_A)` and `hist(Group_B)`.
3. **Two-sample t-tests**:

   * Performs two-sided: `t.test(Group_A, Group_B)` (default).
   * Specifies alternatives: `alternative="two.sided"`, `"less"`, `"greater"`.
4. **One-sample and paired t-tests**:

   * `t.test(data1, mu=x)` for one-sample.
   * `t.test(data1, data2, paired=TRUE)` for matched pairs.
5. **Assumption Checks**:

   * Checks normality: histogram (`hist(data1)`), density plot (`plot(density(data1))`), Q-Q plot (`qqnorm(data1)`).

**Usage example**:

```r
source("Various t.tests.R")
```

---

## Usage Tips

* **ANOVA & Multiple Comparisons**:

  * Replace `file.choose()` with a fixed file path for reproducibility.
* **Chi-square Test**:

  * Modify the contingency table in the script to reflect your observed frequencies.
* **Regression Analysis**:

  * Customize plot symbols (`pch`), colors (`col`), and line widths (`lwd`) for clarity.
  * Compare manual vs. `lm()`-based coefficients to reinforce understanding.
* **t-tests**:

  * Adjust the `alternative` argument in `t.test()` for two-sided, one-sided ("less" or "greater") tests.
  * Use `mu` for one-sample tests and `paired=TRUE` for paired t-tests.
  * Visualize data before testing with `boxplot()`, `hist()`, and assess normality via `qqnorm()` and `plot(density())`.

---

## Contributing

Contributions, bug reports, and enhancements are welcome. Please fork the repository, commit your changes, and submit a pull request.

---

## License

This project is released under the MIT License. Include a copy of the license when distributing.
