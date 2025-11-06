# Interactive Stat Tools Extended.R
# Interactive versions of common small statistical analyses for use in RStudio.

library(ggplot2)
library(reshape2)
library(stats)

# ---- 1. Summary statistics ------------------------------------------------
# Computes descriptive statistics for numeric columns in a data frame
# 
# Parameters:
#   df: data frame containing the data
#   cols: optional character vector of column names to summarize; 
#         if NULL, summarizes all numeric columns
# 
# Returns:
#   Matrix with rows (count, mean, median, sd, IQR, min, max, missing) 
#   and columns for each numeric variable, rounded to 4 decimal places
summary_stats <- function(df, cols = NULL) {
  # Identify numeric columns to summarize
  if (is.null(cols)) {
    # If no columns specified, use all numeric columns
    nums <- names(df)[sapply(df, is.numeric)]
  } else {
    # Otherwise, use intersection of specified columns and numeric columns
    nums <- intersect(cols, names(df))
    nums <- nums[sapply(df[nums], is.numeric)]
  }
  if (length(nums) == 0) stop("No numeric columns to summarize.")
  
  # Helper function to compute summary statistics for a single variable
  summary_fn <- function(x) {
    nmiss <- sum(is.na(x))  # Count missing values
    x2 <- x[!is.na(x)]      # Remove missing values for calculations
    c(
      count = length(x2),      # Sample size (non-missing)
      mean = mean(x2),         # Arithmetic mean
      median = median(x2),     # Median (50th percentile)
      sd = sd(x2),             # Standard deviation
      IQR = IQR(x2),           # Interquartile range (Q3 - Q1)
      min = min(x2),           # Minimum value
      max = max(x2),           # Maximum value
      missing = nmiss          # Count of missing values
    )
  }
  # Apply summary function to each numeric column
  res <- sapply(df[ , nums, drop=FALSE], summary_fn)
  return(round(res, 4))
}

# ---- 2. Frequency table ---------------------------------------------------
# Creates a frequency table and optional bar chart for a categorical variable
# 
# Parameters:
#   df: data frame containing the data
#   column: character string name of the column to analyze
#   plot: logical, whether to create a bar chart (default: TRUE)
#   plot_path: optional character string path to save the plot; 
#              if NULL, plot is displayed only
# 
# Returns:
#   Invisible list with 'counts' (frequency table) and 'rel_freq' (proportions)
frequency_table <- function(df, column, plot = TRUE, plot_path = NULL) {
  if (!(column %in% names(df))) stop("Column not found")
  
  # Create frequency table (counts for each level)
  tbl <- table(df[[column]])
  # Calculate relative frequencies (proportions)
  rel <- prop.table(tbl)
  
  # Print results
  cat("Counts:\n"); print(tbl)
  cat("\nRelative frequencies:\n"); print(round(rel,4))
  
  # Create bar chart if requested
  if (plot) {
    plot_df <- data.frame(level=names(tbl), count=as.integer(tbl), rel=as.numeric(rel))
    p <- ggplot(plot_df, aes(x=level, y=count)) +
      geom_col() +
      labs(title=paste("Frequency of", column), x=column, y="Count") +
      theme_minimal()
    if (!is.null(plot_path)) {
      # Save plot to file
      ggsave(plot_path, p, width=6, height=4)
      cat("Saved barplot to", plot_path, "\n")
    } else {
      # Display plot in RStudio
      print(p)
    }
  }
  invisible(list(counts=tbl, rel_freq=rel))
}

# ---- 3. Correlation matrix ------------------------------------------------
# Computes correlation matrix for all numeric variables and optionally visualizes it
# 
# Parameters:
#   df: data frame containing the data
#   method: correlation method, either "pearson" (default), "spearman", or "kendall"
#   plot: logical, whether to create a heatmap visualization (default: TRUE)
#   plot_path: optional character string path to save the plot; 
#              if NULL, plot is displayed only
# 
# Returns:
#   Correlation matrix (symmetric matrix with values between -1 and 1)
correlation_matrix <- function(df, method = "pearson", plot = TRUE, plot_path = NULL) {
  # Extract only numeric columns
  num_df <- df[ , sapply(df, is.numeric), drop=FALSE]
  if (ncol(num_df) < 2) stop("Need at least two numeric columns")
  
  # Compute correlation matrix using pairwise complete observations
  # (handles missing values by using all available pairs for each variable pair)
  corr <- cor(num_df, use="pairwise.complete.obs", method=method)
  print(round(corr, 3))
  
  # Create heatmap visualization
  if (plot) {
    # Reshape correlation matrix to long format for ggplot
    m <- melt(corr)
    p <- ggplot(m, aes(x=Var1, y=Var2, fill=value)) +
      geom_tile() +  # Create heatmap tiles
      geom_text(aes(label=round(value,2)), size=3) +  # Add correlation values as text
      scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0) +  # Color scale
      labs(title=paste("Correlation matrix (", method, ")", sep=""), x="", y="") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=45, hjust=1))  # Rotate x-axis labels
    if (!is.null(plot_path)) {
      ggsave(plot_path, p, width=6, height=5)
      cat("Saved correlation heatmap to", plot_path, "\n")
    } else {
      print(p)
    }
  }
  return(corr)
}

# ---- 4. Proportion test ---------------------------------------------------
# Performs a two-sample test of proportions (comparing proportions between two groups)
# 
# Parameters:
#   df: data frame containing the data
#   outcome_col: character string name of binary outcome column (coded as 0/1)
#   group_col: character string name of grouping variable (must have exactly 2 levels)
#   correct: logical, whether to apply continuity correction (default: FALSE)
# 
# Returns:
#   Result object from prop.test() containing test statistics and p-value
proportion_test <- function(df, outcome_col, group_col, correct = FALSE) {
  if (!(outcome_col %in% names(df)) || !(group_col %in% names(df))) stop("Columns missing")
  
  # Check that group column has exactly two levels
  groups <- unique(df[[group_col]])
  if (length(groups) != 2) stop("Group column must have exactly two levels")
  
  # Count successes (outcome == 1) for each group
  counts <- tapply(df[[outcome_col]], df[[group_col]], function(x) sum(x == 1, na.rm=TRUE))
  # Count total observations (non-missing) for each group
  totals <- tapply(df[[outcome_col]], df[[group_col]], function(x) sum(!is.na(x)))
  
  cat("Success counts:\n"); print(counts)
  cat("Total per group:\n"); print(totals)
  
  # Perform two-sample proportion test
  test <- prop.test(x=as.numeric(counts), n=as.numeric(totals), correct=correct)
  print(test)
  return(test)
}

# ---- 5. Normality check ---------------------------------------------------
# Assesses whether a numeric variable follows a normal distribution using 
# descriptive statistics, Shapiro-Wilk test, histogram, and Q-Q plot
# 
# Parameters:
#   df: data frame containing the data
#   variable: character string name of the numeric variable to check
#   out_prefix: character string prefix for output file names (default: "normality")
# 
# Returns:
#   None (prints results and saves plots to files)
normality_check <- function(df, variable, out_prefix = "normality") {
  if (!(variable %in% names(df))) stop("Variable not found")
  
  # Extract variable and remove missing values
  x <- df[[variable]]
  x <- x[!is.na(x)]
  if (!is.numeric(x)) stop("Variable must be numeric")
  
  n <- length(x)
  mean_x <- mean(x)
  sd_x <- sd(x)
  
  # Calculate skewness: measure of asymmetry (0 = symmetric, >0 = right-skewed, <0 = left-skewed)
  skew <- sum((x - mean_x)^3)/(n * sd_x^3)
  # Calculate excess kurtosis: measure of tail heaviness (0 = normal, >0 = heavy tails, <0 = light tails)
  kurt <- sum((x - mean_x)^4)/(n * sd_x^4) - 3
  
  cat(sprintf("Variable: %s\nN: %d\nMean: %.3f\nSD: %.3f\nSkewness: %.3f\nKurtosis (excess): %.3f\n",
              variable, n, mean_x, sd_x, skew, kurt))
  
  # Shapiro-Wilk test for normality (only valid for sample sizes 3-5000)
  if (n >= 3 && n <= 5000) {
    sw <- shapiro.test(x)
    cat("Shapiro-Wilk test:\n")
    print(sw)
  } else {
    cat("Shapiro-Wilk not run (n out of range 3-5000)\n")
  }
  
  # Create histogram with normal distribution overlay
  p1 <- ggplot(data.frame(x=x), aes(x)) +
    geom_histogram(bins=30, mapping=aes(y=..density..), fill="grey80", color="black") +
    stat_function(fun=dnorm, args=list(mean=mean_x, sd=sd_x), linetype="dashed") +
    labs(title=paste("Histogram of", variable), y="Density") +
    theme_minimal()
  ggsave(paste0(out_prefix, "_", variable, "_hist.png"), p1, width=5, height=4)
  cat("Saved histogram to", paste0(out_prefix, "_", variable, "_hist.png"), "\n")
  
  # Create Q-Q plot (quantile-quantile plot) to assess normality
  # Points should fall approximately on the line if data is normally distributed
  qq <- ggplot(data.frame(sample=x), aes(sample=sample)) +
    stat_qq() + stat_qq_line() +
    labs(title=paste("Q-Q Plot of", variable)) +
    theme_minimal()
  ggsave(paste0(out_prefix, "_", variable, "_qq.png"), qq, width=5, height=4)
  cat("Saved Q-Q plot to", paste0(out_prefix, "_", variable, "_qq.png"), "\n")
}

# ---- 6. Outlier detection ------------------------------------------------
# Identifies outliers in a numeric column using IQR rule or Z-score method
# 
# Parameters:
#   df: data frame containing the data
#   column: character string name of the numeric column to analyze
#   method: character string, either "iqr" (default) or "zscore"
#   threshold: numeric threshold for outlier detection
#              - For IQR: multiplier for IQR (default 1.5, typical range 1.5-3.0)
#              - For Z-score: absolute Z-score threshold (default 1.5)
# 
# Returns:
#   Invisible vector of row indices containing outliers
outlier_detection <- function(df, column, method = c("iqr", "zscore"), threshold = 1.5) {
  method <- match.arg(method)
  if (!(column %in% names(df))) stop("Column missing")
  
  x <- df[[column]]
  xnum <- x[!is.na(x)]  # Non-missing values for calculations
  if (!is.numeric(xnum)) stop("Column must be numeric")
  
  outlier_idx <- NULL
  
  if (method == "iqr") {
    # IQR method: outliers are values outside Q1 - threshold*IQR and Q3 + threshold*IQR
    Q1 <- quantile(xnum, 0.25)  # First quartile (25th percentile)
    Q3 <- quantile(xnum, 0.75)  # Third quartile (75th percentile)
    IQRv <- Q3 - Q1             # Interquartile range
    lower <- Q1 - threshold * IQRv  # Lower bound
    upper <- Q3 + threshold * IQRv  # Upper bound
    # Find outliers (using original x to preserve NA positions)
    outlier_idx <- which(x < lower | x > upper)
    cat(sprintf("IQR rule: lower=%.3f upper=%.3f\n", lower, upper))
  } else if (method == "zscore") {
    # Z-score method: outliers are values with |z| > threshold
    mu <- mean(xnum)
    sdv <- sd(xnum)
    z <- (x - mu)/sdv  # Calculate Z-scores (using original x to preserve NA positions)
    outlier_idx <- which(abs(z) > threshold)
    cat(sprintf("Z-score threshold: |z| > %.2f\n", threshold))
  }
  
  cat(sprintf("Found %d outliers in column '%s':\n", length(outlier_idx), column))
  if (length(outlier_idx) > 0) {
    print(data.frame(index=outlier_idx, value=df[[column]][outlier_idx]))
  }
  invisible(outlier_idx)
}

# ---- 7. Two-sample t-test -------------------------------------------------
# Performs a two-sample t-test to compare means between two groups
# 
# Parameters:
#   df: data frame containing the data
#   outcome: character string name of the numeric outcome variable
#   group: character string name of the grouping variable (must have exactly 2 levels)
#   paired: logical, whether to perform paired t-test (default: FALSE)
#   var.equal: logical, whether to assume equal variances (default: FALSE, uses Welch's test)
# 
# Returns:
#   Result object from t.test() containing test statistics, p-value, and confidence interval
two_sample_ttest <- function(df, outcome, group, paired = FALSE, var.equal = FALSE) {
  if (!(outcome %in% names(df)) || !(group %in% names(df))) stop("Columns missing")
  if (length(unique(df[[group]])) != 2) stop("Group must have exactly two levels")
  
  # Create formula for t-test
  formula <- as.formula(paste(outcome, "~", group))
  # Perform t-test
  test <- t.test(formula, data = df, paired = paired, var.equal = var.equal)
  print(test)
  
  # Calculate Cohen's d (effect size measure)
  # Cohen's d = (mean1 - mean2) / pooled_standard_deviation
  grp <- split(df[[outcome]], df[[group]])
  m1 <- mean(grp[[1]], na.rm=TRUE); m2 <- mean(grp[[2]], na.rm=TRUE)
  s1 <- sd(grp[[1]], na.rm=TRUE); s2 <- sd(grp[[2]], na.rm=TRUE)
  n1 <- length(na.omit(grp[[1]])); n2 <- length(na.omit(grp[[2]]))
  
  # Calculate pooled standard deviation
  sp <- sqrt(((n1 -1)*s1^2 + (n2 -1)*s2^2)/(n1 + n2 -2))
  d <- (m1 - m2)/sp
  cat(sprintf("Cohen's d (pooled): %.3f\n", d))
  return(test)
}

# ---- 8. Chi-square test --------------------------------------------------
# Performs a chi-square test of independence for two categorical variables
# Tests whether there is a significant association between the two variables
# 
# Parameters:
#   df: data frame containing the data
#   col1: character string name of the first categorical variable
#   col2: character string name of the second categorical variable
# 
# Returns:
#   Result object from chisq.test() containing test statistics and p-value
chi_square_test <- function(df, col1, col2) {
  if (!(col1 %in% names(df)) || !(col2 %in% names(df))) stop("Columns missing")
  
  # Create contingency table (cross-tabulation)
  tbl <- table(df[[col1]], df[[col2]])
  print(tbl)
  
  # Perform chi-square test of independence
  test <- chisq.test(tbl)
  print(test)
  return(test)
}

# ---- 9. Simple linear regression -----------------------------------------
# Fits a simple linear regression model (one predictor) and provides diagnostics
# 
# Parameters:
#   df: data frame containing the data
#   response: character string name of the numeric response/dependent variable
#   predictor: character string name of the numeric predictor/independent variable
# 
# Returns:
#   Linear model object from lm() containing coefficients, residuals, fitted values, etc.
simple_linear_regression <- function(df, response, predictor) {
  if (!(response %in% names(df)) || !(predictor %in% names(df))) stop("Columns missing")
  
  # Create formula for linear regression: response ~ predictor
  formula <- as.formula(paste(response, "~", predictor))
  # Fit linear model using ordinary least squares
  model <- lm(formula, data = df)
  print(summary(model))
  
  # Diagnostic plot: residuals vs fitted values
  # Should show random scatter around zero with no patterns
  res_df <- data.frame(fitted = fitted(model), residuals = resid(model))
  p <- ggplot(res_df, aes(x=fitted, y=residuals)) +
    geom_point() +
    geom_hline(yintercept=0, linetype="dashed") +  # Reference line at zero
    labs(title=paste("Residuals vs Fitted:", response, "~", predictor),
         x="Fitted", y="Residuals") +
    theme_minimal()
  print(p)
  return(model)
}

# ----10. Simple logistic regression ---------------------------------------
# Fits a simple logistic regression model (one predictor) for binary outcomes
# 
# Parameters:
#   df: data frame containing the data
#   outcome: character string name of the binary outcome variable (must be coded as 0/1)
#   predictor: character string name of the predictor variable (can be numeric or categorical)
# 
# Returns:
#   Generalized linear model object from glm() with binomial family
simple_logistic_regression <- function(df, outcome, predictor) {
  if (!(outcome %in% names(df)) || !(predictor %in% names(df))) stop("Columns missing")
  if (!all(df[[outcome]] %in% c(0,1))) stop("Outcome must be coded 0/1")
  
  # Create formula for logistic regression: outcome ~ predictor
  formula <- as.formula(paste(outcome, "~", predictor))
  # Fit logistic regression model using maximum likelihood estimation
  model <- glm(formula, data = df, family = binomial())
  print(summary(model))
  
  # Calculate odds ratios (exponentiated coefficients)
  # Odds ratio > 1 means predictor increases odds of outcome = 1
  OR <- exp(coef(model))
  cat("Odds ratios:\n"); print(OR)
  
  # Generate predicted probabilities
  probs <- predict(model, type="response")
  
  # Create confusion matrix using 0.5 as classification threshold
  pred <- ifelse(probs >= 0.5, 1, 0)
  tab <- table(Predicted=pred, Actual=df[[outcome]])
  cat("Confusion matrix (threshold=0.5):\n"); print(tab)
  return(model)
}

# ----11. Pairwise comparisons (t-tests) -----------------------------------
# Performs pairwise t-tests between all groups with p-value adjustment for multiple comparisons
# Useful for post-hoc analysis after ANOVA or when comparing multiple groups
# 
# Parameters:
#   df: data frame containing the data
#   outcome: character string name of the numeric outcome variable
#   group: character string name of the grouping variable (can have 2+ levels)
#   p_adjust_method: character string method for p-value adjustment
#                    Options: "bonferroni" (default), "holm", "fdr", "none", etc.
#                    See ?p.adjust for full list
# 
# Returns:
#   Result object from pairwise.t.test() containing p-values for all pairwise comparisons
pairwise_comparisons <- function(df, outcome, group, p_adjust_method = "bonferroni") {
  if (!(outcome %in% names(df)) || !(group %in% names(df))) stop("Columns missing")
  if (!is.numeric(df[[outcome]])) stop("Outcome must be numeric")
  if (length(unique(df[[group]])) < 2) stop("Need at least two groups")
  
  # Perform pairwise t-tests with p-value adjustment
  # Adjustment is necessary to control family-wise error rate when making multiple comparisons
  pw <- pairwise.t.test(df[[outcome]], df[[group]], p.adjust.method = p_adjust_method)
  print(pw)
  return(pw)
}

# ---- Example usage in RStudio --------------------------------------------
# df <- read.csv("mock_normality.csv")
# summary_stats(df)
# frequency_table(df, "group")
# correlation_matrix(df)
# proportion_test(df, outcome_col = "outcome", group_col = "group")
# normality_check(df, "value", out_prefix = "demo")
# outlier_detection(df, "value", method="iqr")
# two_sample_ttest(df, outcome="value", group="group")
# chi_square_test(df, "cat1", "cat2")
# simple_linear_regression(df, response="y", predictor="x1")
# simple_logistic_regression(df, outcome="binary", predictor="x1")
# pairwise_comparisons(df, outcome="value", group="group_factor")
