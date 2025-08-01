# Simple Stats tools.R
# RStudio / interactive versions of:
# summary_stats, frequency_table, correlation_matrix, proportion_test,
# normality_check, outlier_detection.

library(ggplot2)
library(reshape2)

# 1. Summary statistics ----------------------------------------------------
summary_stats <- function(df, cols = NULL) {
  if (is.null(cols)) {
    nums <- names(df)[sapply(df, is.numeric)]
  } else {
    nums <- intersect(cols, names(df))
    nums <- nums[sapply(df[nums], is.numeric)]
  }
  if (length(nums) == 0) stop("No numeric columns to summarize.")
  summary_fn <- function(x) {
    nmiss <- sum(is.na(x))
    x2 <- x[!is.na(x)]
    c(
      count = length(x2),
      mean = mean(x2),
      median = median(x2),
      sd = sd(x2),
      IQR = IQR(x2),
      min = min(x2),
      max = max(x2),
      missing = nmiss
    )
  }
  res <- sapply(df[ , nums, drop=FALSE], summary_fn)
  return(round(res, 4))
}

# 2. Frequency table ------------------------------------------------------
frequency_table <- function(df, column, plot = TRUE, plot_path = NULL) {
  if (!(column %in% names(df))) stop("Column not found")
  tbl <- table(df[[column]])
  rel <- prop.table(tbl)
  cat("Counts:\n"); print(tbl)
  cat("\nRelative frequencies:\n"); print(round(rel,4))
  if (plot) {
    plot_df <- data.frame(level=names(tbl), count=as.integer(tbl), rel=as.numeric(rel))
    p <- ggplot(plot_df, aes(x=level, y=count)) +
      geom_col() +
      labs(title=paste("Frequency of", column), x=column, y="Count") +
      theme_minimal()
    if (!is.null(plot_path)) {
      ggsave(plot_path, p, width=6, height=4)
      cat("Saved barplot to", plot_path, "\n")
    } else {
      print(p)
    }
  }
  invisible(list(counts=tbl, rel_freq=rel))
}

# 3. Correlation matrix ---------------------------------------------------
correlation_matrix <- function(df, method = "pearson", plot = TRUE, plot_path = NULL) {
  num_df <- df[ , sapply(df, is.numeric), drop=FALSE]
  if (ncol(num_df) < 2) stop("Need at least two numeric columns")
  corr <- cor(num_df, use="pairwise.complete.obs", method=method)
  print(round(corr, 3))
  if (plot) {
    m <- melt(corr)
    p <- ggplot(m, aes(x=Var1, y=Var2, fill=value)) +
      geom_tile() +
      geom_text(aes(label=round(value,2)), size=3) +
      scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0) +
      labs(title=paste("Correlation matrix (", method, ")", sep=""), x="", y="") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=45, hjust=1))
    if (!is.null(plot_path)) {
      ggsave(plot_path, p, width=6, height=5)
      cat("Saved correlation heatmap to", plot_path, "\n")
    } else {
      print(p)
    }
  }
  return(corr)
}

# 4. Proportion test ------------------------------------------------------
proportion_test <- function(df, outcome_col, group_col, correct = FALSE) {
  if (!(outcome_col %in% names(df)) || !(group_col %in% names(df))) stop("Columns missing")
  groups <- unique(df[[group_col]])
  if (length(groups) != 2) stop("Group column must have exactly two levels")
  counts <- tapply(df[[outcome_col]], df[[group_col]], function(x) sum(x == 1, na.rm=TRUE))
  totals <- tapply(df[[outcome_col]], df[[group_col]], function(x) sum(!is.na(x)))
  cat("Success counts:\n"); print(counts)
  cat("Total per group:\n"); print(totals)
  test <- prop.test(x=as.numeric(counts), n=as.numeric(totals), correct=correct)
  print(test)
  return(test)
}

# 5. Normality check ------------------------------------------------------
normality_check <- function(df, variable, out_prefix = "normality") {
  if (!(variable %in% names(df))) stop("Variable not found")
  x <- df[[variable]]
  x <- x[!is.na(x)]
  if (!is.numeric(x)) stop("Variable must be numeric")
  n <- length(x)
  mean_x <- mean(x)
  sd_x <- sd(x)
  skew <- sum((x - mean_x)^3)/(n * sd_x^3)
  kurt <- sum((x - mean_x)^4)/(n * sd_x^4) - 3
  cat(sprintf("Variable: %s\nN: %d\nMean: %.3f\nSD: %.3f\nSkewness: %.3f\nKurtosis (excess): %.3f\n",
              variable, n, mean_x, sd_x, skew, kurt))
  if (n >= 3 && n <= 5000) {
    sw <- shapiro.test(x)
    cat("Shapiro-Wilk test:\n")
    print(sw)
  } else {
    cat("Shapiro-Wilk not run (n out of range 3-5000)\n")
  }
  # histogram
  p1 <- ggplot(data.frame(x=x), aes(x)) +
    geom_histogram(bins=30, mapping=aes(y=..density..), fill="grey80", color="black") +
    stat_function(fun=dnorm, args=list(mean=mean_x, sd=sd_x), linetype="dashed") +
    labs(title=paste("Histogram of", variable), y="Density") +
    theme_minimal()
  ggsave(paste0(out_prefix, "_", variable, "_hist.png"), p1, width=5, height=4)
  cat("Saved histogram to", paste0(out_prefix, "_", variable, "_hist.png"), "\n")
  # QQ plot
  qq <- ggplot(data.frame(sample=x), aes(sample=sample)) +
    stat_qq() + stat_qq_line() +
    labs(title=paste("Q-Q Plot of", variable)) +
    theme_minimal()
  ggsave(paste0(out_prefix, "_", variable, "_qq.png"), qq, width=5, height=4)
  cat("Saved Q-Q plot to", paste0(out_prefix, "_", variable, "_qq.png"), "\n")
}

# 6. Outlier detection ---------------------------------------------------
outlier_detection <- function(df, column, method = c("iqr", "zscore"), threshold = 1.5) {
  method <- match.arg(method)
  if (!(column %in% names(df))) stop("Column missing")
  x <- df[[column]]
  xnum <- x[!is.na(x)]
  if (!is.numeric(xnum)) stop("Column must be numeric")
  outlier_idx <- NULL
  if (method == "iqr") {
    Q1 <- quantile(xnum, 0.25)
    Q3 <- quantile(xnum, 0.75)
    IQRv <- Q3 - Q1
    lower <- Q1 - threshold * IQRv
    upper <- Q3 + threshold * IQRv
    outlier_idx <- which(x < lower | x > upper)
    cat(sprintf("IQR rule: lower=%.3f upper=%.3f\n", lower, upper))
  } else if (method == "zscore") {
    mu <- mean(xnum)
    sdv <- sd(xnum)
    z <- (x - mu)/sdv
    outlier_idx <- which(abs(z) > threshold)
    cat(sprintf("Z-score threshold: |z| > %.2f\n", threshold))
  }
  cat(sprintf("Found %d outliers in column '%s':\n", length(outlier_idx), column))
  if (length(outlier_idx) > 0) {
    print(data.frame(index=outlier_idx, value=df[[column]][outlier_idx]))
  }
  invisible(outlier_idx)
}

# ---- Example usage (in RStudio) ----
# df <- read.csv("mock_normality.csv")
# print(summary_stats(df))
# frequency_table(df, column = "group")
# correlation_matrix(df)
# proportion_test(df, outcome_col = "outcome", group_col = "group")
# normality_check(df, variable = "value", out_prefix = "demo")
# outlier_detection(df, column = "value", method = "iqr", threshold = 1.5)
