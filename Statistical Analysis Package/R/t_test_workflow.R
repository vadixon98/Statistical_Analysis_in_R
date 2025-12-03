#' T-Test Workflow
#'
#' Comprehensive t-test workflow supporting one-sample, paired, and two-sample tests with diagnostics.
#'
#' @param sample1 Numeric vector for the first sample (required).
#' @param sample2 Numeric vector for the second sample (optional, for two-sample or paired tests).
#' @param paired Logical, whether to perform paired t-test (default: FALSE).
#' @param mu Numeric, hypothesized mean for one-sample test (default: 0).
#' @param var.equal Logical, whether to assume equal variances (default: FALSE).
#' @param alternative Character string, alternative hypothesis: "two.sided", "less", or "greater" (default: "two.sided").
#' @param conf.level Confidence level (default: 0.95).
#' @param group_labels Character vector of length 2 for group labels in plots (default: c("Group 1", "Group 2")).
#' @param save_plots Logical, whether to save plots (default: FALSE).
#' @param plot_prefix Character string prefix for plot filenames (default: "t_test").
#' @return A list containing test results, effect size (Cohen's d), normality checks, and variance test.
#' @export
#' @importFrom ggplot2 ggplot aes geom_density geom_boxplot labs theme_minimal guides ggsave
run_t_test_workflow <- function(sample1,
                                sample2 = NULL,
                                paired = FALSE,
                                mu = 0,
                                var.equal = FALSE,
                                alternative = c("two.sided", "less", "greater"),
                                conf.level = 0.95,
                                group_labels = c("Group 1", "Group 2"),
                                save_plots = FALSE,
                                plot_prefix = "t_test") {
  alternative <- match.arg(alternative)
  x <- sample1[!is.na(sample1)]
  y <- if (!is.null(sample2)) sample2[!is.na(sample2)] else NULL
  
  plot_samples(x, y, group_labels, save_plots, plot_prefix)
  
  test <- if (is.null(y)) {
    t.test(x, mu = mu, alternative = alternative, conf.level = conf.level)
  } else {
    t.test(x, y, paired = paired, var.equal = var.equal, alternative = alternative, conf.level = conf.level)
  }
  
  variance_test <- NULL
  if (!is.null(y) && !paired) {
    variance_test <- var.test(x, y)
  }
  
  list(
    test = test,
    effect_size = cohens_d(x, y, paired = paired, mu = mu),
    shapiro = list(
      group1 = check_normality(x),
      group2 = if (!is.null(y)) check_normality(y) else NULL
    ),
    variance_test = variance_test
  )
}

#' Check Normality
#'
#' Helper function to check normality using Shapiro-Wilk test.
#'
#' @param x Numeric vector.
#' @return List with test statistic, p-value, and pass status.
#' @keywords internal
check_normality <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 3 || length(x) > 5000) {
    return(list(statistic = NA, p_value = NA, passed = NA, note = "Shapiro-Wilk not run (n out of range)"))
  }
  sw <- shapiro.test(x)
  list(
    statistic = sw$statistic,
    p_value = sw$p.value,
    passed = sw$p.value > 0.05,
    note = NULL
  )
}

#' Cohen's d
#'
#' Calculate Cohen's d effect size.
#'
#' @param sample1 Numeric vector for first sample.
#' @param sample2 Numeric vector for second sample (optional).
#' @param paired Logical, whether samples are paired.
#' @param mu Numeric, hypothesized mean for one-sample test.
#' @return Numeric, Cohen's d effect size.
#' @keywords internal
cohens_d <- function(sample1,
                     sample2 = NULL,
                     paired = FALSE,
                     mu = 0) {
  x <- sample1
  y <- sample2
  
  if (is.null(y)) {
    return((mean(x) - mu) / sd(x))
  }
  
  if (paired) {
    diff_vals <- x - y
    return(mean(diff_vals) / sd(diff_vals))
  }
  
  n1 <- length(x)
  n2 <- length(y)
  pooled_sd <- sqrt(((n1 - 1) * var(x) + (n2 - 1) * var(y)) / (n1 + n2 - 2))
  (mean(x) - mean(y)) / pooled_sd
}

#' Plot Samples
#'
#' Helper function to create visualization plots for t-test samples.
#'
#' @param sample1 Numeric vector for first sample.
#' @param sample2 Numeric vector for second sample (optional).
#' @param group_labels Character vector of group labels.
#' @param save_plots Logical, whether to save plots.
#' @param plot_prefix Character string prefix for filenames.
#' @keywords internal
plot_samples <- function(sample1,
                         sample2 = NULL,
                         group_labels = c("Group 1", "Group 2"),
                         save_plots = FALSE,
                         plot_prefix = "t_test") {
  df <- data.frame(
    value = c(sample1, if (!is.null(sample2)) sample2 else numeric(0)),
    group = c(rep(group_labels[1], length(sample1)),
              if (!is.null(sample2)) rep(group_labels[2], length(sample2)) else character(0))
  )
  df <- df[!is.na(df$value), ]
  
  density_plot <- ggplot2::ggplot(df, ggplot2::aes(value, fill = group)) +
    ggplot2::geom_density(alpha = 0.4) +
    ggplot2::labs(title = "Density comparison", x = "Value", y = "Density", fill = "") +
    ggplot2::theme_minimal()
  
  box_plot <- ggplot2::ggplot(df, ggplot2::aes(group, value, fill = group)) +
    ggplot2::geom_boxplot(alpha = 0.6) +
    ggplot2::labs(title = "Boxplot comparison", x = "", y = "Value", fill = "") +
    ggplot2::theme_minimal() +
    ggplot2::guides(fill = "none")
  
  if (save_plots) {
    ggplot2::ggsave(paste0(plot_prefix, "_density.png"), density_plot, width = 6, height = 4, dpi = 300)
    ggplot2::ggsave(paste0(plot_prefix, "_boxplot.png"), box_plot, width = 4, height = 4, dpi = 300)
  } else {
    print(density_plot)
    print(box_plot)
  }
}

