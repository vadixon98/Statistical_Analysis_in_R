# Reusable t-test helpers (one-sample, paired, or two-sample)

suppressPackageStartupMessages({
  library(ggplot2)
})

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
  
  density_plot <- ggplot(df, aes(value, fill = group)) +
    geom_density(alpha = 0.4) +
    labs(title = "Density comparison", x = "Value", y = "Density", fill = "") +
    theme_minimal()
  
  box_plot <- ggplot(df, aes(group, value, fill = group)) +
    geom_boxplot(alpha = 0.6) +
    labs(title = "Boxplot comparison", x = "", y = "Value", fill = "") +
    theme_minimal() +
    guides(fill = "none")
  
  if (save_plots) {
    ggsave(paste0(plot_prefix, "_density.png"), density_plot, width = 6, height = 4, dpi = 300)
    ggsave(paste0(plot_prefix, "_boxplot.png"), box_plot, width = 4, height = 4, dpi = 300)
  } else {
    print(density_plot)
    print(box_plot)
  }
}

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

# Example usage ----------------------------------------
Group_A <- c(-0.17460092, 0.57131273, -0.97957078, 0.55760611, -1.38675079,
             0.87678217, 0.02424806, 0.37201613, -1.08039729, 0.29898955,
             0.14407196, 0.23606151, 1.16615124, -0.34826371, -0.12453904,
             0.20398317, 0.58846349, -0.74028122, 1.76338615, 0.01492071,
             0.19413667, -0.34928128, -0.95047882, 1.57190112, -0.39559300,
             0.04158754, -0.46319252, -1.07299956, -0.27920085, 0.84563163)

Group_B <- c(3.1131149, 1.9480742, 2.5673920, 1.7320103, 4.0895170, 1.1603928,
             1.3140735, 2.5380473, 0.8589795, 0.5685645, 1.9639550, 2.6961566,
             2.5803225, 3.4920269, 1.3917766, 2.1461159, 2.6398042, 0.7714907,
             1.9098055, 1.6603716, 1.9494934, 1.4430121, 2.7838871, 1.7334802,
             3.5283881, 2.9197496, 1.9893398, 3.3018724, 0.1881463, 2.1493005)

# Two-sample example
# result_two_sample <- run_t_test_workflow(
#   sample1 = Group_A,
#   sample2 = Group_B,
#   group_labels = c("Group A", "Group B"),
#   save_plots = TRUE,
#   plot_prefix = "two_sample_t"
# )
# result_two_sample$test

# One-sample example
# result_one_sample <- run_t_test_workflow(
#   sample1 = Group_A,
#   mu = 0,
#   alternative = "two.sided",
#   save_plots = FALSE
# )
# result_one_sample$effect_size

# Paired example
# result_paired <- run_t_test_workflow(
#   sample1 = Group_A,
#   sample2 = Group_A + rnorm(length(Group_A), 0.2, 0.3),
#   paired = TRUE
# )