# Chi-square test workflow helper
# --------------------------------------------
# Source this script to gain a reusable chi-square testing function with
# visualization, expected-count checks, and effect sizes (Cramer's V).

suppressPackageStartupMessages({
  library(ggplot2)
})

prepare_chi_table <- function(contingency = NULL,
                              data = NULL,
                              row_var = NULL,
                              col_var = NULL) {
  if (!is.null(contingency)) {
    if (!is.matrix(contingency) && !inherits(contingency, "table")) {
      stop("`contingency` must be a matrix or table.")
    }
    tbl <- as.table(contingency)
  } else {
    if (is.null(data) || is.null(row_var) || is.null(col_var)) {
      stop("Provide `contingency` or a `data`, `row_var`, `col_var` combination.")
    }
    if (!(row_var %in% names(data)) || !(col_var %in% names(data))) {
      stop("row_var or col_var not found in data.")
    }
    tbl <- table(data[[row_var]], data[[col_var]])
  }
  tbl
}

cramers_v <- function(chi_sq, n, r, c) {
  min_dim <- min(r - 1, c - 1)
  if (min_dim == 0) return(0)
  sqrt(chi_sq / (n * min_dim))
}

plot_chi_counts <- function(tbl, title, save_plot, path_prefix) {
  df <- as.data.frame(tbl)
  names(df) <- c("Row", "Column", "Count")
  p <- ggplot(df, aes(x = Column, y = Count, fill = Row)) +
    geom_col(position = position_dodge(width = 0.8)) +
    labs(
      title = title,
      x = "",
      y = "Observed count",
      fill = ""
    ) +
    theme_minimal()
  if (save_plot) {
    ggsave(paste0(path_prefix, "_counts.png"), p, width = 6, height = 4, dpi = 300)
  } else {
    print(p)
  }
}

expected_count_checks <- function(expected, threshold = 5) {
  violated <- sum(expected < threshold)
  list(
    min_expected = min(expected),
    cells_below_threshold = violated,
    passed = violated == 0
  )
}

run_chi_square_workflow <- function(contingency = NULL,
                                    data = NULL,
                                    row_var = NULL,
                                    col_var = NULL,
                                    correct = FALSE,
                                    title = "Chi-square test",
                                    save_plot = FALSE,
                                    plot_prefix = "chi_square") {
  tbl <- prepare_chi_table(contingency = contingency, data = data, row_var = row_var, col_var = col_var)
  plot_chi_counts(tbl, title, save_plot, plot_prefix)
  
  test <- suppressWarnings(chisq.test(tbl, correct = correct))
  exp_check <- expected_count_checks(test$expected)
  
  n <- sum(tbl)
  effect <- cramers_v(test$statistic, n = n, r = nrow(tbl), c = ncol(tbl))
  
  contributions <- round((test$observed - test$expected)^2 / test$expected, 3)
  
  list(
    table = tbl,
    test = test,
    expected_check = exp_check,
    effect_size = list(
      cramers_v = unname(effect),
      n = n
    ),
    standardized_residuals = round(test$stdres, 3),
    contribution_table = contributions
  )
}

# Example usage with the ladybug counts:
# ladybug <- matrix(c(115, 30, 85, 70),
#                   nrow = 2,
#                   byrow = TRUE,
#                   dimnames = list(c("Black", "Red"),
#                                   c("Industrial", "Rural")))
# results <- run_chi_square_workflow(
#   contingency = ladybug,
#   title = "Ladybug colour vs habitat",
#   save_plot = TRUE,
#   plot_prefix = "ladybug"
# )
# results$effect_size