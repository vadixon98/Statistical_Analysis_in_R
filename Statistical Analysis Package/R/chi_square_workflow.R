#' Chi-Square Test Workflow
#'
#' Comprehensive chi-square test workflow with visualization, expected-count checks, and effect sizes (Cramer's V).
#'
#' @param contingency A matrix or table of contingency counts (optional if data is provided).
#' @param data A data frame (optional if contingency is provided).
#' @param row_var Character string name of the row variable column (required if using data).
#' @param col_var Character string name of the column variable column (required if using data).
#' @param correct Logical, whether to apply continuity correction (default: FALSE).
#' @param title Character string title for plots (default: "Chi-square test").
#' @param save_plot Logical, whether to save plot (default: FALSE).
#' @param plot_prefix Character string prefix for plot filename (default: "chi_square").
#' @return A list containing the contingency table, test results, expected count checks, effect size (Cramer's V), standardized residuals, and contribution table.
#' @export
#' @importFrom ggplot2 ggplot aes geom_col labs theme_minimal ggsave position_dodge
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

#' Prepare Chi-Square Table
#'
#' Helper function to prepare contingency table for chi-square test.
#'
#' @param contingency A matrix or table (optional if data is provided).
#' @param data A data frame (optional if contingency is provided).
#' @param row_var Character string name of the row variable column.
#' @param col_var Character string name of the column variable column.
#' @return A contingency table.
#' @keywords internal
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

#' Cramer's V
#'
#' Calculate Cramer's V effect size for chi-square test.
#'
#' @param chi_sq Chi-square statistic.
#' @param n Total sample size.
#' @param r Number of rows.
#' @param c Number of columns.
#' @return Numeric, Cramer's V effect size.
#' @keywords internal
cramers_v <- function(chi_sq, n, r, c) {
  min_dim <- min(r - 1, c - 1)
  if (min_dim == 0) return(0)
  sqrt(chi_sq / (n * min_dim))
}

#' Plot Chi-Square Counts
#'
#' Helper function to create visualization for chi-square test.
#'
#' @param tbl Contingency table.
#' @param title Character string title for plot.
#' @param save_plot Logical, whether to save plot.
#' @param path_prefix Character string prefix for filename.
#' @keywords internal
plot_chi_counts <- function(tbl, title, save_plot, path_prefix) {
  df <- as.data.frame(tbl)
  names(df) <- c("Row", "Column", "Count")
  p <- ggplot2::ggplot(df, ggplot2::aes(x = Column, y = Count, fill = Row)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8)) +
    ggplot2::labs(
      title = title,
      x = "",
      y = "Observed count",
      fill = ""
    ) +
    ggplot2::theme_minimal()
  if (save_plot) {
    ggplot2::ggsave(paste0(path_prefix, "_counts.png"), p, width = 6, height = 4, dpi = 300)
  } else {
    print(p)
  }
}

#' Expected Count Checks
#'
#' Helper function to check expected counts for chi-square test assumptions.
#'
#' @param expected Matrix of expected counts.
#' @param threshold Minimum expected count threshold (default: 5).
#' @return List with minimum expected count, number of cells below threshold, and pass status.
#' @keywords internal
expected_count_checks <- function(expected, threshold = 5) {
  violated <- sum(expected < threshold)
  list(
    min_expected = min(expected),
    cells_below_threshold = violated,
    passed = violated == 0
  )
}

