#' Simple Linear Regression
#'
#' Fits a linear regression model and plots residuals vs fitted values.
#' This is a simpler interface than run_linear_regression_workflow().
#'
#' @param df A data frame.
#' @param response Response variable (numeric column name).
#' @param predictor Predictor variable (numeric column name).
#' @param plot_path Optional file path to save plot. If NULL, displays the plot.
#' @return The fitted model object with summary.
#' @export
simple_linear_regression <- function(df, response, predictor, plot_path = NULL) {
  if (!(response %in% names(df)) || !(predictor %in% names(df))) stop("Columns missing")
  formula <- as.formula(paste(response, "~", predictor))
  model <- lm(formula, data = df)
  print(summary(model))
  
  # Diagnostic plot: residuals vs fitted values
  res_df <- data.frame(fitted = fitted(model), residuals = resid(model))
  p <- ggplot2::ggplot(res_df, ggplot2::aes(x=fitted, y=residuals)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept=0, linetype="dashed") +
    ggplot2::labs(title=paste("Residuals vs Fitted:", response, "~", predictor),
         x="Fitted", y="Residuals") +
    ggplot2::theme_minimal()
  
  if (!is.null(plot_path)) {
    ggplot2::ggsave(plot_path, p, width=6, height=4)
    cat("Saved plot to", plot_path, "\n")
  } else {
    print(p)
  }
  return(model)
}