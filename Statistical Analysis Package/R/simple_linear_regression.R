#' Simple Linear Regression
#'
#' Fits a linear regression model and plots residuals vs fitted values.
#'
#' @param df A data frame.
#' @param response Response variable (numeric column name).
#' @param predictor Predictor variable (numeric column name).
#' @return The fitted model object.
#' @export
simple_linear_regression <- function(df, response, predictor) {
  if (!(response %in% names(df)) || !(predictor %in% names(df))) stop("Columns missing")
  formula <- as.formula(paste(response, "~", predictor))
  model <- lm(formula, data = df)
  print(summary(model))
  res_df <- data.frame(fitted = fitted(model), residuals = resid(model))
  p <- ggplot2::ggplot(res_df, ggplot2::aes(x=fitted, y=residuals)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept=0, linetype="dashed") +
    ggplot2::labs(title=paste("Residuals vs Fitted:", response, "~", predictor),
         x="Fitted", y="Residuals") +
    ggplot2::theme_minimal()
  print(p)
  return(model)
}