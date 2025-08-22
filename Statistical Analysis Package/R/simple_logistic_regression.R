#' Simple Logistic Regression
#'
#' Fits a logistic regression for a binary outcome and displays odds ratios.
#'
#' @param df A data frame.
#' @param outcome Outcome variable (must be coded 0/1).
#' @param predictor Predictor variable.
#' @return The fitted model object.
#' @export
simple_logistic_regression <- function(df, outcome, predictor) {
  if (!(outcome %in% names(df)) || !(predictor %in% names(df))) stop("Columns missing")
  if (!all(df[[outcome]] %in% c(0,1))) stop("Outcome must be coded 0/1")
  formula <- as.formula(paste(outcome, "~", predictor))
  model <- glm(formula, data = df, family = binomial())
  print(summary(model))
  OR <- exp(coef(model))
  cat("Odds ratios:\n"); print(OR)
  probs <- predict(model, type="response")
  pred <- ifelse(probs >= 0.5, 1, 0)
  tab <- table(Predicted=pred, Actual=df[[outcome]])
  cat("Confusion matrix (threshold=0.5):\n"); print(tab)
  return(model)
}