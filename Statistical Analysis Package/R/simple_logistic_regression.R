#' Simple Logistic Regression
#'
#' Fits a logistic regression for a binary outcome and displays odds ratios.
#'
#' @param df A data frame.
#' @param outcome Outcome variable (must be coded 0/1).
#' @param predictor Predictor variable.
#' @param threshold Numeric threshold for classification (default: 0.5).
#' @return The fitted model object with odds ratios and confusion matrix.
#' @export
simple_logistic_regression <- function(df, outcome, predictor, threshold = 0.5) {
  if (!(outcome %in% names(df)) || !(predictor %in% names(df))) stop("Columns missing")
  if (!all(df[[outcome]] %in% c(0,1))) stop("Outcome must be coded 0/1")
  formula <- as.formula(paste(outcome, "~", predictor))
  model <- glm(formula, data = df, family = binomial())
  print(summary(model))
  
  # Calculate odds ratios (exponentiated coefficients)
  OR <- exp(coef(model))
  cat("Odds ratios:\n"); print(OR)
  
  # Generate predicted probabilities
  probs <- predict(model, type="response")
  
  # Create confusion matrix using supplied threshold
  pred <- ifelse(probs >= threshold, 1, 0)
  tab <- table(Predicted=pred, Actual=df[[outcome]])
  cat(sprintf("Confusion matrix (threshold=%.2f):\n", threshold)); print(tab)
  
  result <- list(
    model = model,
    odds_ratios = OR,
    probabilities = probs,
    confusion_matrix = tab,
    threshold = threshold
  )
  return(result)
}