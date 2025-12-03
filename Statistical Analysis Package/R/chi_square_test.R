#' Chi-Square Test
#'
#' Performs a chi-square test of independence between two categorical variables.
#' This is a simpler interface than run_chi_square_workflow().
#'
#' @param df A data frame.
#' @param col1 First categorical column name.
#' @param col2 Second categorical column name.
#' @return The chi-square test result with effect size.
#' @export
chi_square_test <- function(df, col1, col2) {
  if (!(col1 %in% names(df)) || !(col2 %in% names(df))) stop("Columns missing")
  
  # Create contingency table (cross-tabulation)
  tbl <- table(df[[col1]], df[[col2]])
  print(tbl)
  
  # Perform chi-square test of independence
  test <- chisq.test(tbl)
  print(test)
  n <- sum(tbl)
  effect <- sqrt(as.numeric(test$statistic) / (n * (min(dim(tbl)) - 1)))
  contributions <- round((test$observed - test$expected)^2 / test$expected, 3)
  
  result <- list(
    test = test,
    effect_size = effect,
    table = tbl,
    standardized_residuals = test$stdres,
    contributions = contributions
  )
  return(result)
}