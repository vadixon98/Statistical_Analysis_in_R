#' Chi-Square Test
#'
#' Performs a chi-square test of independence between two categorical variables.
#'
#' @param df A data frame.
#' @param col1 First categorical column name.
#' @param col2 Second categorical column name.
#' @return The chi-square test result.
#' @export
chi_square_test <- function(df, col1, col2) {
  if (!(col1 %in% names(df)) || !(col2 %in% names(df))) stop("Columns missing")
  tbl <- table(df[[col1]], df[[col2]])
  print(tbl)
  test <- chisq.test(tbl)
  print(test)
  return(test)
}