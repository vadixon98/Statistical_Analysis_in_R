#' Proportion Test
#'
#' Performs a two-sample proportion test for a binary outcome between two groups.
#'
#' @param df A data frame.
#' @param outcome_col Column name (character) for binary outcome (should be 1 for success).
#' @param group_col Column name (character) for group (must have exactly two levels).
#' @param correct Logical. If TRUE, applies continuity correction.
#' @return The result of \code{prop.test}.
#' @export
proportion_test <- function(df, outcome_col, group_col, correct = FALSE) {
  if (!(outcome_col %in% names(df)) || !(group_col %in% names(df))) stop("Columns missing")
  groups <- unique(df[[group_col]])
  if (length(groups) != 2) stop("Group column must have exactly two levels")
  counts <- tapply(df[[outcome_col]], df[[group_col]], function(x) sum(x == 1, na.rm=TRUE))
  totals <- tapply(df[[outcome_col]], df[[group_col]], function(x) sum(!is.na(x)))
  cat("Success counts:\n"); print(counts)
  cat("Total per group:\n"); print(totals)
  test <- prop.test(x=as.numeric(counts), n=as.numeric(totals), correct=correct)
  print(test)
  return(test)
}