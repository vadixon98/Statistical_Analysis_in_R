#' Pairwise Comparisons (t-tests)
#'
#' Performs pairwise t-tests between groups with p-value adjustment.
#'
#' @param df A data frame.
#' @param outcome Numeric outcome variable.
#' @param group Grouping variable (factor or character).
#' @param p_adjust_method Method for p-value adjustment ("bonferroni", "holm", etc.)
#' @return Pairwise t-test result.
#' @export
pairwise_comparisons <- function(df, outcome, group, p_adjust_method = "bonferroni") {
  if (!(outcome %in% names(df)) || !(group %in% names(df))) stop("Columns missing")
  if (!is.numeric(df[[outcome]])) stop("Outcome must be numeric")
  if (length(unique(df[[group]])) < 2) stop("Need at least two groups")
  pw <- pairwise.t.test(df[[outcome]], df[[group]], p.adjust.method = p_adjust_method)
  print(pw)
  return(pw)
}