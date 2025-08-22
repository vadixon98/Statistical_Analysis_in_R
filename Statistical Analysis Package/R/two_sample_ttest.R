#' Two-Sample t-Test
#'
#' Performs a two-sample t-test and calculates Cohen's d.
#'
#' @param df A data frame.
#' @param outcome Outcome variable (numeric column name).
#' @param group Group variable (factor/character column name with two levels).
#' @param paired Logical, if TRUE performs paired t-test.
#' @param var.equal Logical, if TRUE assumes equal variance.
#' @return The t-test result.
#' @export
two_sample_ttest <- function(df, outcome, group, paired = FALSE, var.equal = FALSE) {
  if (!(outcome %in% names(df)) || !(group %in% names(df))) stop("Columns missing")
  if (length(unique(df[[group]])) != 2) stop("Group must have exactly two levels")
  formula <- as.formula(paste(outcome, "~", group))
  test <- t.test(formula, data = df, paired = paired, var.equal = var.equal)
  print(test)
  grp <- split(df[[outcome]], df[[group]])
  m1 <- mean(grp[[1]], na.rm=TRUE); m2 <- mean(grp[[2]], na.rm=TRUE)
  s1 <- sd(grp[[1]], na.rm=TRUE); s2 <- sd(grp[[2]], na.rm=TRUE)
  n1 <- length(na.omit(grp[[1]])); n2 <- length(na.omit(grp[[2]]))
  sp <- sqrt(((n1 -1)*s1^2 + (n2 -1)*s2^2)/(n1 + n2 -2))
  d <- (m1 - m2)/sp
  cat(sprintf("Cohen's d (pooled): %.3f\n", d))
  return(test)
}