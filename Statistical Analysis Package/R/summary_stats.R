#' Summary Statistics
#'
#' Computes summary statistics (mean, median, SD, IQR, min, max, missing) for all or selected numeric columns in a data frame.
#'
#' @param df A data frame.
#' @param cols Optional character vector of column names to summarize. If NULL, summarizes all numeric columns.
#' @return A matrix of summary statistics (rounded to 4 digits).
#' @export
summary_stats <- function(df, cols = NULL) {
  if (is.null(cols)) {
    nums <- names(df)[sapply(df, is.numeric)]
  } else {
    nums <- intersect(cols, names(df))
    nums <- nums[sapply(df[nums], is.numeric)]
  }
  if (length(nums) == 0) stop("No numeric columns to summarize.")
  summary_fn <- function(x) {
    nmiss <- sum(is.na(x))
    x2 <- x[!is.na(x)]
    c(
      count = length(x2),
      mean = mean(x2),
      median = median(x2),
      sd = sd(x2),
      IQR = IQR(x2),
      min = min(x2),
      max = max(x2),
      missing = nmiss
    )
  }
  res <- sapply(df[ , nums, drop=FALSE], summary_fn)
  return(round(res, 4))
}