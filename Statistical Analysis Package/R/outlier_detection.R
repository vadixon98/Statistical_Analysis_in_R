#' Outlier Detection
#'
#' Detects outliers in a numeric column using IQR or Z-score methods.
#'
#' @param df A data frame.
#' @param column Column name (character) of numeric variable.
#' @param method Method for outlier detection ("iqr" or "zscore").
#' @param threshold Threshold multiplier (default 1.5 for IQR, 2 or 3 for zscore).
#' @return Indices of detected outliers.
#' @export
outlier_detection <- function(df, column, method = c("iqr", "zscore"), threshold = 1.5) {
  method <- match.arg(method)
  if (!(column %in% names(df))) stop("Column missing")
  x <- df[[column]]
  xnum <- x[!is.na(x)]
  if (!is.numeric(xnum)) stop("Column must be numeric")
  outlier_idx <- NULL
  if (method == "iqr") {
    Q1 <- quantile(xnum, 0.25)
    Q3 <- quantile(xnum, 0.75)
    IQRv <- Q3 - Q1
    lower <- Q1 - threshold * IQRv
    upper <- Q3 + threshold * IQRv
    outlier_idx <- which(x < lower | x > upper)
    cat(sprintf("IQR rule: lower=%.3f upper=%.3f\n", lower, upper))
  } else if (method == "zscore") {
    mu <- mean(xnum)
    sdv <- sd(xnum)
    z <- (x - mu)/sdv
    outlier_idx <- which(abs(z) > threshold)
    cat(sprintf("Z-score threshold: |z| > %.2f\n", threshold))
  }
  cat(sprintf("Found %d outliers in column '%s':\n", length(outlier_idx), column))
  if (length(outlier_idx) > 0) {
    print(data.frame(index=outlier_idx, value=df[[column]][outlier_idx]))
  }
  invisible(outlier_idx)
}