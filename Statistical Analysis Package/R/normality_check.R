#' Normality Check
#'
#' Assesses normality of a numeric variable: skewness, kurtosis, Shapiro-Wilk test, histogram, and Q-Q plot.
#'
#' @param df A data frame.
#' @param variable Column name (character) of numeric variable.
#' @param out_prefix Prefix for saved file names.
#' @return None (prints results and saves plots).
#' @export
normality_check <- function(df, variable, out_prefix = "normality") {
  if (!(variable %in% names(df))) stop("Variable not found")
  x <- df[[variable]]
  x <- x[!is.na(x)]
  if (!is.numeric(x)) stop("Variable must be numeric")
  n <- length(x)
  mean_x <- mean(x)
  sd_x <- sd(x)
  skew <- sum((x - mean_x)^3)/(n * sd_x^3)
  kurt <- sum((x - mean_x)^4)/(n * sd_x^4) - 3
  cat(sprintf("Variable: %s\nN: %d\nMean: %.3f\nSD: %.3f\nSkewness: %.3f\nKurtosis (excess): %.3f\n",
              variable, n, mean_x, sd_x, skew, kurt))
  if (n >= 3 && n <= 5000) {
    sw <- shapiro.test(x)
    cat("Shapiro-Wilk test:\n")
    print(sw)
  } else {
    cat("Shapiro-Wilk not run (n out of range 3-5000)\n")
  }
  p1 <- ggplot2::ggplot(data.frame(x=x), ggplot2::aes(x)) +
    ggplot2::geom_histogram(bins=30, mapping=ggplot2::aes(y=..density..), fill="grey80", color="black") +
    ggplot2::stat_function(fun=dnorm, args=list(mean=mean_x, sd=sd_x), linetype="dashed") +
    ggplot2::labs(title=paste("Histogram of", variable), y="Density") +
    ggplot2::theme_minimal()
  ggplot2::ggsave(paste0(out_prefix, "_", variable, "_hist.png"), p1, width=5, height=4)
  cat("Saved histogram to", paste0(out_prefix, "_", variable, "_hist.png"), "\n")
  qq <- ggplot2::ggplot(data.frame(sample=x), ggplot2::aes(sample=sample)) +
    ggplot2::stat_qq() + ggplot2::stat_qq_line() +
    ggplot2::labs(title=paste("Q-Q Plot of", variable)) +
    ggplot2::theme_minimal()
  ggplot2::ggsave(paste0(out_prefix, "_", variable, "_qq.png"), qq, width=5, height=4)
  cat("Saved Q-Q plot to", paste0(out_prefix, "_", variable, "_qq.png"), "\n")
}