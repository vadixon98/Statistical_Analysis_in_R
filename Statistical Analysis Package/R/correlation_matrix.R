#' Correlation Matrix
#'
#' Calculates and prints the correlation matrix for numeric columns. Optionally plots a heatmap.
#'
#' @param df A data frame.
#' @param method Correlation method ("pearson", "spearman", "kendall").
#' @param plot Logical. If TRUE, plots the correlation heatmap.
#' @param plot_path Optional file path to save plot. If NULL, displays the plot.
#' @return The correlation matrix.
#' @export
correlation_matrix <- function(df, method = "pearson", plot = TRUE, plot_path = NULL) {
  num_df <- df[ , sapply(df, is.numeric), drop=FALSE]
  if (ncol(num_df) < 2) stop("Need at least two numeric columns")
  corr <- cor(num_df, use="pairwise.complete.obs", method=method)
  print(round(corr, 3))
  if (plot) {
    m <- reshape2::melt(corr)
    p <- ggplot2::ggplot(m, ggplot2::aes(x=Var1, y=Var2, fill=value)) +
      ggplot2::geom_tile() +
      ggplot2::geom_text(ggplot2::aes(label=round(value,2)), size=3) +
      ggplot2::scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0) +
      ggplot2::labs(title=paste("Correlation matrix (", method, ")", sep=""), x="", y="") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=45, hjust=1))
    if (!is.null(plot_path)) {
      ggplot2::ggsave(plot_path, p, width=6, height=5)
      cat("Saved correlation heatmap to", plot_path, "\n")
    } else {
      print(p)
    }
  }
  return(corr)
}