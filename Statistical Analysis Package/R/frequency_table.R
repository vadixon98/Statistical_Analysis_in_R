#' Frequency Table
#'
#' Produces a frequency table and bar plot for a factor or categorical variable.
#'
#' @param df A data frame.
#' @param column Column name (character) for which to compute frequency.
#' @param plot Logical. If TRUE, creates a bar plot.
#' @param plot_path Optional file path to save plot. If NULL, displays the plot.
#' @return Invisibly returns a list with counts and relative frequencies.
#' @export
frequency_table <- function(df, column, plot = TRUE, plot_path = NULL) {
  if (!(column %in% names(df))) stop("Column not found")
  tbl <- table(df[[column]])
  rel <- prop.table(tbl)
  cat("Counts:\n"); print(tbl)
  cat("\nRelative frequencies:\n"); print(round(rel,4))
  if (plot) {
    plot_df <- data.frame(level=names(tbl), count=as.integer(tbl), rel=as.numeric(rel))
    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x=level, y=count)) +
      ggplot2::geom_col() +
      ggplot2::labs(title=paste("Frequency of", column), x=column, y="Count") +
      ggplot2::theme_minimal()
    if (!is.null(plot_path)) {
      ggplot2::ggsave(plot_path, p, width=6, height=4)
      cat("Saved barplot to", plot_path, "\n")
    } else {
      print(p)
    }
  }
  invisible(list(counts=tbl, rel_freq=rel))
}