#' Plot Sample Correlation Heatmap
#'
#' Generates a heatmap showing the correlation between samples based on FPKM values.
#'
#' @param fpkm_data A data frame of FPKM values.
#'
#' @return A heatmap plot.
#' @export
plot_sample_correlation <- function(fpkm_data) {
  numeric_data <- fpkm_data[, sapply(fpkm_data, is.numeric), drop = FALSE]

  # Drop zero-variance columns
  numeric_data <- numeric_data[, apply(numeric_data, 2, function(x) var(x, na.rm = TRUE) != 0), drop = FALSE]

  if (ncol(numeric_data) < 2) {
    warning("Not enough variable columns to compute correlation matrix.")
    return(NULL)
  }

  correlation_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
  df_long <- reshape2::melt(correlation_matrix, varnames = c("Sample1", "Sample2"), value.name = "Correlation")

  ggplot2::ggplot(df_long, ggplot2::aes(x = Sample1, y = Sample2, fill = Correlation)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0.5) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Sample Correlation Heatmap", fill = "Correlation")
}


# modified to drop columns with no variance and handles empty cases
