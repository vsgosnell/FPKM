#' Plot Sample Correlation Heatmap
#'
#' This function computes the correlation matrix between samples based on their FPKM values
#' and then visualizes the correlations using a heatmap. It can help assess the similarity between
#' samples based on gene expression data.
#'
#' @param numeric_data A data frame or matrix where rows represent genes and columns represent samples.
#' The values should be numeric (FPKM or other expression data).
#'
#' @return A `pheatmap` object displaying the correlation matrix between samples.
#' The heatmap is clustered by both rows (samples) and columns (samples) using the Euclidean distance.
#'
#' @examples
#' # Example usage of plot_sample_correlation
#' df <- data.frame(
#'   Gene = paste0("Gene", 1:3),
#'   Sample1 = c(1, 2, 3),
#'   Sample2 = c(4, 5, 6),
#'   Sample3 = c(7, 8, 9)
#' )
#' plot_sample_correlation(df)
#'
#' @importFrom grDevices colorRampPalette
#'
#' @export
plot_sample_correlation <- function(numeric_data) {
  # Ensure the data is numeric
  if (!all(sapply(numeric_data, is.numeric))) {
    stop("The data must be numeric.")
  }

  # Compute correlation matrix
  cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

  # Plot correlation heatmap
  pheatmap::pheatmap(cor_matrix,
                     clustering_distance_rows = "euclidean",
                     clustering_distance_cols = "euclidean",
                     clustering_method = "complete",
                     color = colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(100),
                     main = "Sample Correlation Heatmap")
}
