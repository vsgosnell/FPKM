#' Plot Sample Correlation Heatmap
#'
#' @param numeric_data A data frame or matrix of numeric FPKM values (genes as rows, samples as columns).
#' @return A pheatmap object.
#' @export
plot_sample_correlation <- function(numeric_data) {
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
