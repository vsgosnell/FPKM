#' Plot Expression Heatmap
#'
#' @param fpkm_data A data frame of FPKM values (genes as rows, samples as columns).
#'
#' @return A pheatmap object.
#' @export
plot_expression_heatmap <- function(fpkm_data) {
  # Ensure only numeric columns are passed to pheatmap
  numeric_data <- fpkm_data[, sapply(fpkm_data, is.numeric)]

  # If there are no numeric columns, stop and notify
  if (ncol(numeric_data) == 0) {
    stop("No numeric columns found in the data.")
  }

  # Convert to matrix for pheatmap
  expression_matrix <- as.matrix(numeric_data)

  # Plot heatmap
  pheatmap::pheatmap(expression_matrix,
                     scale = "row",
                     clustering_distance_rows = "euclidean",
                     clustering_distance_cols = "euclidean",
                     clustering_method = "complete",
                     color = colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(100))
}
