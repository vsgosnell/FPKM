#' Plot Expression Heatmap
#'
#' This function takes a matrix or data frame of FPKM values and visualizes a heatmap of the most variable genes.
#'
#' @param fpkm_data A data frame or matrix where rows represent genes and columns represent samples.
#' @param top_n An integer specifying the number of top genes (by variance) to display (default 20).
#'
#' @return A heatmap plot of the scaled expression values.
#' @export
plot_expression_heatmap <- function(fpkm_data, top_n = 20) {
  if (is.data.frame(fpkm_data)) {
    fpkm_data <- as.matrix(fpkm_data)
  }

  # Check for row and column names BEFORE proceeding
  if (is.null(rownames(fpkm_data)) || is.null(colnames(fpkm_data))) {
    stop("Row or column names are missing.")
  }

  # Make sure everything is numeric, but KEEP rownames
  storage.mode(fpkm_data) <- "numeric"

  # Check and optionally handle NA values
  if (any(is.na(fpkm_data))) {
    warning("Matrix contains NA values. Imputing with column means.")
    for (j in seq_len(ncol(fpkm_data))) {
      fpkm_data[is.na(fpkm_data[, j]), j] <- mean(fpkm_data[, j], na.rm = TRUE)
    }
  }

  # Find top N variable genes
  gene_variances <- apply(fpkm_data, 1, var, na.rm = TRUE)
  top_genes <- order(gene_variances, decreasing = TRUE)[seq_len(min(top_n, length(gene_variances)))]

  fpkm_data_top <- fpkm_data[top_genes, , drop = FALSE]

  # Scale by row
  scaled_matrix <- scale(fpkm_data_top, center = TRUE, scale = TRUE)

  # Plot heatmap
  pheatmap::pheatmap(scaled_matrix,
                     scale = "none",
                     clustering_distance_rows = "euclidean",
                     clustering_distance_cols = "euclidean",
                     clustering_method = "complete",
                     color = colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(100),
                     labels_row = rownames(scaled_matrix))
}
