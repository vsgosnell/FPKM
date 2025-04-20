#' Plot Expression Heatmap for Top N Genes
#'
#' Generates a heatmap for the top N expressed genes.
#'
#' @param fpkm_data A data frame of FPKM values.
#' @param top_n The number of top expressed genes to plot.
#'
#' @return A heatmap plot.
#' @export
plot_expression_heatmap <- function(fpkm_data, top_n = 20) {
  # Keep only numeric columns
  numeric_data <- fpkm_data[, sapply(fpkm_data, is.numeric), drop = FALSE]

  # Select top N genes by mean expression
  gene_means <- rowMeans(numeric_data)
  top_genes <- order(gene_means, decreasing = TRUE)[1:min(top_n, nrow(fpkm_data))]

  mat <- as.matrix(numeric_data[top_genes, , drop = FALSE])

  if (!is.numeric(mat)) stop("Heatmap matrix must be numeric")

  pheatmap::pheatmap(mat, scale = "row", cluster_rows = TRUE, cluster_cols = TRUE)
}
