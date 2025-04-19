#' Plot Expression Heatmap for Top N Genes
#'
#' Generates a heatmap for the top N expressed genes.
#'
#' @param fpkm_data A data frame of FPKM values.
#' @param top_n The number of top expressed genes to plot.
#'
#' @return A heatmap plot.
#' @export
plot_expression_heatmap <- function(fpkm_data, top_n = 10) {
  library(pheatmap)
  top_genes <- get_top_expressed_genes(fpkm_data, top_n)
  pheatmap(top_genes, scale = "row", main = paste("Top", top_n, "Expressed Genes"))
}
