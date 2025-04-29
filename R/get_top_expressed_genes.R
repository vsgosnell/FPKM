#' Get Top Expressed Genes
#'
#' Retrieves the top N most highly expressed genes based on average expression across samples.
#'
#' @param fpkm_data A data frame or matrix of (optionally log-transformed) FPKM values, where rows are genes and columns are samples.
#' @param top_n The number of top expressed genes to return (default is 50).
#'
#' @return A character vector of gene names corresponding to the top expressed genes.
#' @export
get_top_expressed_genes <- function(fpkm_data, top_n = 50) {
  # Calculate the mean expression across samples for each gene
  gene_means <- rowMeans(fpkm_data, na.rm = TRUE)

  # Sort the genes by mean expression in descending order
  sorted_genes <- sort(gene_means, decreasing = TRUE)

  # Get the top 'n' genes
  top_genes <- names(sorted_genes)[1:min(top_n, length(sorted_genes))]

  return(top_genes)
}
