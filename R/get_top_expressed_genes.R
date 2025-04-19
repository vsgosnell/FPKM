#' Get Top Expressed Genes
#'
#' Retrieves the top N most highly expressed genes.
#'
#' @param fpkm_data A data frame of FPKM values.
#' @param top_n The number of top expressed genes to return.
#'
#' @return A data frame of the top expressed genes.
#' @export
get_top_expressed_genes <- function(log_fpkm, n = 50) {
  # Calculate the mean expression across samples for each gene
  gene_means <- rowMeans(log_fpkm, na.rm = TRUE)

  # Sort the genes by mean expression in descending order
  sorted_genes <- sort(gene_means, decreasing = TRUE)

  # Get the top 'n' genes
  top_genes <- names(sorted_genes)[1:n]

  return(top_genes)
}
