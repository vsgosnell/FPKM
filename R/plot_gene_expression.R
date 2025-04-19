#' Plot Gene Expression Over Samples
#'
#' Plots the expression of a specific gene across different samples.
#'
#' @param fpkm_data A data frame of FPKM values.
#' @param gene The gene of interest.
#' @param plot_type The type of plot ("bar" or "line").
#'
#' @return A plot of the gene expression.
#' @export
plot_gene_expression <- function(fpkm_data, gene, plot_type = "bar") {
  library(ggplot2)
  gene_data <- fpkm_data[gene, ]
  gene_data <- data.frame(Sample = colnames(fpkm_data), FPKM = gene_data)

  ggplot(gene_data, aes(x = Sample, y = FPKM)) +
    (if (plot_type == "bar") geom_bar(stat = "identity") else geom_line()) +
    theme_minimal() +
    labs(title = paste("Expression of", gene))
}
