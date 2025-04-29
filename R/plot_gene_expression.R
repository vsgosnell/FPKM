#' Plot Gene Expression Over Samples
#'
#' Plots the expression of a specific gene across different samples.
#'
#' @param fpkm_data A data frame of FPKM values.
#' @param gene The gene of interest.
#'
#' @return A bar plot of the gene expression.
#' @export
plot_gene_expression <- function(fpkm_data, gene) {
  if (!gene %in% rownames(fpkm_data)) {
    stop(sprintf("Gene '%s' not found in row names", gene))
  }

  expr <- as.numeric(fpkm_data[gene, ])
  df <- data.frame(Sample = colnames(fpkm_data), Expression = expr)

  ggplot2::ggplot(df, ggplot2::aes(x = Sample, y = Expression)) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste("Expression of", gene), y = "FPKM")
}

