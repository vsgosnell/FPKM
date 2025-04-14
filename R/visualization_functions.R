#' Plot Sample Correlation Heatmap
#'
#' Generates a heatmap showing the correlation between samples based on FPKM values.
#'
#' @param fpkm_data A data frame of FPKM values.
#'
#' @return A heatmap plot.
#' @export
plot_sample_correlation <- function(fpkm_data) {
  library(pheatmap)
  sample_correlations <- cor(t(fpkm_data))  # Correlation between samples
  pheatmap(sample_correlations, main = "Sample Correlation Heatmap")
}

#' Plot FPKM Distribution Boxplot
#'
#' Generates a boxplot of FPKM values across samples.
#'
#' @param fpkm_data A data frame of FPKM values.
#'
#' @return A boxplot of FPKM distribution.
#' @export
plot_fpkm_boxplot <- function(fpkm_data) {
  library(ggplot2)
  fpkm_data_long <- reshape2::melt(fpkm_data)
  colnames(fpkm_data_long) <- c("Gene", "Sample", "FPKM")

  ggplot(fpkm_data_long, aes(x = Sample, y = FPKM)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "FPKM Distribution Across Samples")
}

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

#' Plot PCA of FPKM Values
#'
#' Generates a PCA plot based on FPKM values for all genes.
#'
#' @param fpkm_data A data frame of FPKM values.
#'
#' @return A PCA plot.
#' @export
plot_pca_fpkm <- function(fpkm_data) {
  library(ggplot2)
  pca_result <- prcomp(t(fpkm_data), scale. = TRUE)
  pca_data <- data.frame(PC1 = pca_result$x[, 1], PC2 = pca_result$x[, 2])

  ggplot(pca_data, aes(x = PC1, y = PC2)) +
    geom_point() +
    theme_minimal() +
    labs(title = "PCA of FPKM Values")
}

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

