#' Plot Sample Correlation Heatmap
#'
#' Generates a heatmap showing the correlation between samples based on FPKM values.
#'
#' @param fpkm_data A data frame of FPKM values.
#'
#' @return A heatmap plot.
#' @export
# Ensure the data is numeric (ignoring any non-numeric columns like Gene)
log_fpkm_numeric <- log_fpkm %>%
  select(-Gene) %>%  # Exclude the Gene column
  mutate(across(everything(), as.numeric))  # Convert all columns to numeric

plot_sample_correlation <- function(log_fpkm) {
  # Ensure all columns (except Gene) are numeric
  log_fpkm_numeric <- log_fpkm %>%
    select(-Gene) %>%
    mutate(across(everything(), as.numeric))  # Convert all columns to numeric

  # Check if there are any non-numeric values
  if (anyNA(log_fpkm_numeric)) {
    warning("There are NA values in the numeric data!")
  }

  # Compute correlation matrix (transpose the data to get samples as rows)
  correlation_matrix <- cor(t(log_fpkm_numeric))  # Transpose to get samples as rows

  # Plot correlation matrix as a heatmap
  library(ggplot2)
  library(reshape2)

  # Reshape for ggplot
  correlation_data <- melt(correlation_matrix)

  # Plot the heatmap
  ggplot(correlation_data, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(midpoint = 0, low = "blue", high = "red", mid = "white") +
    theme_minimal() +
    labs(title = "Sample Correlation Heatmap", x = "Sample", y = "Sample") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Call the function
plot_sample_correlation(log_fpkm)



#' Plot FPKM Distribution Boxplot
#'
#' Generates a boxplot of FPKM values across samples.
#'
#' @param fpkm_data A data frame of FPKM values.
#'
#' @return A boxplot of FPKM distribution.
#' @export
library(tidyr)
library(ggplot2)

plot_fpkm_boxplot <- function(log_fpkm) {
  # Exclude the "Gene" column for the check
  counts_only <- log_fpkm[, !colnames(log_fpkm) %in% "Gene"]

  # Ensure log_fpkm has the correct number of columns (one for each sample)
  if (ncol(counts_only) != 4) {  # Adjust the number to match your dataset
    stop("log_fpkm has an unexpected number of columns.")
  }

  # Reshape the data for ggplot
  fpkm_data_long <- log_fpkm %>%
    rownames_to_column("Gene") %>%
    pivot_longer(cols = -Gene, names_to = "Sample", values_to = "FPKM")

  # Check the reshaped data
  cat("First few rows of reshaped data:\n")
  print(head(fpkm_data_long))

  # Create the boxplot
  ggplot(fpkm_data_long, aes(x = Sample, y = FPKM)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "FPKM Boxplot", x = "Sample", y = "FPKM")
}

# Call the function to plot
plot_fpkm_boxplot(log_fpkm)





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
plot_pca_fpkm <- function(log_fpkm) {
  # Ensure that the data is numeric (ignoring non-numeric columns such as rownames)
  log_fpkm_numeric <- log_fpkm %>%
    select(-Gene) %>% # Exclude the Gene column if it exists
    as.data.frame()

  # Perform PCA
  pca_result <- prcomp(log_fpkm_numeric, scale. = TRUE)

  # Create a data frame for plotting
  pca_data <- data.frame(pca_result$x)

  # Plot PCA
  ggplot(pca_data, aes(x = PC1, y = PC2)) +
    geom_point(aes(color = rownames(pca_data))) +
    theme_minimal() +
    labs(title = "PCA of FPKM Data", x = "PC1", y = "PC2")
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

