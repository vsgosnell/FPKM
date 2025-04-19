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
