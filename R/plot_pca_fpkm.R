#' Plot PCA of FPKM Values
#'
#' Generates a PCA plot based on FPKM values for all genes.
#'
#' @param fpkm_data A data frame of FPKM values (genes as rows, samples as columns).
#' @return A PCA plot.
#' @export
plot_pca_fpkm <- function(fpkm_data) {
  # Log2 transform the FPKM data (adding 1 to avoid log(0))
  log_fpkm <- log2(fpkm_data + 1)

  # Transpose so samples are rows for PCA
  pca_result <- prcomp(t(log_fpkm), scale. = TRUE)

  # Prepare PCA data frame
  pca_df <- data.frame(pca_result$x)
  pca_df$Sample <- rownames(pca_df)

  # Plot
  ggplot(pca_df, aes(x = PC1, y = PC2, label = Sample)) +
    geom_point(size = 4, color = "steelblue") +
    geom_text(nudge_y = 1.5, size = 3) +
    theme_minimal() +
    labs(title = "PCA of FPKM Data", x = "PC1", y = "PC2")
}
