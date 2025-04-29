#' PCA Plot of FPKM Matrix
#'
#' This function performs PCA on a log2-transformed FPKM matrix, removing non-sample columns like "Gene" or "FPKM".
#'
#' @param numeric_data A numeric matrix or data frame with genes as rows and samples as columns.
#'
#' @return A ggplot2 PCA plot object.
#' @export
plot_pca_fpkm <- function(numeric_data) {
  # Remove common non-sample columns like 'Gene' or 'FPKM'
  cols_to_remove <- c("Gene", "FPKM")
  data_clean <- numeric_data[, !(colnames(numeric_data) %in% cols_to_remove), drop = FALSE]

  # Log2 transform (adding 1 to avoid log(0))
  log_fpkm <- log2(data_clean + 1)

  # Remove any rows with missing values
  log_fpkm <- log_fpkm[complete.cases(log_fpkm), ]

  # Perform PCA
  pca_result <- stats::prcomp(t(log_fpkm), scale. = TRUE)
  pca_df <- as.data.frame(pca_result$x)
  pca_df$Sample <- rownames(pca_df)

  # Generate PCA plot
  ggplot2::ggplot(pca_df, ggplot2::aes(x = PC1, y = PC2, color = Sample)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::labs(
      title = "PCA of log2(FPKM + 1) Expression",
      x = "Principal Component 1",
      y = "Principal Component 2"
    ) +
    ggplot2::theme_minimal()
}
