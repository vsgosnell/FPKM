#' PCA Plot of FPKM Matrix
#'
#' This function performs PCA on a log2-transformed FPKM matrix.
#'
#' @param fpkm_data A numeric matrix or data frame with genes as rows and samples as columns.
#'
#' @return A ggplot2 PCA plot object.
#' @export
plot_pca_fpkm <- function(numeric_data) {
  # Check if 'Gene' column exists
  if("Gene" %in% colnames(numeric_data)) {
    log_fpkm <- numeric_data %>% select(-Gene)  # Remove 'Gene' if it exists
  } else {
    log_fpkm <- numeric_data  # Skip the removal if 'Gene' is not in the data
  }

  # Proceed with PCA as usual
  pca_result <- stats::prcomp(t(log_fpkm), scale. = TRUE)
  pca_df <- as.data.frame(pca_result$x)
  pca_df$Sample <- rownames(pca_df)

  ggplot2::ggplot(pca_df, ggplot2::aes(x = PC1, y = PC2, color = Sample)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::labs(title = "PCA of FPKM Values", x = "Principal Component 1", y = "Principal Component 2") +
    ggplot2::theme_minimal()
}
