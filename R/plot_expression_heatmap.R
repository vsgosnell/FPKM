#' Plot Expression Heatmap
#'
#' @param fpkm_data A data frame or matrix of FPKM values (genes as rows, samples as columns).
#' @return A pheatmap object.
#' @export
plot_expression_heatmap <- function(fpkm_data) {

  # Ensure the data is numeric (force coercion if necessary)
  if (is.data.frame(fpkm_data)) {
    numeric_data <- fpkm_data[, sapply(fpkm_data, is.numeric)]
    # Coerce to matrix if it's still a data frame
    fpkm_data <- as.matrix(numeric_data)
  }

  # Force coercion to numeric just in case
  fpkm_data[] <- lapply(fpkm_data, as.numeric)

  # Check if all columns are numeric
  if (!all(sapply(fpkm_data, is.numeric))) {
    stop("All columns must be numeric.")
  }

  # Check for NAs in the matrix
  if (any(is.na(fpkm_data))) {
    warning("Matrix contains NA values. They will be imputed.")
    # Impute NAs with column means
    fpkm_data[is.na(fpkm_data)] <- colMeans(fpkm_data, na.rm = TRUE)[col(fpkm_data)[is.na(fpkm_data)]]
  }

  # Ensure row and column names are present
  if (is.null(rownames(fpkm_data)) || is.null(colnames(fpkm_data))) {
    stop("Row or column names are missing.")
  }

  # Debugging: Print matrix structure before scaling
  print("Pre-scaling matrix:")
  print(str(fpkm_data))  # Check the structure

  # Test on a small subset (e.g., first 10 rows and columns)
  print("Testing on a subset of data (first 10 rows and columns):")
  print(fpkm_data[1:10, 1:4])  # Check a small subset

  # Scale the matrix manually (center and scale)
  scaled_matrix <- scale(fpkm_data, center = TRUE, scale = TRUE)

  # Debugging: Print matrix structure after scaling
  print("Scaled matrix:")
  print(str(scaled_matrix))  # Check the structure

  # Test again on a small subset (first 10 rows and columns) after scaling
  print("Testing scaled matrix on subset (first 10 rows and columns):")
  print(scaled_matrix[1:10, 1:4])  # Check the first 10 rows and columns

  # Plot the heatmap using pheatmap
  pheatmap::pheatmap(scaled_matrix,
                     scale = "none",  # Already scaled manually
                     clustering_distance_rows = "euclidean",
                     clustering_distance_cols = "euclidean",
                     clustering_method = "complete",
                     color = colorRampPalette(RColorBrewer::brewer.pal(9, "Blues"))(100),
                     labels_row = NA)  # Turn off labels if needed
}
