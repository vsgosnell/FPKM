#' Plot FPKM Distribution Boxplot
#'
#' Generates a boxplot of FPKM values across samples.
#'
#' @param fpkm_data A data frame of FPKM values (genes as rows, samples as columns).
#'
#' @return A ggplot2 boxplot.
#' @export
plot_fpkm_boxplot <- function(fpkm_data) {
  library(ggplot2)

  # Ensure that the data is numeric
  numeric_data <- fpkm_data[, sapply(fpkm_data, is.numeric)]

  # Log-transform the data
  log_fpkm <- log2(numeric_data + 1)

  # Create a long-format data frame
  long_fpkm <- data.frame(
    Gene = rep(rownames(log_fpkm), times = ncol(log_fpkm)),
    Sample = rep(colnames(log_fpkm), each = nrow(log_fpkm)),
    logFPKM = as.vector(log_fpkm)
  )

  # Print the first few rows of the reshaped data to check
  print("Head of reshaped data:")
  print(head(long_fpkm))

  # Plot the boxplot
  ggplot(long_fpkm, aes(x = Sample, y = logFPKM)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "FPKM Expression Boxplot", y = "log2(FPKM + 1)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
