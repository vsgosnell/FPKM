#' Plot FPKM Distribution Boxplot
#'
#' Generates a boxplot of FPKM values across samples.
#'
#' @param fpkm_data A data frame of FPKM values (genes as rows, samples as columns).
#'
#' @return A ggplot2 boxplot.
#' @export
plot_fpkm_boxplot <- function(fpkm_data) {
  library(tidyr)
  library(ggplot2)

  numeric_data <- fpkm_data[, sapply(fpkm_data, is.numeric)]
  numeric_data <- numeric_data[, !grepl("FPKM", colnames(numeric_data))]

  if (ncol(numeric_data) == 0) {
    stop("No numeric sample columns found in the data after excluding 'FPKM'.")
  }

  log_fpkm <- log2(numeric_data + 1)
  print("Head of log-transformed data:")
  print(head(log_fpkm))

  long_fpkm <- reshape2::melt(log_fpkm)
  print("Head of reshaped data:")
  print(head(long_fpkm))

  if (ncol(long_fpkm) != 3) {
    stop("The reshaped data does not have 3 columns. Check the reshaping process.")
  }

  colnames(long_fpkm) <- c("Gene", "Sample", "logFPKM")

  ggplot2::ggplot(long_fpkm, ggplot2::aes(x = Sample, y = logFPKM)) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "FPKM Expression Boxplot", y = "log2(FPKM + 1)") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
