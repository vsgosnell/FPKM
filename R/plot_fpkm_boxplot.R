#' Plot FPKM Distribution Boxplot
#'
#' Generates a boxplot of FPKM values across samples.
#'
#' @param log_fpkm A data frame of log-transformed FPKM values.
#'
#' @return A ggplot2 boxplot.
#' @export
plot_fpkm_boxplot <- function(fpkm_data) {

  library(tidyr)
  library(ggplot2)

  log_fpkm <- log2(fpkm_data + 1)

  # Reshape to long format
  long_fpkm <- reshape2::melt(as.matrix(log_fpkm))
  colnames(long_fpkm) <- c("Gene", "Sample", "logFPKM")

  ggplot2::ggplot(long_fpkm, ggplot2::aes(x = Sample, y = logFPKM)) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "FPKM Expression Boxplot", y = "log2(FPKM + 1)")
}
