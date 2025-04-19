#' Plot FPKM Distribution Boxplot
#'
#' Generates a boxplot of FPKM values across samples.
#'
#' @param log_fpkm A data frame of log-transformed FPKM values.
#'
#' @return A ggplot2 boxplot.
#' @export
plot_fpkm_boxplot <- function(log_fpkm) {
  library(tidyr)
  library(ggplot2)

  counts_only <- log_fpkm[, !colnames(log_fpkm) %in% "Gene"]

  if (ncol(counts_only) != 4) {
    stop("log_fpkm has an unexpected number of columns.")
  }

  fpkm_data_long <- log_fpkm %>%
    tibble::rownames_to_column("Gene") %>%
    pivot_longer(cols = -Gene, names_to = "Sample", values_to = "FPKM")

  ggplot(fpkm_data_long, aes(x = Sample, y = FPKM)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "FPKM Boxplot", x = "Sample", y = "FPKM")
}
