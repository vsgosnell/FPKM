#' Plot FPKM Boxplot
#'
#' This function takes a wide-format FPKM matrix and plots the distribution per sample.
#'
#' @param fpkm_data A data frame or matrix of FPKM values (genes x samples).
#'
#' @return A ggplot2 boxplot object.
#' @export
plot_fpkm_boxplot <- function(fpkm_data) {
  library(ggplot2)
  library(reshape2)

  if (!is.data.frame(fpkm_data) && !is.matrix(fpkm_data)) {
    stop("Input must be a data frame or matrix.")
  }

  # Convert to data.frame
  fpkm_data <- as.data.frame(fpkm_data)

  # Log-transform if needed
  log_fpkm <- log2(fpkm_data + 1)

  # Melt into long format
  long_data <- reshape2::melt(log_fpkm)

  # Rename columns
  colnames(long_data) <- c("Sample", "logFPKM")

  # Plot
  p <- ggplot(long_data, aes(x = Sample, y = logFPKM)) +
    geom_boxplot(fill = "#D6EAF8", color = "#2E86C1", outlier.color = "#C0392B", outlier.size = 1.5) +
    theme_minimal(base_size = 14) +
    labs(title = "Distribution of log2(FPKM + 1)",
         x = "Sample", y = "log2(FPKM + 1)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  return(p)
}
