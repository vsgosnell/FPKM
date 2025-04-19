#' Plot FPKM Distribution Boxplot
#'
#' Generates a boxplot of FPKM values across samples.
#'
#' @param fpkm_data A data frame of FPKM values.
#'
#' @return A boxplot of FPKM distribution.
#' @export
library(tidyr)
library(ggplot2)

plot_fpkm_boxplot <- function(log_fpkm) {
  # Exclude the "Gene" column for the check
  counts_only <- log_fpkm[, !colnames(log_fpkm) %in% "Gene"]

  # Ensure log_fpkm has the correct number of columns (one for each sample)
  if (ncol(counts_only) != 4) {  # Adjust the number to match your dataset
    stop("log_fpkm has an unexpected number of columns.")
  }

  # Reshape the data for ggplot
  fpkm_data_long <- log_fpkm %>%
    rownames_to_column("Gene") %>%
    pivot_longer(cols = -Gene, names_to = "Sample", values_to = "FPKM")

  # Check the reshaped data
  cat("First few rows of reshaped data:\n")
  print(head(fpkm_data_long))

  # Create the boxplot
  ggplot(fpkm_data_long, aes(x = Sample, y = FPKM)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "FPKM Boxplot", x = "Sample", y = "FPKM")
}

# Call the function to plot
plot_fpkm_boxplot(log_fpkm)
