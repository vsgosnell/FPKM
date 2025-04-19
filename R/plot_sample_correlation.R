#' Plot Sample Correlation Heatmap
#'
#' Generates a heatmap showing the correlation between samples based on FPKM values.
#'
#' @param fpkm_data A data frame of FPKM values.
#'
#' @return A heatmap plot.
#' @export
plot_sample_correlation <- function(log_fpkm) {
  # Ensure all columns (except Gene) are numeric
  log_fpkm_numeric <- log_fpkm %>%
    select(-Gene) %>%
    mutate(across(everything(), as.numeric))  # Convert all columns to numeric

  # Check if there are any non-numeric values
  if (anyNA(log_fpkm_numeric)) {
    warning("There are NA values in the numeric data!")
  }

  # Compute correlation matrix (transpose the data to get samples as rows)
  correlation_matrix <- cor(t(log_fpkm_numeric))  # Transpose to get samples as rows

  # Plot correlation matrix as a heatmap
  library(ggplot2)
  library(reshape2)

  # Reshape for ggplot
  correlation_data <- melt(correlation_matrix)

  # Plot the heatmap
  ggplot(correlation_data, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(midpoint = 0, low = "blue", high = "red", mid = "white") +
    theme_minimal() +
    labs(title = "Sample Correlation Heatmap", x = "Sample", y = "Sample") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Call the function
# plot_sample_correlation(log_fpkm)
