#' Plot FPKM Distribution
#'
#' @param fpkm_values Numeric vector of FPKM values
#' @return A ggplot histogram
#' @export
plot_fpkm_distribution <- function(fpkm_values) {
  ggplot2::ggplot(data.frame(FPKM = fpkm_values), aes(x = FPKM)) +
    ggplot2::geom_histogram(bins = 50, fill = "blue", alpha = 0.6) +
    scale_x_log10() +
    ggplot2::labs(title = "FPKM Distribution", x = "FPKM", y = "Count") +
    ggplot2::theme_minimal()
}
