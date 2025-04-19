#' Summarize Expression Data
#'
#' Summarizes gene expression data with mean, median, and variance.
#'
#' @param fpkm_data A data frame of FPKM values.
#'
#' @return A data frame with the summary statistics for each gene.
#' @export
summarize_expression <- function(fpkm_data) {
  summary_stats <- data.frame(
    mean = rowMeans(fpkm_data),
    median = apply(fpkm_data, 1, median),
    variance = apply(fpkm_data, 1, var)
  )
  return(summary_stats)
}
