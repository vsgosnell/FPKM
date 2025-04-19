#' Filter Low Expression Genes
#'
#' Filters out genes that have low expression across all samples.
#'
#' @param fpkm_data A data frame of FPKM values.
#' @param threshold The minimum average FPKM value to retain a gene.
#'
#' @return A data frame of FPKM values for genes with average expression above the threshold.
#' @export
filter_low_expression <- function(fpkm_data, threshold = 1) {
  avg_fpkm <- rowMeans(fpkm_data)
  filtered_data <- fpkm_data[avg_fpkm >= threshold, ]
  return(filtered_data)
}
