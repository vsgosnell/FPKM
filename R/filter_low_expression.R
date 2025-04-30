#' Filter Low Expression Genes
#'
#' Filters out genes (rows) with mean FPKM below a specified threshold.
#'
#' @param fpkm_data A data frame or matrix of FPKM values (genes x samples)
#' @param threshold A numeric value; genes with mean FPKM below this will be removed
#'
#' @return Filtered FPKM data with all original sample columns retained
#' @export
filter_low_expression <- function(fpkm_data, threshold = 1) {
  gene_means <- rowMeans(fpkm_data, na.rm = TRUE)
  fpkm_data[gene_means >= threshold, , drop = FALSE]
}
