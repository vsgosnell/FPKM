#' Summarize Expression Data
#'
#' Summarizes gene expression data with mean, median, and variance.
#'
#' @param fpkm_data A data frame of FPKM values.
#'
#' @return A data frame with the summary statistics for each gene.
#' @export
summarize_expression <- function(log_fpkm) {
  # Check if the data is a data frame or matrix
  if (!is.data.frame(log_fpkm) && !is.matrix(log_fpkm)) {
    stop("log_fpkm must be a data frame or matrix")
  }

  # Calculate mean and variance for each gene
  gene_stats <- data.frame(
    Gene = rownames(log_fpkm),
    Mean = rowMeans(log_fpkm),
    Variance = apply(log_fpkm, 1, var)
  )

  # Return the data frame with summary stats
  return(gene_stats)
}
