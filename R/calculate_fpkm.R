#remove all variables from environment
rm(list = ls())

#' Calculate FPKM
#'
#' @param counts Numeric vector of read counts
#' @param lengths Numeric vector of gene lengths (in kilobases)
#' @return Numeric vector of FPKM values
#' @export
calculate_fpkm <- function(counts, lengths) {
  if (length(counts) != length(lengths)) {
    stop("Counts and lengths must be the same length.")
  }
  total_counts <- sum(counts)
  fpkm <- (counts / lengths) / (total_counts / 1e9)
  return(fpkm)
}

