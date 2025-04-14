#remove all variables from environment
rm(list = ls())

#' Calculate FPKM
#'
#' @param counts Numeric vector of read counts
#' @param lengths Numeric vector of gene lengths (in kilobases)
#' @return Numeric vector of FPKM values
#' @export
calculate_fpkm <- function(counts, lengths) {
  # Ensure inputs are correct
  if (!is.data.frame(counts) && !is.matrix(counts)) {
    stop("Counts must be a data.frame or matrix.")
  }

  if (nrow(counts) != length(lengths)) {
    stop(paste("Mismatch: counts has", nrow(counts), "rows, but lengths has", length(lengths), "entries."))
  }

  # Convert lengths to kilobases
  lengths_kb <- lengths / 1000

  # Sum of counts for each sample (column-wise)
  total_counts_per_sample <- colSums(counts)

  # FPKM calculation
  fpkm <- sweep(counts, 2, total_counts_per_sample / 1e6, FUN = "/") # per million
  fpkm <- sweep(fpkm, 1, lengths_kb, FUN = "/")  # per kb

  return(fpkm)
}





