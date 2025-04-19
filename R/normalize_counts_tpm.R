#' Normalize Counts to TPM
#'
#' Normalize gene expression data (counts) to Transcripts Per Million (TPM).
#'
#' @param counts A vector of gene counts.
#' @param lengths A vector of gene lengths.
#'
#' @return A vector of TPM values.
#' @export
normalize_counts_tpm <- function(counts, lengths) {
  # Normalize counts to TPM
  lengths_kb <- lengths / 1000  # Convert lengths to kilobases
  rpk <- sweep(counts, 1, lengths_kb, FUN = "/")  # RPK calculation

  # TPM scaling factor: total sum of RPKs across all samples
  scaling_factor <- sweep(rpk, 2, colSums(rpk) / 1e6, FUN = "/")  # Normalize to TPM
  return(scaling_factor)
}
