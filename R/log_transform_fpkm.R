#' Log-Transform FPKM Values
#'
#' Applies a log2 transformation to FPKM values.
#'
#' @param fpkm_data A data frame of FPKM values.
#' @param pseudocount A pseudocount to avoid log(0), default is 1.
#'
#' @return A data frame of log-transformed FPKM values.
#' @export
log_transform_fpkm <- function(fpkm_data, pseudocount = 1) {
  # Apply log2 transformation with a pseudocount to avoid log(0)
  log_fpkm <- log2(fpkm_data + pseudocount)
  return(log_fpkm)
}
