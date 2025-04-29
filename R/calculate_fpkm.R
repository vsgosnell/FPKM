#' Calculate FPKM Using For Loop
#'
#' This function calculates FPKM values for each gene in each sample using a for loop.
#'
#' @param data A data.frame containing RNA-seq data with gene lengths and counts.
#' @param count_cols A character vector specifying the column names that contain the raw count data.
#'
#' @return A matrix of FPKM values with gene names as rownames and sample names as colnames.
#' @export
calculate_fpkm <- function(data, count_cols) {
  # Extract counts for each sample
  counts <- data[, count_cols]

  # Extract the Length column
  lengths <- data$length

  # Ensure lengths are in the correct format
  if(length(lengths) != nrow(counts)) {
    stop("Mismatch between the number of genes (rows) and the length data.")
  }

  # Convert lengths to kilobases
  lengths_kb <- lengths / 1000

  # Sum of counts for each sample
  total_counts_per_sample <- colSums(counts)

  # FPKM calculation
  fpkm <- sweep(counts, 2, total_counts_per_sample / 1e6, FUN = "/")
  fpkm <- sweep(fpkm, 1, lengths_kb, FUN = "/")

  # Preserve gene names if available
  rownames(fpkm) <- data$GeneID

  return(fpkm)
}
