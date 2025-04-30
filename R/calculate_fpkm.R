#' Calculate FPKM
#'
#' Computes Fragments Per Kilobase of transcript per Million mapped reads.
#'
#' @param data A data frame containing counts and a "Length" column (case-insensitive)
#' @param count_cols A character vector of column names representing sample count data
#'
#' @return A data frame of FPKM values
#' @export
calculate_fpkm <- function(data, count_cols = NULL) {
  # Auto-detect count columns if not provided
  if (is.null(count_cols)) {
    count_cols <- setdiff(colnames(data), c("Geneid", "geneid", "Chr", "chr",
                                            "Start", "start", "End", "end",
                                            "Strand", "strand", "Length", "length"))
  }

  # Ensure count columns are numeric
  counts <- data[, count_cols, drop = FALSE]
  if (!all(sapply(counts, is.numeric))) {
    stop("All count columns must be numeric.")
  }

  # Find the Length column safely
  if ("Length" %in% colnames(data)) {
    gene_lengths <- data$Length
  } else if ("length" %in% colnames(data)) {
    gene_lengths <- data$length
  } else {
    stop("No Length or length column found in data.")
  }

  # ---- New: Handle mismatches automatically ----
  if (length(gene_lengths) != nrow(counts)) {
    min_length <- min(length(gene_lengths), nrow(counts))
    counts <- counts[seq_len(min_length), , drop = FALSE]
    gene_lengths <- gene_lengths[seq_len(min_length)]
  }
  # -----------------------------------------------

  # Calculate library sizes
  library_sizes_millions <- colSums(counts) / 1e6

  # Calculate gene lengths in kilobases
  gene_lengths_kb <- gene_lengths / 1000

  # Calculate FPKM
  fpkm_matrix <- sweep(counts, 2, library_sizes_millions, "/")
  fpkm_matrix <- sweep(fpkm_matrix, 1, gene_lengths_kb, "/")

  # Return as data frame
  fpkm_df <- as.data.frame(fpkm_matrix)
  rownames(fpkm_df) <- NULL

  return(fpkm_df)
}
