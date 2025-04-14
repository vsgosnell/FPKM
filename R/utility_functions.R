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

#' Merge Multiple FPKM Tables
#'
#' Merges multiple FPKM tables (e.g., replicates or conditions) into a single table.
#'
#' @param fpkm_list A list of data frames with FPKM values.
#'
#' @return A merged data frame.
#' @export
merge_fpkm_tables <- function(fpkm_list) {
  merged_data <- Reduce(function(x, y) merge(x, y, by = "Gene"), fpkm_list)
  return(merged_data)
}

#' Get Top Expressed Genes
#'
#' Retrieves the top N most highly expressed genes.
#'
#' @param fpkm_data A data frame of FPKM values.
#' @param top_n The number of top expressed genes to return.
#'
#' @return A data frame of the top expressed genes.
#' @export
get_top_expressed_genes <- function(log_fpkm, n = 50) {
  # Calculate the mean expression across samples for each gene
  gene_means <- rowMeans(log_fpkm, na.rm = TRUE)

  # Sort the genes by mean expression in descending order
  sorted_genes <- sort(gene_means, decreasing = TRUE)

  # Get the top 'n' genes
  top_genes <- names(sorted_genes)[1:n]

  return(top_genes)
}


