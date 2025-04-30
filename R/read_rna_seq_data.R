#' Read RNA-seq Data
#'
#' Reads an RNA-seq count file, handles headers, ensures correct formatting.
#'
#' @param filepath Path to the RNA-seq counts file (.tsv or .txt)
#'
#' @return A data frame containing gene identifiers, lengths, and sample counts
#' @export
read_rna_seq_data <- function(filepath) {
  data <- readr::read_tsv(filepath)

  # If 'length' column exists but not 'Length', fix it
  if ("length" %in% colnames(data) && !"Length" %in% colnames(data)) {
    colnames(data)[colnames(data) == "length"] <- "Length"
  }

  return(data)
}
