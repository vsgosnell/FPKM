#' Read RNA-seq Data File
#'
#' @param file_path Path to RNA-seq data file
#' @return A data frame with GeneID, Counts, and Length_kb
#' @export
read_rna_seq_data <- function(file_path) {
  fread(file_path) %>%
    rename(GeneID = V1, Counts = V2, Length_kb = V3)
}
