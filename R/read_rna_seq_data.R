#' Read RNA-seq Data
#'
#' This function reads RNA-seq data from a file and processes it by renaming columns if necessary.
#'
#' @param file_path A string representing the path to the RNA-seq data file.
#' @return A data frame containing the processed RNA-seq data.
#' @export
read_rna_seq_data <- function(file_path) {
  # Load necessary libraries
  library(data.table)
  library(dplyr)

  # Read the data
  data <- fread(file_path)

  # Print column names to inspect the structure
  print(colnames(data))

  # Check and rename columns only if necessary
  data <- data %>%
    rename_with(~ c("GeneID", "Chr", "Start", "End", "Strand", "Length",
                    "vehicle_rep1", "vehicle_rep2", "drug_rep1", "drug_rep2"),
                everything())  # Rename all columns to the desired names

  # Return the processed data
  return(data)
}
