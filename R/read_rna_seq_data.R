#' Read RNA-seq Data
#'
#' This function reads RNA-seq data from a file, standardizes annotation columns,
#' and preserves all sample columns without hardcoding.
#'
#' @param file_path A string representing the path to the RNA-seq data file.
#' @return A data frame containing the processed RNA-seq data.
#' @export
read_rna_seq_data <- function(file_path) {
  # Check if the file exists
  if (!file.exists(file_path)) {
    stop("File does not exist or path is empty: ", file_path)
  }

  # Read the lines to detect where the real header starts
  all_lines <- readLines(file_path)

  # Find the header line (look for "Geneid" at the start of a line)
  header_line <- grep("^Geneid\\t", all_lines)

  if (length(header_line) == 0) {
    stop("No valid header line found (looking for line starting with 'Geneid'). Check file format.")
  }

  # How many lines to skip to reach the header
  skip_n <- header_line - 1

  # Now read the file properly
  data <- readr::read_tsv(file_path, skip = skip_n, show_col_types = FALSE)

  # Print the structure of the data to check column names
  cat("Data structure:\n")
  print(str(data))

  # Expected columns based on the provided data (adjust for your current data)
  expected_columns <- c("Geneid", "Chr", "Start", "End", "Strand", "Length")

  # Check if any expected columns are missing
  missing_columns <- setdiff(expected_columns, colnames(data))
  if (length(missing_columns) > 0) {
    message("Missing expected columns: ", paste(missing_columns, collapse = ", "))
    for (col in missing_columns) {
      data[[col]] <- NA
    }
  }

  # Keep the metadata and expression columns together
  # Ensure we do not exclude metadata columns anymore
  expr_data <- data

  # Rename sample columns to lowercase
  colnames(expr_data) <- tolower(colnames(expr_data))

  # Return the processed data
  return(expr_data)
}
