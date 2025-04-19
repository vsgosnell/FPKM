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
