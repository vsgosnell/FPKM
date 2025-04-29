#' Merge FPKM Tables
#'
#' @param fpkm_list A list of data frames of FPKM values to merge
#'
#' @return A merged data frame
#' @export
merge_fpkm_tables <- function(fpkm_list) {
  Reduce(function(x, y) merge(x, y, by = "Gene", all = TRUE), fpkm_list)
}
