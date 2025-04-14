#' Process FPKM Data Frame
#'
#' @param data Data frame with `Counts` and `Length_kb` columns
#' @return Data frame with added `FPKM` column, sorted by FPKM
#' @export
process_fpkm_data <- function(data) {
  data %>%
    mutate(FPKM = (Counts / Length_kb) / (sum(Counts) / 1e6)) %>%
    arrange(desc(FPKM))
}
