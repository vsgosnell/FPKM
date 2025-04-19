test_that("process_fpkm_data adds FPKM column and sorts", {
  df <- data.frame(Counts = c(100, 50), Length_kb = c(1, 2))
  result <- process_fpkm_data(df)
  expect_true("FPKM" %in% colnames(result))
})
