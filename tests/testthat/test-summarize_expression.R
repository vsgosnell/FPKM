test_that("summarize_expression returns summary stats", {
  df <- data.frame(S1 = 1:3, S2 = 4:6)
  result <- summarize_expression(df)
  expect_equal(colnames(result), c("mean", "median", "variance"))
})
