test_that("summarize_expression returns summary stats", {
  df <- data.frame(
    Sample1 = rnorm(100),
    Sample2 = rnorm(100)
  )
  rownames(df) <- paste0("Gene", 1:100)

  result <- summarize_expression(df)
  expect_true(all(c("Gene", "Mean", "Variance") %in% colnames(result)))
})
