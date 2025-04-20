test_that("plot_expression_heatmap handles input", {
  df <- data.frame(
    Sample1 = c(1, 2, 3),
    Sample2 = c(2, 3, 4),
    Sample3 = c(3, 4, 5)
  )
  rownames(df) <- c("GeneA", "GeneB", "GeneC")

  expect_silent(plot_expression_heatmap(df, top_n = 2))
})
