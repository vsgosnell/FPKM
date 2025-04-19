test_that("plot_expression_heatmap handles input", {
  df <- data.frame(S1 = 1:10, S2 = 11:20)
  expect_error(plot_expression_heatmap(df, top_n = 5), NA)
})
