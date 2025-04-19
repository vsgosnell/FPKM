test_that("filter_low_expression filters correctly", {
  df <- data.frame(S1 = c(5, 0.5), S2 = c(5, 0.5))
  filtered <- filter_low_expression(df, threshold = 1)
  expect_equal(nrow(filtered), 1)
})
