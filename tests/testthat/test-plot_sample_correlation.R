test_that("plot_sample_correlation handles zero variance", {
  df <- data.frame(
    Gene1 = c(1, 1, 1, 1),  # zero variance
    Gene2 = c(1, 2, 3, 4),
    Gene3 = c(4, 3, 2, 1)
  )
  expect_silent(plot_sample_correlation(df))

  df_zero_var <- data.frame(
    Gene1 = c(1, 1, 1, 1),
    Gene2 = c(2, 2, 2, 2)
  )
  expect_warning(res <- plot_sample_correlation(df_zero_var), "Not enough variable columns")
  expect_null(res)
})
