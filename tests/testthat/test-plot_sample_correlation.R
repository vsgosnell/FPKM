test_that("plot_sample_correlation handles numeric input", {
  df <- data.frame(
    sample1 = rnorm(10),  # numeric
    sample2 = rnorm(10),  # numeric
    sample3 = rnorm(10)   # numeric
  )

  # Double-check: all columns are numeric
  expect_true(all(sapply(df, is.numeric)))

  # The plot should not error
  expect_silent(plot_sample_correlation(df))
})
