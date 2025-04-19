test_that("plot_sample_correlation returns a ggplot object", {
  df <- data.frame(Gene = paste0("G", 1:3),
                   Sample1 = c(1, 2, 3),
                   Sample2 = c(3, 2, 1))
  p <- plot_sample_correlation(df)
  expect_s3_class(p, "ggplot")
})
