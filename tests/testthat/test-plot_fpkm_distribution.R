test_that("plot_fpkm_distribution returns a ggplot object", {
  p <- plot_fpkm_distribution(c(1, 10, 100))
  expect_s3_class(p, "ggplot")
})
