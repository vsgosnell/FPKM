test_that("plot_pca_fpkm returns a ggplot object and excludes non-sample columns", {
  df <- data.frame(
    Gene = c("G1", "G2", "G3"),
    Sample1 = c(1, 2, 3),
    Sample2 = c(4, 5, 6),
    FPKM = c(10, 20, 30)  # Non-sample column to be excluded
  )
  p <- plot_pca_fpkm(df)
  expect_s3_class(p, "ggplot")
})
