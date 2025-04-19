test_that("plot_fpkm_boxplot returns a ggplot object", {
  df <- data.frame(Gene = c("G1", "G2", "G3"),
                   Sample1 = c(1, 2, 3),
                   Sample2 = c(4, 5, 6),
                   Sample3 = c(7, 8, 9),
                   Sample4 = c(10, 11, 12))
  p <- plot_fpkm_boxplot(df)
  expect_s3_class(p, "ggplot")
})
