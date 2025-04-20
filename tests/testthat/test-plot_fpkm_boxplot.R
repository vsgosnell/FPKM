test_that("plot_fpkm_boxplot returns a ggplot object", {
  df <- data.frame(
    Sample1 = c(1, 5, 3),
    Sample2 = c(2, 6, 4)
  )
  rownames(df) <- c("Gene1", "Gene2", "Gene3")

  p <- plot_fpkm_boxplot(df)
  expect_s3_class(p, "ggplot")
})
