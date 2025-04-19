test_that("plot_gene_expression returns a ggplot object", {
  df <- data.frame(S1 = 1:3, S2 = 4:6)
  rownames(df) <- c("Gene1", "Gene2", "Gene3")
  p <- plot_gene_expression(df, "Gene1")
  expect_s3_class(p, "ggplot")
})
