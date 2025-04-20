test_that("plot_gene_expression works with proper row names", {
  df <- data.frame(
    Sample1 = c(10, 20, 30),
    Sample2 = c(15, 25, 35)
  )
  rownames(df) <- c("Gene1", "Gene2", "Gene3")

  expect_silent(plot_gene_expression(df, "Gene1"))
})
