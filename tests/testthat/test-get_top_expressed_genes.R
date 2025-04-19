test_that("get_top_expressed_genes returns correct number", {
  df <- data.frame(S1 = 1:100, S2 = 101:200)
  top_genes <- get_top_expressed_genes(df, n = 5)
  expect_equal(length(top_genes), 5)
})
