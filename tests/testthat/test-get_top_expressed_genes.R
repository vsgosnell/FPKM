test_that("get_top_expressed_genes returns correct number", {
  df <- data.frame(
    Sample1 = c(1, 2, 3, 4, 5),
    Sample2 = c(2, 3, 4, 5, 6)
  )
  rownames(df) <- paste0("Gene", 1:5)

  top_genes <- get_top_expressed_genes(df)
  expect_length(top_genes, 5)
})
