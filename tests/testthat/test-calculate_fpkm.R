test_that("calculate_fpkm computes expected FPKM values", {
  df <- data.frame(
    geneid = c("GeneA", "GeneB"),
    length = c(1000, 2000),
    sample1 = c(100, 200),
    sample2 = c(300, 400)
  )

  fpkm <- calculate_fpkm(df, count_cols = c("sample1", "sample2"))
  expect_equal(dim(fpkm), c(2, 2))
  expect_true(all(!is.na(fpkm)))
})
