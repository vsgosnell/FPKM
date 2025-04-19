test_that("merge_fpkm_tables merges by Gene", {
  df1 <- data.frame(Gene = c("A", "B"), Sample1 = c(1, 2))
  df2 <- data.frame(Gene = c("A", "B"), Sample2 = c(3, 4))
  merged <- merge_fpkm_tables(list(df1, df2))
  expect_equal(ncol(merged), 3)
})
