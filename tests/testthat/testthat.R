
test_that("calculate_fpkm works correctly", {
  counts <- c(100, 300, 600)
  lengths <- c(1, 2, 3)
  fpkm <- calculate_fpkm(counts, lengths)
  expect_equal(length(fpkm), 3)
  expect_true(all(fpkm >= 0))
})
