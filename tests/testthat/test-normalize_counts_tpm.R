test_that("normalize_counts_tpm returns correct shape", {
  counts <- matrix(c(100, 300, 400, 200), nrow = 2)
  lengths <- c(1000, 2000)
  result <- normalize_counts_tpm(counts, lengths)
  expect_equal(dim(result), dim(counts))
})
