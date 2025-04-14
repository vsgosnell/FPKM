test_that("calculate_fpkm computes expected FPKM values", {
  # Example input: read counts and lengths
  counts <- c(100, 200, 300)
  lengths <- c(1000, 2000, 1500)
  total_counts <- sum(counts)

  # Expected FPKM = (10^9 * counts) / (lengths * total_counts)
  expected_fpkm <- (1e9 * counts) / (lengths * total_counts)

  # Call the function from your package
  computed_fpkm <- calculate_fpkm(counts, lengths)

  expect_equal(computed_fpkm, expected_fpkm)
})
