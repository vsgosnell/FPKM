test_that("log_transform_fpkm handles zero values correctly", {
  data <- data.frame(A = c(0, 1, 10), B = c(5, 0, 20))
  log_result <- log_transform_fpkm(data)
  expect_true(all(is.finite(as.matrix(log_result))))
})
