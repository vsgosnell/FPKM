test_that("read_rna_seq_data reads a file", {
  tf <- tempfile(fileext = ".tsv")
  write.table(data.frame(V1 = 1:10, V2 = 11:20, V3 = 21:30, V4 = 31:40, V5 = 41:50,
                         V6 = 61:70, V7 = 71:80, V8 = 81:90, V9 = 91:100, V10 = 101:110),
              tf, sep = "\t", row.names = FALSE, col.names = FALSE)
  result <- read_rna_seq_data(tf)
  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 10)
})
