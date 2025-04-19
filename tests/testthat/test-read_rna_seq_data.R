test_that("read_rna_seq_data reads a file", {
  tf <- tempfile(fileext = ".tsv")
  write.table(data.frame(V1 = 1:3, V2 = 4:6), tf, sep = "\t", row.names = FALSE)
  result <- read_rna_seq_data(tf)
  expect_s3_class(result, "data.frame")
})
