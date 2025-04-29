test_that("read_rna_seq_data reads a properly formatted file", {
  tf <- tempfile(fileext = ".txt")
  writeLines(c(
    "# comment line",
    "Geneid\tChr\tStart\tEnd\tStrand\tLength\tsample1.bam\tsample2.bam",
    "geneA\tchr1\t1\t1000\t+\t1000\t10\t20",
    "geneB\tchr2\t1\t2000\t-\t2000\t30\t40"
  ), con = tf)

  result <- read_rna_seq_data(tf)
  expect_true(is.data.frame(result))
  expect_true("sample1.bam" %in% colnames(result))
  unlink(tf)
})
