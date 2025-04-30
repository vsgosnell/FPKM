## ----installation, results = 'hide', message = FALSE--------------------------
devtools::install_github("vsgosnell/FPKM")

## -----------------------------------------------------------------------------
library(FPKM)
library(ggplot2)
library(readr)

## ----read RNAseq data, results = 'hide', message = FALSE----------------------
# Load data from featureCounts output
vehicle_drug_data_path <- "/Users/veronicagosnell/Desktop/R/FPKM/inst/extdata/vehicle_drug_feature_counts.txt"

# Read using readr::read_tsv and skip comment lines
vehicle_drug_data <- readr::read_tsv(vehicle_drug_data_path, comment = "#")

# Rename length column if needed
if ("length" %in% colnames(vehicle_drug_data)) {
  colnames(vehicle_drug_data)[colnames(vehicle_drug_data) == "length"] <- "Length"
}

stopifnot("Length" %in% colnames(vehicle_drug_data))
stopifnot(all(sapply(vehicle_drug_data[, grep("\\.bam$", colnames(vehicle_drug_data))], is.numeric)))
stopifnot(nrow(vehicle_drug_data) == length(vehicle_drug_data$Length))


## ----head RNAseq data---------------------------------------------------------
head(vehicle_drug_data)

