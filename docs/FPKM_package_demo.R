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

## ----calculate FPKM, warning = FALSE, message = FALSE-------------------------
# Identify sample columns
sample_columns <- grep("\\.bam$", colnames(vehicle_drug_data), value = TRUE)

# Calculate FPKM
fpkm_values <- calculate_fpkm(vehicle_drug_data, count_cols = sample_columns)

# Preview
head(fpkm_values)

## ----filter low expression----------------------------------------------------
# Filter out low-expression genes (e.g., average FPKM < 1)
filtered_data <- filter_low_expression(fpkm_values, threshold = 1)

## ----heatmap-top-variables, message=FALSE, warning=FALSE, fig.width=8, fig.height=10----
plot_expression_heatmap(filtered_data, top_n = 100)

## ----FPKM Distribution Boxplot, warning = FALSE, message = FALSE, fig.width=8, fig.height=6----
plot_fpkm_boxplot(filtered_data)

## ----FPKM Distribution Bar Graph, fig.width=8, fig.height=6-------------------
filtered_data$FPKM <- rowMeans(filtered_data)
plot_fpkm_distribution(filtered_data$FPKM)

## ----PCA simple, message=FALSE, warning=FALSE, fig.width=8, fig.height=6------
plot_pca_fpkm(filtered_data)

## ----PCA breakdown colored, message=FALSE, warning=FALSE, fig.width=8, fig.height=6----
# Log-transform filtered data
log_fpkm <- log2(filtered_data + 1)

# transpose the matrix
pca_input <- t(log_fpkm)

# Run PCA
pca_result <- prcomp(pca_input, scale. = TRUE)

# Calculate percentage of variance explained by PC1 and PC2
pve <- round(100 * summary(pca_result)$importance[2, 1:2], 1)

# Create PCA data frame for plotting
pca_df <- as.data.frame(pca_result$x)
pca_df$Sample <- rownames(pca_df)

# Assign conditions based on sample names
pca_df$Condition <- ifelse(grepl("vehicle", pca_df$Sample, ignore.case = TRUE), "Vehicle", ifelse(grepl("drug", pca_df$Sample, ignore.case = TRUE), "Drug", "Unknown"))

# load the ggplot library
library(ggplot2)

# Plot PCA with color by condition
ggplot(pca_df, aes(x = PC1, y = PC2, color = Condition, label = Sample)) +
  geom_point(size = 4) +
  geom_text(vjust = 1.5, size = 4) +
  labs(
    title = "PCA of Gene Expression",
    x = paste0("PC1 (", pve[1], "%)"),
    y = paste0("PC2 (", pve[2], "%)")
  ) +
  theme_minimal()

## ----Correlation Heatmap, message=FALSE, warning=FALSE, fig.width=8, fig.height=6----
plot_sample_correlation(filtered_data)

