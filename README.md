# FPKM

![R-CMD-check]([https://github.com/vsgosnell/FPKM])

This package is released under the CC0 1.0 Universal (CC0 1.0) Public Domain Dedication.

## Overview

**FPKM** is an R package designed to streamline the analysis of RNA-seq data using the **Fragments Per Kilobase of transcript per Million mapped reads (FPKM)** normalization method. It provides functions for calculating, transforming, visualizing, and exploring gene expression data, making it a user-friendly solution for both exploratory and publication-ready analyses.

This package is especially useful for processing raw read count data, normalizing it, and generating intuitive plots like expression heatmaps, PCA plots, and sample correlation matrices.

## Features

- **Calculate FPKM** values from raw counts and gene lengths  
- **Filter** out lowly expressed genes   
- 🔍 **Identify** top expressed genes  
- **Log-transform** expression matrices  
- **Visualize** gene expression via boxplots, heatmaps, and PCA  
- **Plot expression** for top expressed genes based on a specified threshold
- **Merge** multiple FPKM tables across samples  
- **Summarize** raw RNA-seq data 
- **Explore correlations** across samples and expression profiles

## Installation

You can install the development version of **FPKM** from GitHub using:

```r
# install.packages("devtools")
devtools::install_github("vsgosnell/FPKM")
```

```r
library(FPKM)

# Example input
counts <- matrix(c(100, 200, 300, 400), nrow = 2,
                 dimnames = list(c("GeneA", "GeneB"), c("Sample1", "Sample2")))
gene_lengths <- c(GeneA = 1000, GeneB = 2000)

# Calculate FPKM
fpkm <- calculate_fpkm(counts, gene_lengths)

# Log-transform FPKM
log_fpkm <- log_transform_fpkm(fpkm)

# Plot gene expression
plot_gene_expression(fpkm, gene = "GeneA")

# Sample correlation heatmap
plot_sample_correlation(fpkm)
```

# Vignettes & Documentation
Detailed documentation and examples are provided for each function. A vignette (vignette("FPKM-demo")) is available with real-world examples of the entire workflow, from raw counts to visualization.

To access the help pages for any function:
```r
??FPKM
```
