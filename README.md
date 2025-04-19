# FPKM

![R-CMD-check]([https://github.com/vsgosnell/FPKM])

## Overview

**FPKM** is an R package designed to streamline the analysis of RNA-seq data using the **Fragments Per Kilobase of transcript per Million mapped reads (FPKM)** normalization method. It provides functions for calculating, transforming, visualizing, and exploring gene expression data, making it a user-friendly solution for both exploratory and publication-ready analyses.

This package is especially useful for processing raw read count data, normalizing it, and generating intuitive plots like expression heatmaps, PCA plots, and sample correlation matrices.

## Features

- 📊 **Calculate FPKM** values from raw counts and gene lengths  
- 🧹 **Filter** out lowly expressed genes  
- 🔄 **Normalize** data using TPM  
- 🔍 **Identify** top expressed genes  
- 🔢 **Log-transform** expression matrices  
- 📈 **Visualize** gene expression via boxplots, heatmaps, density plots, and PCA  
- 🧬 **Plot expression** for specific genes of interest  
- 🔗 **Merge** multiple FPKM tables across samples  
- 🧪 **Read and summarize** raw RNA-seq data  
- 🧬 **Explore correlations** across samples and expression profiles

## Installation

You can install the development version of **FPKM** from GitHub using:

```r
# install.packages("devtools")
devtools::install_github("vsgosnell/FPKM")
