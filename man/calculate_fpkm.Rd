% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_fpkm.R
\name{calculate_fpkm}
\alias{calculate_fpkm}
\title{Calculate FPKM}
\usage{
calculate_fpkm(data, count_cols = NULL)
}
\arguments{
\item{data}{A data frame containing counts and a "Length" column (case-insensitive)}

\item{count_cols}{A character vector of column names representing sample count data}
}
\value{
A data frame of FPKM values
}
\description{
Computes Fragments Per Kilobase of transcript per Million mapped reads.
}
