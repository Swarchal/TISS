% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale_d.R
\name{scale_d}
\alias{scale_d}
\title{Z-score normalises D-statistic vectors}
\usage{
scale_d(d_table)
}
\arguments{
\item{d_table}{matrix or dataframe of D-statistics}
}
\value{
z_out matrix or dataframe of z-scored D-statistics
}
\description{
Given a table of D-statistic values calculated by \code{calculate_d}. This 
function normalises each column via a z-score. With column representing the 
D-statistic vector for a compound. Each column is normalised independently of
the other columns in d_table
}

