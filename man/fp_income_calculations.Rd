% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NTFP.R
\name{fp_income_calculations}
\alias{fp_income_calculations}
\title{NTFP Income Calculation}
\usage{
fp_income_calculations(
  data,
  unit_conv_tibble = NULL,
  fp_sold_kg_per_year_column,
  fp_sold_units_column,
  fp_sold_income_column,
  new_fp_sold_income,
  new_price_column
)
}
\arguments{
\item{data}{The main Data set}

\item{unit_conv_tibble}{Conversion table}

\item{fp_sold_kg_per_year_column}{Column for the forest product sold per year}

\item{fp_sold_units_column}{Column with the forest product sold units}

\item{fp_sold_income_column}{Column with the amount of FP sold income}

\item{new_fp_sold_income}{The name of the new FP income column to be produced}

\item{new_price_column}{The name of the new price column to be produced}
}
\description{
Calculate the Income from NTFPs
in LCU per year
}
