% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NTFP.R
\name{ntfp_sold_and_consumed_calculation}
\alias{ntfp_sold_and_consumed_calculation}
\title{NTFP sold and consumed Calculation}
\usage{
ntfp_sold_and_consumed_calculation(
  data,
  fp_harvest_kg,
  fp_prop_sold_numeric,
  fp_amount_sold_kg,
  fp_prop_consumed_numeric,
  fp_amount_consumed_kg,
  fp_props_process_numeric,
  fp_amount_process_kg,
  fp_props_process_sold_numeric,
  fp_amount_process_sold_kg,
  fp_prop_process_consumed_numeric,
  fp_amount_process_consumed_kg
)
}
\arguments{
\item{data}{The dataset}

\item{fp_harvest_kg}{The column containing ntfp harvests}

\item{fp_prop_sold_numeric}{Numeric proportion columns for fp sold}

\item{fp_amount_sold_kg}{The column to be created for fp sold}

\item{fp_prop_consumed_numeric}{The numeric proportion of NTFP consumed}

\item{fp_amount_consumed_kg}{Amount consumed columns to be created}

\item{fp_props_process_numeric}{Proportions processed columns}

\item{fp_amount_process_kg}{Amount processed columns to be created}

\item{fp_props_process_sold_numeric}{The proportions processed sold numeric columns}

\item{fp_amount_process_sold_kg}{The amount processed and and sold in kg (to be created by function)}

\item{fp_prop_process_consumed_numeric}{The proportion processed consumed numeric}

\item{fp_amount_process_consumed_kg}{The amount processed and sold in kilograms (column to be created)}
}
\description{
Calculate the amounts of NTFPs sold and
consumed in KG
}
