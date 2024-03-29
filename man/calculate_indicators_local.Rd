% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03-calculate-indicators.R
\name{calculate_indicators_local}
\alias{calculate_indicators_local}
\title{Calculate all of the Local Indicators}
\usage{
calculate_indicators_local(
  base_path = "./",
  file_path,
  id_type = c("string", "column"),
  proj_id,
  form_id,
  gender_categories = pkg.env$gender_categories,
  unique_id_col = "_uuid",
  hh_id_col = NULL,
  overwrite = F
)
}
\arguments{
\item{base_path}{Tha folder where the analysis is being conducted}

\item{file_path}{The direct file path to the indicator file}

\item{id_type}{The type of ID being provided ("string" or "column")}

\item{proj_id}{The project id string or column name}

\item{form_id}{The form id string or column name}

\item{gender_categories}{The gender categories to examine}

\item{unique_id_col}{The column containing unique household ids}

\item{hh_id_col}{The column containing household ids to use, could be the same as unique_id_col. Household IDs can be linked to older longitudinal surveys when merging datasets.}

\item{overwrite}{Whether or not to overwrite household ids}
}
\description{
Calculate all of the Local Indicators
}
