% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01-extract-units.R
\name{extract_units_and_conversions_server}
\alias{extract_units_and_conversions_server}
\title{Extract Units and Conversions from Server}
\usage{
extract_units_and_conversions_server(
  central_url,
  central_email,
  central_password,
  project_name,
  form_name,
  database,
  isDraft,
  central_test_case = FALSE
)
}
\arguments{
\item{central_url}{The url of the ODK-central}

\item{central_email}{The email of the ODK-central account being used}

\item{central_password}{The password of the ODK-central account being used}

\item{project_name}{The name of the ODK-central project being processed}

\item{form_name}{The name of the ODK-central form being processed}

\item{database}{The name of the database you would like to save results to}

\item{isDraft}{Whether or not the ODK form you are working with is a draft
or a final version. Only relevant if you are processing a project from ODK central}

\item{central_test_case}{This flag is used for running a test-sample dataset from ODK the inst/sample_central_project/ folder}
}
\description{
Rpackage file: 01-extract-units.R
}
