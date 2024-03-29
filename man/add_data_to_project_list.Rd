% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-mongodb.R
\name{add_data_to_project_list}
\alias{add_data_to_project_list}
\title{Add Data to Project List}
\usage{
add_data_to_project_list(
  data,
  collection,
  database = "rhomis",
  url = "mongodb://localhost",
  projectID,
  formID,
  overwrite = F
)
}
\arguments{
\item{data}{Data to add}

\item{collection}{Collection to add it to}

\item{database}{Database to add it to}

\item{url}{URL of the database}

\item{projectID}{ID of the project you are adding}

\item{formID}{The id of the form being you are adding}

\item{overwrite}{Whether or not to overwrite the project}
}
\description{
A method for submitting another project
to a list of projects.
}
\details{
Rpackage file: utils-mongodb.R
}
