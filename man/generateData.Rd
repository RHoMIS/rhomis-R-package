% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RunAll.R
\name{generateData}
\alias{generateData}
\title{Generate Data}
\usage{
generateData(
  central_url,
  central_email,
  central_password,
  project_name,
  form_name,
  number_of_responses,
  isDraft = T
)
}
\arguments{
\item{central_url}{The URL of the central server holding the data}

\item{central_email}{The email of the administrative user}

\item{central_password}{The password of the administrative user}

\item{project_name}{The name of the project to generate data for}

\item{form_name}{The name of the form to generate data for}

\item{number_of_responses}{The number of responses to generate}

\item{isDraft}{Whether or not this is a draft state project}
}
\description{
Generate fake data and submit it to a test project
}
\details{
Rpackage file: RunAll.R
}
