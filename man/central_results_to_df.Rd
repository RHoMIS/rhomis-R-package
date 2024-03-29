% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-odk-central.R
\name{central_results_to_df}
\alias{central_results_to_df}
\title{Convert the nested list of central results into a tibble}
\usage{
central_results_to_df(central_results)
}
\arguments{
\item{central_results}{The nested list returned from a central query}
}
\value{
A table of central results
}
\description{
ODK central returns context in the form of a nested list. This converts
this nested list into a more accessible tibble format
}
\details{
Rpackage file: utils-odk-central.R
}
\examples{
central_results <- list(
  list("id" = 1, "name" = "name1", "email" = "email1"),
  list("id" = 2, "name" = "name2", "email" = "email2"),
  list("id" = 3, "name" = "name3", "email" = "email3")
)
central_results_to_df(list(
  list("id" = 1, "name" = "name1", "email" = "email1"),
  list("id" = 2, "name" = "name2", "email" = "email2"),
  list("id" = 3, "name" = "name3", "email" = "email3")
))
}
