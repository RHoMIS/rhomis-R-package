% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-odk-central.R
\name{extract_form_metadata}
\alias{extract_form_metadata}
\title{Extract Form Metadata}
\usage{
extract_form_metadata(
  central_url,
  central_email,
  central_password,
  projectID,
  formID,
  form_version = 1,
  isDraft
)
}
\arguments{
\item{central_url}{The url of the ODK central server}

\item{central_email}{The email of your ODK central account}

\item{central_password}{The password to your ODK central account}

\item{projectID}{The ID of the project you are looking at.
To get a list of project, see the
"get_projects" function}

\item{formID}{The XML form ID from a specific project}

\item{form_version}{The version of the form you are examining. For now
we presume you are looking for the first version of the form}

\item{isDraft}{Whether or not the form was a draft}
}
\description{
Extract the metadata for a RHoMIS project
}
\details{
Rpackage file: utils-odk-central.R
}
