% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ProgressOutOfPoverty.R
\name{ppi_score}
\alias{ppi_score}
\title{PPI score}
\usage{
ppi_score(df, country_code_column)
}
\arguments{
\item{df}{The data set containing the PPI columns}

\item{country_code_column}{The country codes for the data-sets concerned}
}
\description{
This function calculates the ppi score based off of a typical RHoMIS
dataset. It relies on conversion sheets and information which can be found
in the "data" folder of the package.
}
\details{
Rpackage file: ProgressOutOfPoverty.R
}
\examples{

df <- tibble::as_tibble(list(
    "PPI_1" = c(5, 1, 3, 4,1,3),
    "PPI_2" = c(3, 12, 7, 18,4,5),
    "PPI_3" = c(17, 1, 2, 3,5,3),
    "PPI_4" = c(NA, 2, 1, 6,8,7),
    "PPI_5" = c(1, NA, 3, 1,1,2),
    "PPI_6" = c(3, 7, 4, 1,4,5),
    "PPI_7" = c(8, 3, NA, 2,7,6),
    "PPI_8" = c(3, 8, 1, 3,0,2),
    "PPI_9" = c(9, 2, 8, 5,NA,1),
    "PPI_10" = c(12, 4, 7, 1,1,1),
    "random_other_column" = c(NA, NA, NA, NA,NA,NA)
))

country_code_column <- c("VN", "VN", "KE", "KE",NA,"AZ")
ppi_score(df, country_code_column)
ppi_score(df, country_code_column)
     country_code_column <- toupper(country_code_column)


}
