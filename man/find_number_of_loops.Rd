% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReformatLoops.R
\name{find_number_of_loops}
\alias{find_number_of_loops}
\title{Find number of loops}
\usage{
find_number_of_loops(data, name_column)
}
\arguments{
\item{data}{The data containing the loops}

\item{name_column}{The "nam" column for the loops you are looking at, for example "crop_name"}
}
\value{
A single number, the number of loops for that variable
}
\description{
The RHoMIS data is arranged in a looping structure.
This function helps identify how many loops there are
for example for a variable like "crop_name".
 
Rpackage file: ReformatLoops.R
}
\examples{
name_column <- "crop_name"
data <- tibble::as_tibble(list(
  crop_name_1 = c("banana", "cassava", NA, "millet"),
  crop_name_2 = c("cassava", NA, "melon", "maize"),
  random_crop_name_2 = c("blue", "green", "red", NA),
  crop_name = c("orange", "purple", NA, "black")
))
find_number_of_loops(data, name_column)
}
