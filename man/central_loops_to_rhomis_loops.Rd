% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReformatLoops.R
\name{central_loops_to_rhomis_loops}
\alias{central_loops_to_rhomis_loops}
\title{Central Loops to RHoMIS Core}
\usage{
central_loops_to_rhomis_loops(central_core, loop_data)
}
\arguments{
\item{central_core}{Core data downloaded from ODK central}

\item{loop_data}{The sheet of loops which needs to be appended to the dataset}
}
\description{
ODK central data comes in a different format to the conventional RHoMIS
datasets. This function allows us to convert the ODK central
loops into the format needed for the indicator calculations to properly
function
}
\details{
Rpackage file: ReformatLoops.R
}
