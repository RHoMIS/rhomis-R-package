% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataReformatting.R
\name{proportions_calculation}
\alias{proportions_calculation}
\title{Proportions for individual proportion types}
\usage{
proportions_calculation(data, use, use_column, prop_column, loop_number = NULL)
}
\arguments{
\item{data}{A standard RHoMIS data set}

\item{use}{The use of crops being examined
This could include "eat", "sell", "feed_livestock"}

\item{use_column}{The column which includes the uses for this item}

\item{prop_column}{The column containing the proportions for this use}

\item{loop_number}{The number of the loop which is being processed}
}
\description{
A function for calculating the numeric proportions of crops which are sold,
consumed, or fed to livestock
}
\details{
Rpackage file: DataReformatting.R
}
