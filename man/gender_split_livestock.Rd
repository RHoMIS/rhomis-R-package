% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LivestockCalculations.R
\name{gender_split_livestock}
\alias{gender_split_livestock}
\title{Gender Split of Livestock Information}
\usage{
gender_split_livestock(data, gender_categories = pkg.env$gender_categories)
}
\arguments{
\item{data}{RHoMIS data including information on number
of livestock sold and who sells this livestock}

\item{gender_categories}{The categories you are interested in examining}
}
\description{
Whole livestock, and products produced from livestock
are divided among male and female farmers. This function
determines how these values are split
}
\details{
Rpackage file: LivestockCalculations.R
}
