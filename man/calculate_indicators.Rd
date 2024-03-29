% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/03-calculate-indicators.R
\name{calculate_indicators}
\alias{calculate_indicators}
\title{Calculate Indicators}
\usage{
calculate_indicators(
  rhomis_data,
  units_and_conversions,
  prices,
  calorie_conversions,
  gender_categories
)
}
\arguments{
\item{rhomis_data}{a rhomis dataset}

\item{units_and_conversions}{A list of units and conversions}

\item{prices}{A list of price conversions (mean price per kg)}

\item{calorie_conversions}{A list of calorie conversions for different products}

\item{gender_categories}{The different categories of people (e.g. male_youth, female_youth, male_adult, female_adult)}
}
\description{
Calculate rhomis indicators. In
order to calculate indicators, it
is best to make sure that you have
first extracted units. That you
then verify prices and calories
for products in the
}
\details{
Rpackage file: 03-calculate-indicators.R
}
