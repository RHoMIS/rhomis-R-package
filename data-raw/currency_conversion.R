# Currency conversion factors last updated 03/01/2023
library(readr)
library(dplyr)
currency_conversion <- readr::read_csv("./inst/ppp_conversion.csv",skip = 4)
currency_conversion["...67"] <- NULL
year_columns <- colnames(currency_conversion)[colnames(currency_conversion)>1959 & colnames(currency_conversion)<2100 ]

currency_conversion <- pivot_longer(currency_conversion, cols = year_columns)

colnames(currency_conversion)[colnames(currency_conversion)=="name"] <- "year"

usethis::use_data(currency_conversion, overwrite = T)
