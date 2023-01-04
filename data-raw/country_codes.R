library(readr)
library(dplyr)
country_codes <- readr::read_csv("./inst/country_codes.csv")



usethis::use_data(country_codes, overwrite = T)
