## code to prepare `crop_price_units` dataset goes here
library(tibble)
library(tidyr)
library(magrittr)
crop_price_units <- tibble::as_tibble(list("total_income_per_year"="total_income_per_year",
                                   "price_per_kg"="1",
                                   "price_per_bag_50kg"="0.02",
                                   "price_per_bag_100kg"="0.01",
                                   "price_per_100kg"="0.01",
                                   "price_per_bag_45kg"="0.022",
                                   "price_per_quintal"="0.01",
                                   "price_per_bag_90kg"="0.011"))%>% tidyr::pivot_longer(tidyr::everything(),names_to = "unit", values_to = "conversion")


usethis::use_data(crop_price_units, overwrite = TRUE)
