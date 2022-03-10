## code to prepare `crop_calories` dataset goes here
# Generated with the command (usethis::use_data_raw())

library(tibble)
library(tidyr)
library(magrittr)

#full_data <- read_csv("inst/extdata/whole_data/RHoMIS_Full_Data.csv")

# sort(table(full_data$crop_yield_units_1))
# sort(table(full_data$crop_sold_price_quantityunits_1))

eggs_calories <- tibble::as_tibble(list(
    "cattle"=NA,
    "sheep"=NA,
    "goats"=NA,
    "camel"=NA,
    "otherpoultry"=1550,
    "guinea_pigs"=NA,
    "donkeys_horses"=NA,
    "pigs"=NA,
    "fish"=NA,
    "chicken"=1550,
    "duck"=1550,
    "buffalo"=NA)) %>% tidyr::pivot_longer(tidyr::everything(),names_to = "survey_value", values_to = "conversion")

usethis::use_data(eggs_calories, overwrite = TRUE)
