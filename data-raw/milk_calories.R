## code to prepare `crop_calories` dataset goes here
# Generated with the command (usethis::use_data_raw())

library(tibble)
library(tidyr)
library(magrittr)


milk_calories <- tibble::as_tibble(list(
    "cattle"=597,
    "sheep"=597,
    "goats"=597,
    "camel"=597,
    "otherpoultry"=NA,
    "guinea_pigs"=NA,
    "donkeys_horses"=NA,
    "pigs"=NA,
    "fish"=NA,
    "chicken"=NA,
    "duck"=NA,
    "buffalo"=NA)) %>% tidyr::pivot_longer(tidyr::everything(),names_to = "survey_value", values_to = "conversion")

usethis::use_data(milk_calories, overwrite = TRUE)
