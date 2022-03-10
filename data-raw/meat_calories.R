library(tibble)
library(tidyr)
library(magrittr)


meat_calories <- tibble::as_tibble(list(
    "cattle"=2197,
    "sheep"=1075,
    "goats"=1075,
    "camel"=2197,
    "otherpoultry"=1290,
    "guinea_pigs"=1290,
    "donkeys_horses"=2197,
    "pigs"=2197,
    "fish"=1290,
    "chicken"=1290,
    "duck"=2197,
    "buffalo"=2197)) %>% tidyr::pivot_longer(tidyr::everything(),names_to = "survey_value", values_to = "conversion")

usethis::use_data(meat_calories, overwrite = TRUE)
