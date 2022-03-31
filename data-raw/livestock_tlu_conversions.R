library(usethis)
library(tibble)
library(magrittr)
library(tidyr)

livestock_tlu_conversions <- tibble::as_tibble(list(
    "cattle" = 0.7,
    "sheep" = 0.1,
    "goats" = 0.1,
    "camel" = 0.7,
    "otherpoultry" = 0.05,
    "guinea_pigs" = 0.01,
    "donkeys_horses" = 0.7,
    "pigs" = 0.3,
    "fish" = NA,
    "chicken" = 0.01,
    "duck" = 0.01,
    "buffalo" = 0.7,
    "bees" = NA,
    "small_mammal" = 0.01,
    "geese" = 0.02
)) %>% tidyr::pivot_longer(tidyr::everything(), names_to = "survey_value", values_to = "conversion")

usethis::use_data(livestock_tlu_conversions, overwrite = TRUE)