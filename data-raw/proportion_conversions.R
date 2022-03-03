## code to prepare `proportion_conversions` dataset goes here
library(tibble)
library(tidyr)
library(magrittr)
proportion_conversions<- tibble::as_tibble(list("all"=0.9,
                         "most"=0.7,
                         "half"=0.5,
                         "underhalf"=0.2,
                         "little"=0.1,
                         "none"=0)) %>% tidyr::pivot_longer(cols=tidyr::everything(), names_to = "survey_value", values_to = "conversion")


usethis::use_data(proportion_conversions, overwrite = TRUE)
