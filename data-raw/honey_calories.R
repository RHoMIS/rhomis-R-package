## code to prepare `crop_calories` dataset goes here
# Generated with the command (usethis::use_data_raw())

library(tibble)
library(tidyr)
library(magrittr)

#full_data <- read_csv("inst/extdata/whole_data/RHoMIS_Full_Data.csv")

# sort(table(full_data$crop_yield_units_1))
# sort(table(full_data$crop_sold_price_quantityunits_1))

honey_calories <- tibble::as_tibble(list(
    "bees"=3040)) %>% tidyr::pivot_longer(tidyr::everything(),names_to = "survey_value", values_to = "conversion")

usethis::use_data(honey_calories, overwrite = TRUE)
