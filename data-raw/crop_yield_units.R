## code to prepare `crop_yield_units` dataset goes here
# Generated with the command (usethis::use_data_raw())

library(tibble)
library(tidyr)
library(magrittr)

#full_data <- read_csv("inst/extdata/whole_data/RHoMIS_Full_Data.csv")

# sort(table(full_data$crop_yield_units_1))
# sort(table(full_data$crop_sold_price_quantityunits_1))

crop_yield_units <- tibble::as_tibble(list("kg"=1,
                                   "sacks_100kg_quintal"=100,
                                   "sacks_50kg"=50,
                                   "sacks_100kg"=100,
                                   "tonnes"=1000,
                                   "cart_225kg"=225,
                                   "tons"=1000,
                                   "gunia_100kg"=100,
                                   "oxcart_250_to_500_kg"=375,
                                   "quintal"=100,
                                   "sacks_90kg"=90,
                                   "bunches_18kg"=18,
                                   "sacks_45kg"=237,
                                   "bushels_25kg"=25,
                                   "qunital_100kg"=100,
                                   "cart_450kg"=450,
                                   "sacks_46kg"=46,
                                   "quintal_100kg"=100,
                                   "debe_18kg"=18,
                                   "debe"=18,
                                   "sacks_25kg"=25,
                                   "bag_100kg"=100,
                                   "bunch_15kg"=15)) %>% tidyr::pivot_longer(tidyr::everything(),names_to = "unit", values_to = "conversion")

usethis::use_data(crop_yield_units, overwrite = TRUE)
