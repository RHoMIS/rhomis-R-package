## code to prepare `crop_calories` dataset goes here
# Generated with the command (usethis::use_data_raw())

library(tibble)
library(tidyr)
library(magrittr)

#full_data <- read_csv("inst/extdata/whole_data/RHoMIS_Full_Data.csv")

# sort(table(full_data$crop_yield_units_1))
# sort(table(full_data$crop_sold_price_quantityunits_1))

crop_calories <- tibble::as_tibble(list("maize"=3650,
                                        "millet"=3780,
                                        "cassava"=1600,
                                        "rubber"=NA,
                                        "coffee"=NA,
                                        "other_vegetables"=720,
                                        "ground_nut"=5760,
                                        "sugarcane"=400,
                                        "irish_potato"=580,
                                        "eggplant"=250,
                                        "cashew"=5530,
                                        "rice"=3600,
                                        "soya_bean"=1470,
                                        "yam"=1180,
                                        "sweet_potato"=860,
                                        "bambara_nut"=5670,
                                        "tomato"=210,
                                        "onions"=420,
                                        "fodder"=NA,
                                        "oilpalm"=NA)) %>% tidyr::pivot_longer(tidyr::everything(),names_to = "survey_value", values_to = "conversion")

usethis::use_data(crop_calories, overwrite = TRUE)
