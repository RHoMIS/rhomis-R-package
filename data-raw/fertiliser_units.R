library(usethis)


fertiliser_units <- tibble::as_tibble(list("kg"=1,
                                           "litre"=1,
                                           "sacks_50kg"=50,
                                           "sacks_100kg"=100,
                                           "tonnes"=1000,
                                           "cart_225kg"=225,
                                           "tons"=1000,
                                           "quintal"=100,
                                           "sacks_90kg"=90,
                                           "sacks_45kg"=237,
                                           "cart_450kg"=450,
                                           "sacks_25kg"=25,
                                           "bag_100kg"=100)) %>% tidyr::pivot_longer(tidyr::everything(),names_to = "survey_value", values_to = "conversion")

usethis::use_data(fertiliser_units, overwrite = TRUE)
