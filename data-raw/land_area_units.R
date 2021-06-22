library(tibble)

land_area_units <- tibble::as_tibble(list(units=c("hectare","acre","m2"),
                                          conversions=c(1,0.4,0.0001)))

usethis::use_data(land_area_units, overwrite = T)
