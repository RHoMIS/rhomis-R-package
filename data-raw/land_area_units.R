library(tibble)

land_area_units <- tibble::as_tibble(list(survey_value=c("hectare","acre","m2"),
                                          conversion=c(1,0.4,0.0001)))

usethis::use_data(land_area_units, overwrite = T)
