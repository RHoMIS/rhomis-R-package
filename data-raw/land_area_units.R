library(tibble)

land_area_units <- tibble::as_tibble(list(survey_value=c("hectare","acre","m2", "hectares","acres"),
                                          conversion=c(1,0.4,0.0001, 1,0.4)))

usethis::use_data(land_area_units, overwrite = T)
