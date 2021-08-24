library(tibble)

milk_amount_units <- tibble::as_tibble(list("unit"=c("l/day",
                                                     "l/animal/day",
                                                     "0.3l/day",
                                                     "per animal per week",
                                                     "0.3l/animal/day"
                                                     ),
                                            "conversion_factor"=c(365,
                                                                  "l/animal/day",
                                                                  0.3*365,
                                                                  "per animal per week",
                                                                  "0.3l/animal/day"
                                                                  )))

usethis::use_data(milk_amount_units, overwrite = TRUE)
