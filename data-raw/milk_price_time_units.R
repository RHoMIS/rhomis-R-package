library(tibble)

milk_price_time_units <- tibble::as_tibble(list(unit=c("day",
                                                       "week",
                                                       "month",
                                                       "year",
                                                       "litre",
                                                       "0.3l"),
                                                conversion_factor=c("day",
                                                                    "week",
                                                                    "month",
                                                                    "year",
                                                                    1,
                                                                    0.3)))

usethis::use_data(milk_price_time_units, overwrite = T)
