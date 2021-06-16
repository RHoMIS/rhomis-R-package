library(tibble)

eggs_price_time_units <- tibble::as_tibble(list(unit=c("day",
                                                       "week",
                                                       "month",
                                                       "year",
                                                       "total",
                                                       "per_egg"),
                                                conversion_factor=c(365,
                                                                    365/7,
                                                                    365/28,
                                                                    1,
                                                                    1,
                                                                    "per_egg")))

usethis::use_data(eggs_price_time_units, overwrite = T)
