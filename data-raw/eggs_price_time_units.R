library(tibble)

eggs_price_time_units <- tibble::as_tibble(list(survey_value=c("day",
                                                       "week",
                                                       "month",
                                                       "year",
                                                       "total",
                                                       "per_egg"),
                                                conversion=c(365,
                                                                    365/7,
                                                                    365/28,
                                                                    1,
                                                                    1,
                                                                    "per_egg")))

usethis::use_data(eggs_price_time_units, overwrite = T)
