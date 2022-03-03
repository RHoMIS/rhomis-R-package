library(tibble)

eggs_amount_units <- tibble::as_tibble(list(survey_value=c("pieces/day",
                                                   "pieces/week",
                                                   "pieces/month",
                                                   "pieces/animal/day",
                                                   "total"
                                                   ),
                                            conversion=c(365,
                                                                365/7,
                                                                365/28,
                                                                "pieces/animal/day",
                                                                1)))

usethis::use_data(eggs_amount_units, overwrite = T)
