library(tibble)

# 1 litre of honey is 1.43 kg.
# This conversion sheet converts everything into kg values
honey_amount_units <- tibble::as_tibble(list(survey_value=c("litres",
                                                    "kg"),
                                             conversion=c(1.43,
                                                                 1
                                                                 )))

usethis::use_data(honey_amount_units, overwrite = T)
