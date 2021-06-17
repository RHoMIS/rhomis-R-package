library(tibble)

# 1 litre of honey is 1.43 kg.
# This conversion sheet converts everything into kg values
honey_amount_units <- tibble::as_tibble(list(units=c("litres",
                                                    "kg"),
                                             conversion_factors=c(1.43,
                                                                 1
                                                                 )))

usethis::use_data(honey_amount_units, overwrite = T)
