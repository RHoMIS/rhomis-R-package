library(usethis)


livestock_name <- c(
    "cattle",
    "sheep",
    "goats",
    "camel",
    "otherpoultry",
    "guinea_pigs",
    "donkeys_horses",
    "pigs",
    "fish",
    "chicken",
    "duck",
    "buffalo",
    "bees"
)

usethis::use_data(livestock_name, overwrite = T)