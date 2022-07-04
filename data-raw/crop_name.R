
library(usethis)


crop_name <- c(
    "maize",
    "millet",
    "cassava",
    "rubber",
    "coffee",
    "other_vegetables",
    "ground_nut",
    "sugarcane",
    "irish_potato",
    "eggplant",
    "cashew",
    "rice",
    "soya_bean",
    "yam",
    "sweet_potato",
    "bambara_nut",
    "tomato",
    "onions",
    "fodder",
    "oilpalm"
)

usethis::use_data(crop_name, overwrite = T)