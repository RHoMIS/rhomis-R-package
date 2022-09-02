library(usethis)
library(tibble)
library(magrittr)
library(tidyr)

# Countr ISO
country_to_iso2 = c(
    "BF"="burkina_faso",
    "BF"="burkina faso",
    "BI"="burundi",
    "BO"="bolivia",
    "CD"="drc",
    "CI"="cote d'ivoire",
    "CR"="costa_rica",
    "CR"="costa rica",
    "EC"="ecuador",
    "ET"="ethiopia",
    "GH"="ghana",
    "GT"="guatemala",
    "HN"="honduras",
    "IN"="india",
    "KE"="kenya",
    "KH"="cambodia",
    "KM"="comoros",
    "LA"="laopdr",
    "LA"="laos",
    "MA"="morocco",
    "ML"="mali",
    "MW"="malawi",
    "NE"="niger",
    "NG"="nigeria",
    "NI"="nicaragua",
    "PE"="peru",
    "SV"="el_salvador",
    "SV"="el salvador",
    "TZ"="tanzania",
    "UG"="uganda",
    "VN"="vietnam",
    "ZA"="south africa",
    "ZA"="south_africa",
    "ZM"="Zambia",
    "RW"="rwanda",
    "PS"="palestine",
    "SN"="senegal",
    "SL"="sierra leone",
    "SL"="sierra_leone",
    "MZ"="mozambique")

country_to_iso2 <- tibble::as_tibble(list(
    "survey_value"=country_to_iso2,
    "conversion"=names(country_to_iso2)
))

usethis::use_data(country_to_iso2, overwrite = TRUE)




# Crop Name Conversions
crop_name_to_std <- c(
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

usethis::use_data(crop_name_to_std, overwrite = T)


crop_calories <- tibble::as_tibble(list(
    "maize" = 3650,
    "millet" = 3780,
    "cassava" = 1600,
    "rubber" = NA,
    "coffee" = NA,
    "other_vegetables" = 720,
    "sugarcane" = 400,
    "irish_potato" = 580,
    "eggplant" = 250,
    "cashew" = 5530,
    "rice" = 3600,
    "soya_bean" = 1470,
    "yam" = 1180,
    "sweet_potato" = 860,
    "bambara_nut" = 5670,
    "tomato" = 210,
    "onions" = 420,
    "fodder" = NA,
    "ground_nut" = 5670,
    "sesame" = 5730,
    "cowpea" = 3360,
    "chilipepper" = NA,
    "sorghum" = 3390,
    "field_pea" = 3360,
    "okra" = 420,
    "fonio" = 3520,
    "haricot_bean" = 650,
    "fruits" = 800,
    "oilpalm" = 360,
    "banana" = 890,
    "taro" = 1180,
    "amaranth" = 3710,
    "pineapple" = 800,
    "climbing_beans" = 1480,
    "bush_beans" = 1480,
    "cabbage" = 250,
    "pigeon_pea" = 1470,
    "cucumber" = 150,
    "custard_apple" = 800,
    "passion_fruit" = 800,
    "palm_heart" = 360,
    "oranges" = 800,
    "coconut" = 800,
    "beans" = 1480,
    "strawberry" = 800,
    "agave" = NA,
    "pumpkin" = 480,
    "broad_beans" = 1480,
    "barley" = 3520,
    "lettuce" = 150,
    "lentils" = 3780,
    "carrot" = 480,
    "oats" = 350,
    "cauliflower" = 250,
    "turnip" = 720,
    "fava_bean" = 1470,
    "fenugreek" = 1000,
    "teff" = 3670,
    "linseed" = 3000,
    "chick_peas" = 3600,
    "chat" = 500,
    "grass_pea" = 1470,
    "garlic" = 1490,
    "peas" = 810,
    "fingermillet" = 3780,
    "cacao" = 1290,
    "plantain" = 890,
    "moringa" = 370,
    "agushi" = NA,
    "fra_fra_potato" = 580,
    "radish" = 160,
    "pulses" = 3530,
    "pearl_millet" = 3780,
    "clustar_bean" = 1430,
    "green_gram" = 3470,
    "kidney_bean" = 1430,
    "buckwheat" = 3350,
    "mango" = 800,
    "kales" = 490,
    "arrow_root" = 650,
    "avocado" = 1600,
    "lemons" = 290,
    "butternut_squash" = 450,
    "watermelon" = 30,
    "spinach" = 230,
    "macadamia" = 7180,
    "chinese_cabbage" = 250,
    "jackfruit" = 940,
    "wet_rice" = 1300,
    "mung_bean" = 347,
    "yardlong_bean" = 1430,
    "guava" = 680,
    "tamarind" = 2390,
    "durian" = 1470,
    "dragon_fruit" = 600,
    "betel" = 3390,
    "rambutan" = 750,
    "cactus" = 160,
    "date_palm" = 2770,
    "almonds" = 5790,
    "apple" = 520,
    "citrus_fruits" = 290,
    "zucchini" = 170,
    "sorrel" = 220,
    "pumpkin_leaf" = 220,
    "melon" = 340,
    "kola_nut" = 930,
    "black_beans" = 430,
    "grapes" = 670,
    "simsim" = 5730,
    "mushrooms" = 220
)) %>% tidyr::pivot_longer(tidyr::everything(), names_to = "survey_value", values_to = "conversion")

usethis::use_data(crop_calories, overwrite = TRUE)

# Crop Price Units
crop_price_to_lcu_per_kg <- tibble::as_tibble(list("total_income_per_year"="total_income_per_year",
                                                   "price_per_kg"="1",
                                                   "price_per_bag_50kg"="0.02",
                                                   "price_per_bag_100kg"="0.01",
                                                   "price_per_100kg"="0.01",
                                                   "price_per_bag_45kg"="0.022",
                                                   "price_per_quintal"="0.01",
                                                   "price_per_bag_90kg"="0.011"))%>% tidyr::pivot_longer(tidyr::everything(),names_to = "survey_value", values_to = "conversion")


usethis::use_data(crop_price_to_lcu_per_kg, overwrite = TRUE)




crop_amount_to_kg <- tibble::as_tibble(list("kg"=1,
                                            "sacks_100kg_quintal"=100,
                                            "sacks_50kg"=50,
                                            "sacks_100kg"=100,
                                            "tonnes"=1000,
                                            "cart_225kg"=225,
                                            "tons"=1000,
                                            "gunia_100kg"=100,
                                            "oxcart_250_to_500_kg"=375,
                                            "quintal"=100,
                                            "sacks_90kg"=90,
                                            "bunches_18kg"=18,
                                            "sacks_45kg"=237,
                                            "bushels_25kg"=25,
                                            "qunital_100kg"=100,
                                            "cart_450kg"=450,
                                            "sacks_46kg"=46,
                                            "quintal_100kg"=100,
                                            "debe_18kg"=18,
                                            "debe"=18,
                                            "sacks_25kg"=25,
                                            "bag_100kg"=100,
                                            "bunch_15kg"=15)) %>% tidyr::pivot_longer(tidyr::everything(),names_to = "survey_value", values_to = "conversion")

usethis::use_data(crop_amount_to_kg, overwrite = TRUE)


eggs_amount_to_pieces_per_year <- tibble::as_tibble(list(survey_value=c("pieces/day",
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

usethis::use_data(eggs_amount_to_pieces_per_year, overwrite = T)

eggs_price_to_lcu_per_year <- tibble::as_tibble(list(survey_value=c("day",
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

usethis::use_data(eggs_price_to_lcu_per_year, overwrite = T)


fertiliser_amount_to_kg <- tibble::as_tibble(list("kg"=1,
                                                  "litre"=1,
                                                  "sacks_50kg"=50,
                                                  "sacks_100kg"=100,
                                                  "tonnes"=1000,
                                                  "cart_225kg"=225,
                                                  "tons"=1000,
                                                  "quintal"=100,
                                                  "sacks_90kg"=90,
                                                  "sacks_45kg"=237,
                                                  "cart_450kg"=450,
                                                  "sacks_25kg"=25,
                                                  "bag_100kg"=100)) %>% tidyr::pivot_longer(tidyr::everything(),names_to = "survey_value", values_to = "conversion")

usethis::use_data(fertiliser_amount_to_kg, overwrite = TRUE)


# 1 litre of honey is 1.43 kg.
# This conversion sheet converts everything into kg values
fertiliser_amount_to_kg <- tibble::as_tibble(list(survey_value=c("litres",
                                                                 "kg"),
                                                  conversion=c(1.43,
                                                               1
                                                  )))

usethis::use_data(fertiliser_amount_to_kg, overwrite = T)


# 1 litre of honey is 1.43 kg.
# This conversion sheet converts everything into kg values
honey_amount_to_l <- tibble::as_tibble(list(survey_value=c("litres",
                                                           "kg"),
                                            conversion=c(1.43,
                                                         1
                                            )))

usethis::use_data(honey_amount_to_l, overwrite = T)



honey_calories <- tibble::as_tibble(list(
    "bees"=3040)) %>% tidyr::pivot_longer(tidyr::everything(),names_to = "survey_value", values_to = "conversion")

usethis::use_data(honey_calories, overwrite = TRUE)




land_area_to_ha <- tibble::as_tibble(list(survey_value=c("hectare","acre","m2", "hectares","acres"),
                                          conversion=c(1,0.4,0.0001, 1,0.4)))

usethis::use_data(land_area_to_ha, overwrite = T)






livestock_name_to_std <- c(
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

usethis::use_data(livestock_name_to_std, overwrite = T)



livestock_count_to_tlu <- tibble::as_tibble(list(
    "cattle" = 0.7,
    "sheep" = 0.1,
    "goats" = 0.1,
    "camel" = 0.7,
    "otherpoultry" = 0.05,
    "guinea_pigs" = 0.01,
    "donkeys_horses" = 0.7,
    "pigs" = 0.3,
    "fish" = NA,
    "chicken" = 0.01,
    "duck" = 0.01,
    "buffalo" = 0.7,
    "bees" = NA,
    "small_mammal" = 0.01,
    "geese" = 0.02
)) %>% tidyr::pivot_longer(tidyr::everything(), names_to = "survey_value", values_to = "conversion")

usethis::use_data(livestock_count_to_tlu, overwrite = TRUE)




eggs_calories <- tibble::as_tibble(list(
    "cattle"=NA,
    "sheep"=NA,
    "goats"=NA,
    "camel"=NA,
    "otherpoultry"=1550,
    "guinea_pigs"=NA,
    "donkeys_horses"=NA,
    "pigs"=NA,
    "fish"=NA,
    "chicken"=1550,
    "duck"=1550,
    "buffalo"=NA)) %>% tidyr::pivot_longer(tidyr::everything(),names_to = "survey_value", values_to = "conversion")

usethis::use_data(eggs_calories, overwrite = TRUE)




livestock_weight_kg <- structure(list(survey_value = c("fish",
                                                       "pigs",
                                                       "chicken",
                                                       "cattle",
                                                       "duck",
                                                       "goats",
                                                       "donkeys_horses",
                                                       "rabbits",
                                                       "crocodile",
                                                       "otherpoultry",
                                                       "geese",
                                                       "buffalo",
                                                       "guinea_pigs",
                                                       "doves",
                                                       "camel",
                                                       "llama",
                                                       "alpaca",
                                                       "sheep"),
                                      conversion = c(1,
                                                     150,
                                                     1,
                                                     250,
                                                     1,
                                                     25,
                                                     250,
                                                     1,
                                                     55,
                                                     1,
                                                     2,
                                                     250,
                                                     1,
                                                     1,
                                                     250,
                                                     250,
                                                     250,
                                                     25)),
                                 row.names = c(NA, -18L),
                                 class = c("tbl_df", "tbl", "data.frame"))

usethis::use_data(livestock_weight_kg, overwrite = TRUE)



meat_calories <- tibble::as_tibble(list(
    "cattle"=2197,
    "sheep"=1075,
    "goats"=1075,
    "camel"=2197,
    "otherpoultry"=1290,
    "guinea_pigs"=1290,
    "donkeys_horses"=2197,
    "pigs"=2197,
    "fish"=1290,
    "chicken"=1290,
    "duck"=2197,
    "buffalo"=2197)) %>% tidyr::pivot_longer(tidyr::everything(),names_to = "survey_value", values_to = "conversion")

usethis::use_data(meat_calories, overwrite = TRUE)





milk_amount_to_l <- tibble::as_tibble(list("survey_value"=c("l/day",
                                                             "l/animal/day",
                                                             "0.3l/day",
                                                             "per animal per week",
                                                             "0.3l/animal/day"
),
"conversion"=c(365,
               "l/animal/day",
               0.3*365,
               "per animal per week",
               "0.3l/animal/day"
)))

usethis::use_data(milk_amount_to_l, overwrite = TRUE)






milk_calories <- tibble::as_tibble(list(
    "cattle"=597,
    "sheep"=597,
    "goats"=597,
    "camel"=597,
    "otherpoultry"=NA,
    "guinea_pigs"=NA,
    "donkeys_horses"=NA,
    "pigs"=NA,
    "fish"=NA,
    "chicken"=NA,
    "duck"=NA,
    "buffalo"=NA)) %>% tidyr::pivot_longer(tidyr::everything(),names_to = "survey_value", values_to = "conversion")

usethis::use_data(milk_calories, overwrite = TRUE)





milk_price_to_lcu_per_l <- tibble::as_tibble(list(survey_value=c("day",
                                                          "week",
                                                          "month",
                                                          "year",
                                                          "litre",
                                                          "0.3l"),
                                           conversion=c("day",
                                                        "week",
                                                        "month",
                                                        "year",
                                                        1,
                                                        0.3)))

usethis::use_data(milk_price_to_lcu_per_l, overwrite = T)






fp_amount_to_kg <- tibble::as_tibble(
    list(
        survey_value=c("bundle_20kg", "kg", "tine_20kg","calabash_5litres"),
        conversion=c(20,1, 20, 5)
    )
)
usethis::use_data(fp_amount_to_kg, overwrite = T)




fp_income_per_freq_to_lcu_per_year <- tibble::as_tibble(
    list(
        survey_value=c("day", "week", "month","year"),
        conversion=c(365,52, 12, 1)
    )
)
usethis::use_data(fp_income_per_freq_to_lcu_per_year, overwrite = T)



