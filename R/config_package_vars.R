
#' This function is run on load (see zzz.R)
#' to set the default list of repeated columns
#' when reading in a new rhomis dataset.
set_repeat_column_names <- function() {
    assign("repeat_columns",
           c(
               "crop_repeat",
               "livestock_repeat",
               "offfarm_repeat",
               "offfarm_income_repeat",
               "hh_pop_repeat",
               "hh_rep"
           ),
           envir = pkg.env
    )

    return()

}

set_gender_categories <- function() {
    assign("gender_categories",
           c(
               "female_youth",
               "female_adult",
               "male_youth",
               "male_adult"
           ),
           envir = pkg.env
    )

    return()
}


set_conversion_file_names <- function(){

    loop_values_to_extract <- c(
        # Core columns
        "crop_name",
        "livestock_name",
        "crop_yield_units",
        "crop_yield_units_other",
        "crop_sold_price_quantityunits",
        "crop_price_quantityunits_other",
        "milk_units",
        "milk_amount_units_other",
        "milk_sold_price_timeunits",
        "milk_amount_time_units_other",
        "bees_honey_production_units",
        "bees_honey_production_units_other",
        "eggs_units",
        "eggs_amount_units_other",
        "eggs_sold_price_timeunits",
        "eggs_sold_price_timeunits_other",

        # NTFP Columns
        "fruit_amount_units",
        "fruit_amount_units_other",
        "nut_amount_units",
        "nut_amount_units_other_kg",
        "leaves_amount_units",
        "bark_amount_units",
        "roots_amount_units",
        "gum_amount_units",
        "fruit_sold_frequency",
        "fruit_sold_amount_units_other",
        "fruit_process_sold_frequency",
        "fruit_process_sold_amount_units_other",
        "nut_sold_frequency",
        "nut_sold_amount_units_other",
        "leaves_sold_frequency",
        "bark_sold_frequency",
        "roots_sold_frequency",
        "gum_sold_frequency"
    )






    assign("loop_values_to_extract",
           loop_values_to_extract,
           envir = pkg.env
    )



    individual_columns_to_extract <- c(
        # Core Columns
        "country",
        "crops_other1",
        "crops_other2",
        "crops_other3",
        "livestock_other1",
        "livestock_other2",
        "livestock_other3",
        "unitland",
        "areaunits_other",
        "areaunits_other_own",
        "areaunits_other_rent",
        "unitland_owned",
        "unitland_rentin",
        "unitland_rentout",
        "fertiliser_units",
        "fertiliser_units_other"
    )




    assign("individual_columns_to_extract",
           individual_columns_to_extract,
           envir = pkg.env
    )



    categories_to_merge <- list(
        country = c("country"),
        crop_name = c("crop_name", "crops_other1", "crops_other2", "crops_other3"),
        livestock_name = c("livestock_name", "livestock_other1", "livestock_other2", "livestock_other3", "livestock_heads"),
        crop_yield_units = c("crop_yield_units_other"),
        crop_sold_price_quantityunits = c("crop_price_quantityunits_other"),
        unitland = c("unitland", "unitland_owned", "unitland_rentin", "unitland_rentout", "areaunits_other_own", "areaunits_other_rent", "areaunits_other"),
        milk_units = c("milk_amount_units_other"),
        milk_sold_price_timeunits = c("milk_amount_time_units_other"),
        bees_honey_production_units = c("bees_honey_production_units_other"),
        eggs_units = c("eggs_amount_units_other"),
        eggs_sold_price_timeunits = c("eggs_sold_price_timeunits_other"),
        fertiliser_units = c("fertiliser_units_other"),

        fp_amount_units = c("fruit_amount_units","nut_amount_units","leaves_amount_units","bark_amount_units","roots_amount_units", "gum_amount_units"),
        fp_income_units = c("fruit_sold_frequency","nut_sold_frequency","leaves_sold_frequency","bark_sold_frequency","roots_sold_frequency", "gum_sold_frequency")

    )

    assign("categories_to_merge",
           categories_to_merge,
           envir = pkg.env
    )

    assign("optional_units",
           c(
               "fp_amount_units",
               "fp_income_units"
           ),
           envir = pkg.env)

    assign("unit_file_names",
           list(
               "country" = "country_to_iso2",
               "crop_name" = "crop_name_to_std",
               "livestock_name" = "livestock_name_to_std",
               "crop_yield_units" = "crop_amount_to_kg",
               "crop_sold_price_quantityunits" = "crop_price_to_lcu_per_kg",
               "unitland" = "land_area_to_ha",
               "milk_units" = "milk_amount_to_l",
               "milk_sold_price_timeunits" = "milk_price_to_lcu_per_l",
               "bees_honey_production_units" = "honey_amount_to_l",
               "eggs_units" = "eggs_amount_to_pieces_per_year",
               "eggs_sold_price_timeunits" = "eggs_price_to_lcu_per_year",
               "fertiliser_units" = "fertiliser_amount_to_kg",

               "fp_amount_units" = "fp_amount_to_kg",
               "fp_income_units" = "fp_income_per_freq_to_lcu_per_year",
               "livestock_count_to_tlu"="livestock_name_to_std",
               "livestock_weight_kg"="livestock_name_to_std" #Isn't collected in data but needs to be converted

           ),
           envir = pkg.env
    )

}

set_secondary_units <- function(){
    assign("secondary_units",
           list(
               "livestock_count_to_tlu"="livestock_name_to_std",

               "livestock_weight_kg"="livestock_name_to_std" #Isn't collected in data but needs to be converted
           ),
           envir = pkg.env
    )
}


# set_local_units_file_list <- function() {
#   assign("local_units_file_list",
#     list(
#       "country" = "country_conversions",
#       "crop_name" = "crop_name_conversions",
#       "livestock_name" = "livestock_name_conversions",
#       "crop_yield_units" = "crop_yield_unit_conversions",
#       "crop_sold_price_quantityunits" = "crop_price_unit_conversions",
#       "unitland" = "land_unit_conversion",
#       "milk_units" = "milk_unit_conversion",
#       "milk_sold_price_timeunits" = "milk_price_unit_conversion",
#       "bees_honey_production_units" = "honey_unit_conversion",
#       "eggs_units" = "eggs_unit_conversion",
#       "eggs_sold_price_timeunits" = "eggs_price_unit_conversion",
#       "fertiliser_units" = "fertiliser_unit_conversion",
#       "livestock_tlu" = "livestock_tlu_conversions"
#     ),
#     envir = pkg.env
#   )
#
#   # Conversion from the file name
#   # to the name of the conversion
#   # within the R package.
#
#   # file = package_table
#   assign("local_units_file_tibble_list",
#     list(
#       "country" = "country",
#       "crop_name" = "crop_name",
#       "livestock_name" = "livestock_name",
#       "crop_yield_units" = "crop_yield_units",
#       "crop_sold_price_quantityunits" = "crop_price_units",
#       "unitland" = "land_area_units",
#       "milk_units" = "milk_amount_units",
#       "milk_sold_price_timeunits" = "milk_price_time_units",
#       "bees_honey_production_units" = "honey_amount_units",
#       "eggs_units" = "eggs_amount_units",
#       "eggs_sold_price_timeunits" = "eggs_price_time_units",
#       "fertiliser_units" = "fertiliser_units",
#       "livestock_tlu" = "livestock_tlu"
#     ),
#     envir = pkg.env
#   )
#
#   return()
# }


set_produce_list <- function() {
    assign("produce_group_list",
           c(
               "crop",
               "eggs",
               "milk",
               "honey",
               "meat"
           ),
           envir = pkg.env
    )

    return()
}

set_identification_columns <- function() {
    assign("identification_column_list",
           list(
               # The usual name of the "country" column
               "country" = "country",
               # The usual column with unique ids for local datasets
               "uuid_local" = "_uuid",
               # The usual column with unique ids for datasets obtained form ODK central
               "uuid_central" = "KEY"
           ),
           envir = pkg.env
    )
}


set_local_processing_paths <- function() {
    assign("local_processing_paths",
           list(
               "default_base_path" = "./",
               "original_units" = "original_units",
               "converted_units" = "converted_units"
           ),
           envir = pkg.env
    )
}


set_prices_list <- function() {

    fp_prices <- c()
    for (fp_product in rhomis::fp_products){

        fp_prices <- c(fp_prices,paste0(fp_product$base_name,"_price_lcu_per_kg"))
        fp_prices <- c(fp_prices,paste0(fp_product$base_name,"_process_price_lcu_per_kg"))


    }

    assign("price_conversion_list",
           c(
               "mean_crop_price_lcu_per_kg",
               "mean_livestock_price_per_animal",
               "mean_meat_price_per_kg",
               "mean_milk_price_per_litre",
               "mean_eggs_price_per_kg",
               "mean_bees_honey_price_per_kg",
               fp_prices
           ),
           envir = pkg.env
    )
}

set_calories_list <- function() {

    fp_calories <- c()
    for (fp_product in rhomis::fp_products){

        fp_calories <- c(fp_calories,paste0(fp_product$base_name,"_calories_kcal_per_kg"))
        fp_calories <- c(fp_calories,paste0(fp_product$base_name,"_process_calories_kcal_per_kg"))


    }

    assign("calorie_conversion_list",
           c(
               "crop_calories",
               "milk_calories",
               "eggs_calories",
               "honey_calories",
               "meat_calories",
               fp_calories
           ),
           envir = pkg.env
    )
}
