
#' This function is run on load (see zzz.R) to set the default list of repeated columns when reading in a new rhomis dataset.
set_repeat_column_names <- function(){

    assign("repeat_columns",
           c("crop_repeat",
             "livestock_repeat",
             "offfarm_repeat",
             "offfarm_income_repeat",
             "hh_pop_repeat",
             "hh_rep"
             ),
           envir = pkg.env)

    return()
}

set_gender_categories <- function(){

    assign("gender_categories",
           c("male_youth",
             "female_youth",
             "male_adult",
             "female_adult"
             ),
           envir = pkg.env)

    return()
}


set_local_units_file_list <- function(){

    assign("local_units_file_list",
    list("country.csv" = "country_conversions",
         "crop_name.csv" = "crop_name_conversions",
         "livestock_name.csv" = "livestock_name_conversions",
         "crop_yield_units.csv" = "crop_yield_unit_conversions",
         "crop_sold_price_quantityunits.csv" = "crop_price_unit_conversions",
         "unitland.csv" = "land_unit_conversion",
         "milk_units.csv" = "milk_unit_conversion",
         "milk_sold_price_timeunits.csv" = "milk_price_unit_conversion",
         "bees_honey_production_units.csv" = "honey_unit_conversion",
         "eggs_units.csv" = "eggs_unit_conversion",
         "eggs_sold_price_timeunits.csv" = "eggs_price_unit_conversion",
         "fertiliser_units.csv" = "fertiliser_unit_conversion"),
    envir = pkg.env)


    assign("local_units_file_tibble_list",
    list("country.csv"          = "country",
         "crop_name.csv"        = "crop_name",
         "livestock_name.csv"   = "livestock_name",
         "crop_yield_units.csv" = "crop_yield_units",
         "crop_sold_price_quantityunits.csv" = "crop_price_units",
         "unitland.csv"         = "land_area_units",
         "milk_units.csv"       = "milk_amount_units",
         "milk_sold_price_timeunits.csv" = "milk_price_time_units",
         "bees_honey_production_units.csv" = "honey_amount_units",
         "eggs_units.csv" = "eggs_amount_units",
         "eggs_sold_price_timeunits.csv" = "eggs_price_time_units",
         "fertiliser_units.csv" = "fertiliser_units"),
    envir = pkg.env)



    return()

}


