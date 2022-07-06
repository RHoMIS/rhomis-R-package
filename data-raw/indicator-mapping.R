#' A mapping of all of the indicators calculated in RHoMIS.
#'
#' This comes in the form of a list, each item in the list
#' looks like this
#'
#' {
#' indicator_name:  "crop_yield", # "the name of the indicator being calculated",
#' individual_columns_required: [], #
#' loop_columns_required: [], #
#' conversion_tables_required: [], #
#' api_data_required: [] #
#' indicators_required: [], #
#' file_located: "" #,
#' }
#'

library(jsonlite)


indicator_list <- list()


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# METADATA 
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------


indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "iso_country_code",
    output_format="column",
    individual_columns_required=list("country"), 
    loop_columns_required=list(),
    conversion_tables_required=list("country_name_conversions"),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="run_preliminary_calculations",
    search_term="indicator_search_id_rhomis_dataset"
)

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "year",
    output_format="column",
    individual_columns_required=list("year", "start_time_user"), 
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="run_preliminary_calculations",
    search_term="indicator_search_year")


indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "currency_conversion_lcu_to_ppp",
    output_format="column",
    individual_columns_required=list(), 
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list("world_bank_api"),
    indicators_required=list("year","iso_country_code"),
    function_calculated="run_preliminary_calculations",
    search_term="indicator_search_currency_conversion_lcu_to_ppp")


indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "survey_length_minutes",
    output_format="column",
    individual_columns_required=list("start_time_user","end_time_user"), 
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="run_preliminary_calculations",
    search_term="indicator_search_survey_length_minutes")


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# CROP INFORMATION
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------



indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "crop_harvest_kg_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list(
                                   "crop_name",
                                   "crop_yield",
                                   "crop_yield_units",
                                   "crop_yield_units_other"
                              ),
    conversion_tables_required=list("crop_name_conversions", "crop_yield_unit_conversions"),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="crop_harvest_calculations",
    search_term="indicator_search_crop_harvest_kg_per_year")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "crop_consumed_kg_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("crop_use",
                              "crop_consumed_prop"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("crop_harvest_kg_per_year"),
    function_calculated="crop_sold_and_consumed_calculation",
    search_term="indicator_search_crop_consumed_kg_per_year")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "crop_sold_kg_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("crop_use",
                              "crop_sold_prop"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("crop_harvest_kg_per_year"),
    function_calculated="crop_sold_and_consumed_calculation",
    search_term="indicator_search_crop_sold_kg_per_year")


indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "crop_income_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("crop_sold_income", "crop_sold_price_quantityunits"),
    conversion_tables_required=list("crop_price_unit_conversions"),
    api_data_required = list(),
    indicators_required=list("crop_sold_kg_per_year"),
    function_calculated="crop_income_calculations",
    search_term="indicator_search_crop_income_per_year")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "crop_price",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("crop_income_per_year","crop_sold_kg_per_year"),
    function_calculated="crop_income_calculations",
    search_term="indicator_search_crop_price")

#-------------------------
# Gender Crop Consumption
#-------------------------
indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "male_youth_crop_consumed_kg_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("crop_consume_control"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("crop_consumed_kg_per_year"),
    function_calculated="crop_gender_calculations",
    search_term="indicator_search_male_youth_crop_consumed_kg_per_year")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "male_adult_crop_consumed_kg_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("crop_consume_control"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("crop_consumed_kg_per_year"),
    function_calculated="crop_gender_calculations",
    search_term="indicator_search_male_adult_crop_consumed_kg_per_year")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "female_youth_crop_consumed_kg_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("crop_consume_control"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("crop_consumed_kg_per_year"),
    function_calculated="crop_gender_calculations",
    search_term="indicator_search_female_youth_crop_consumed_kg_per_year")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "female_adult_crop_consumed_kg_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("crop_consume_control"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("crop_consumed_kg_per_year"),
    function_calculated="crop_gender_calculations",
    search_term="indicator_search_female_adult_crop_consumed_kg_per_year")

#-------------------------
# Gender Crop Selling
#-------------------------

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "male_youth_crop_sold_kg_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("crop_who_control_revenue"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("crop_sold_kg_per_year"),
    function_calculated="crop_gender_calculations",
    search_term="indicator_search_male_youth_crop_sold_kg_per_year")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "male_adult_crop_sold_kg_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("crop_who_control_revenue"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("crop_sold_kg_per_year"),
    function_calculated="crop_gender_calculations",
    search_term="indicator_search_male_adult_crop_sold_kg_per_year")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "female_youth_crop_sold_kg_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("crop_who_control_revenue"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("crop_sold_kg_per_year"),
    function_calculated="crop_gender_calculations",
    search_term="indicator_search_female_youth_crop_sold_kg_per_year")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "female_adult_crop_sold_kg_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("crop_who_control_revenue"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("crop_sold_kg_per_year"),
    function_calculated="crop_gender_calculations",
    search_term="indicator_search_female_adult_crop_sold_kg_per_year")

#-------------------------
# Gender Crop Income
#-------------------------

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "male_youth_crop_income_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("crop_who_control_revenue"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("crop_income_per_year"),
    function_calculated="crop_gender_calculations",
    search_term="indicator_search_male_youth_crop_income_per_year")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "male_adult_crop_income_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("crop_who_control_revenue"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("crop_income_per_year"),
    function_calculated="crop_gender_calculations",
    search_term="indicator_search_male_adult_crop_income_per_year")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "female_youth_crop_income_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("crop_who_control_revenue"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("crop_income_per_year"),
    function_calculated="crop_gender_calculations",
    search_term="indicator_search_female_youth_crop_income_per_year")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "female_adult_crop_income_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("crop_who_control_revenue"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("crop_income_per_year"),
    function_calculated="crop_gender_calculations",
    search_term="indicator_search_female_adult_crop_income_per_year")


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# LIVESTOCK CALCULATIONS
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "livestock_price_per_animal",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("livestock_sold","livestock_sale_income","livestock_price_per_animal"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="price_per_livestock",
    search_term="indicator_search_livestock_price_per_animal")


indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "meat_kg_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("livestock_name","killed_for_meat"),
    conversion_tables_required=list("livestock_weights"),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="meat_amount_calculation",
    search_term="indicator_search_meat_kg_per_year")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "meat_sold_kg_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("meat_sell_amount","meat_use"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("meat_kg_per_year"),
    function_calculated="meat_sold_and_consumed_calculation",
    search_term="indicator_search_meat_sold_kg_per_year")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "meat_consumed_kg_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("meat_consumed_amount","meat_use"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("meat_kg_per_year"),
    function_calculated="meat_sold_and_consumed_calculation",
    search_term="indicator_search_meat_consumed_kg_per_year")


indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "meat_price_per_kg",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("meat_sold_income"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("meat_sold_kg_per_year"),
    function_calculated="meat_prices",
    search_term="indicator_search_meat_price_per_kg")


indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "milk_amount_good_season_litres_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("milk_amount_good_season", "milk_units", "milk_number_animals_milked"),
    conversion_tables_required=list("milk_unit_conversion"),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="milk_amount_calculations",
    search_term="indicator_search_milk_amount_good_season_litres_per_year")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "milk_amount_bad_season_litres_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("milk_amount_bad_season", "milk_units", "milk_number_animals_milked"),
    conversion_tables_required=list("milk_unit_conversion"),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="milk_amount_calculations",
    search_term="indicator_search_milk_amount_bad_season_litres_per_year")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "milk_collected_litres_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("milk_amount_good_season_litres_per_year","milk_amount_bad_season_litres_per_year"),
    function_calculated="milk_amount_calculations",
    search_term="indicator_search_milk_collected_litres_per_year")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "milk_sold_litres_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("milk_sell_amount"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("milk_collected_litres_per_year"),
    function_calculated="milk_sold_and_consumed_calculations",
    search_term="indicator_search_milk_sold_litres_per_year")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "milk_consumed_litres_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("milk_consumed_amount"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("milk_collected_litres_per_year"),
    function_calculated="milk_sold_and_consumed_calculations",
    search_term="indicator_search_milk_consumed_litres_per_year")


indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "milk_sold_income_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("milk_sold_income", "milk_sold_price_timeunits"),
    conversion_tables_required=list("milk_price_unit_conversion"),
    api_data_required = list(),
    indicators_required=list("milk_sold_litres_per_year"),
    function_calculated="milk_income_calculations",
    search_term="indicator_search_milk_sold_income_per_year")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "eggs_amount_good_season_kg_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("eggs_amount_good", "eggs_units"),
    conversion_tables_required=list("eggs_unit_conversion"),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="eggs_amount_calculations",
    search_term="indicator_search_eggs_amount_good_season_kg_per_year")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "eggs_amount_bad_season_kg_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("eggs_amount_good", "eggs_units"),
    conversion_tables_required=list("eggs_unit_conversion"),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="eggs_amount_calculations",
    search_term="indicator_search_eggs_amount_bad_season_kg_per_year")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "eggs_collected_kg_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("eggs_amount_good_season_kg_per_year", "eggs_amount_bad_season_kg_per_year"),
    function_calculated="eggs_amount_calculations",
    search_term="c")


indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "eggs_sold_kg_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("eggs_use", "eggs_sell_amount"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("eggs_collected_kg_per_year"),
    function_calculated="eggs_sold_and_consumed_calculations",
    search_term="indicator_search_eggs_sold_kg_per_year")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "eggs_consumed_kg_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("eggs_use", "eggs_consumed_amount"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("eggs_collected_kg_per_year"),
    function_calculated="eggs_sold_and_consumed_calculations",
    search_term="indicator_search_eggs_consumed_kg_per_year")


indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "eggs_income_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("eggs_sold_income", "eggs_sold_price_timeunits"),
    conversion_tables_required=list("eggs_price_unit_conversion"),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="egg_income_calculations",
    search_term="indicator_search_eggs_income_per_year")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "eggs_price_per_kg",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("eggs_income_per_year", "eggs_sold_kg_per_year"),
    function_calculated="egg_income_calculations",
    search_term="indicator_search_eggs_price_per_kg")


indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "bees_honey_kg_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("bees_honey_production", "bees_honey_production_units"),
    conversion_tables_required=list("honey_unit_conversion"),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="honey_amount_calculation",
    search_term="indicator_search_bees_honey_kg_per_year")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "bees_honey_sold_kg_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("bees_honey_use","bees_honey_sell_amount"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("bees_honey_kg_per_year"),
    function_calculated="honey_amount_sold_and_consumed_calculations",
    search_term="indicator_search_bees_honey_sold_kg_per_year")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "bees_honey_consumed_kg_per_year",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("bees_honey_consumed_amount","bees_honey_use"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("bees_honey_kg_per_year"),
    function_calculated="honey_amount_sold_and_consumed_calculations",
    search_term="indicator_search_bees_honey_consumed_kg_per_year")


indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "bees_honey_price_per_kg",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list("bees_honey_sold_income"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("bees_honey_sold_kg_per_year"),
    function_calculated="honey_income_calculations",
    search_term="indicator_search_bees_honey_price_per_kg")



plot_dependency_network(
     indicator_name="bees_honey_price_per_kg", 
     indicator_list=indicator_list,
)

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(), 
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="",
    search_term="indicator_search_")

    
usethis::use_data(, overwrite = T)



#------------------------
# Plotting Example
#------------------------

jsonlite::toJSON(indicator_list, pretty=T)


find_nested_dependencies_list(
     indicator_name="currency_conversion_lcu_to_ppp", 
     indicator_list=indicator_list, 
     dependency_required="individual")


find_nested_dependencies_network(
     indicator_name="currency_conversion_lcu_to_ppp", 
     indicator_list=indicator_list
)

plot_dependency_network(
     indicator_name="currency_conversion_lcu_to_ppp", 
     indicator_list=indicator_list
)





jsonlite::fromJSON(json_mapping,simplifyVector = F)
