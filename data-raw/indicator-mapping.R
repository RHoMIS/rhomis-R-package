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
    description="Two letter iso country code (lower case), see [here](https://www.iban.com/country-codes)",
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
    description="The year the survey was conducted",
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
    description="Currency conversion factors to convert incomes from lcu to ppp. To convert, mutliply the original income by this conversion factor",
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
    description="How long it took to complete the survey (in minutes)",
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
    description="The amount of crop harvested per year. Calculated by converting `crop_yield_units`",
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
    description="",
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
    description="",
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
    description="",
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
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("crop_income_per_year","crop_sold_kg_per_year"),
    function_calculated="crop_income_calculations",
    search_term="indicator_search_crop_price")

#-------------------------
# Gender Crop Indicators
#-------------------------

indicator_list <- add_gendered_indicator(
    indicator_list,
    indicator_name = "crop_consumed_kg_per_year",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("crop_consume_control"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("crop_consumed_kg_per_year"),
    function_calculated="crop_gender_calculations"
)


indicator_list <- add_gendered_indicator(
    indicator_list,
    indicator_name = "crop_sold_kg_per_year",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("crop_who_control_revenue"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("crop_sold_kg_per_year"),
    function_calculated="crop_gender_calculations"
)

indicator_list <- add_gendered_indicator(
    indicator_list,
    indicator_name = "crop_income_per_year",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("crop_who_control_revenue"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("crop_income_per_year"),
    function_calculated="crop_gender_calculations"
)




#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# LIVESTOCK CALCULATIONS
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------


#----------
#  WHOLE LIVESTOCK
#----------
indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "livestock_price_per_animal",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("livestock_sold","livestock_sale_income","livestock_price_per_animal"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="price_per_livestock",
    search_term="indicator_search_livestock_price_per_animal")


#----------
#  MEAT
#----------
indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "meat_kg_per_year",
    description="",
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
    description="",
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
    description="",
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
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("meat_sold_income"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("meat_sold_kg_per_year"),
    function_calculated="meat_prices",
    search_term="indicator_search_meat_price_per_kg")


#----------
#  MILK
#----------

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "milk_amount_good_season_litres_per_year",
    description="",
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
    description="",
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
    description="",
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
    description="",
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
    description="",
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
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("milk_sold_income", "milk_sold_price_timeunits"),
    conversion_tables_required=list("milk_price_unit_conversion"),
    api_data_required = list(),
    indicators_required=list("milk_sold_litres_per_year"),
    function_calculated="milk_income_calculations",
    search_term="indicator_search_milk_sold_income_per_year")


#----------
#  EGGS
#----------

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "eggs_amount_good_season_kg_per_year",
    description="",
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
    description="",
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
    description="",
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
    description="",
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
    description="",
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
    description="",
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
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("eggs_income_per_year", "eggs_sold_kg_per_year"),
    function_calculated="egg_income_calculations",
    search_term="indicator_search_eggs_price_per_kg")


#----------
#  HONEY
#----------

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "bees_honey_kg_per_year",
    description="",
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
    description="",
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
    description="",
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
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("bees_honey_sold_income"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("bees_honey_sold_kg_per_year"),
    function_calculated="honey_income_calculations",
    search_term="indicator_search_bees_honey_price_per_kg")

#----------
#  LIVESTOCK GENDER
#----------

#----------
#  Whole Livestock
#----------
indicator_list <- add_gendered_indicator(
    indicator_list,
    indicator_name = "livestock_sale_income",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("livestock_sale_income","livestock_who_sells"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="gender_split_livestock"
)

#----------
#  MEAT
#----------

indicator_list <- add_gendered_indicator(
    indicator_list,
    indicator_name = "meat_sold_income",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("meat_sold_income","livestock_meat_who_sells"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="gender_split_livestock"
)

indicator_list <- add_gendered_indicator(
    indicator_list,
    indicator_name = "meat_sold_kg_per_year",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("livestock_meat_who_sells"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("meat_sold_kg_per_year"),
    function_calculated="gender_split_livestock"
)

indicator_list <- add_gendered_indicator(
    indicator_list,
    indicator_name = "meat_consumed_kg_per_year",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("livestock_meat_who_control_eating"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("meat_consumed_kg_per_year"),
    function_calculated="gender_split_livestock"
)

#----------
#  MILK
#----------


indicator_list <- add_gendered_indicator(
    indicator_list,
    indicator_name = "milk_sold_litres_per_year",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("milk_who_sells"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("milk_sold_litres_per_year"),
    function_calculated="gender_split_livestock"
)

indicator_list <- add_gendered_indicator(
    indicator_list,
    indicator_name = "milk_sold_income_per_year",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("milk_who_sells"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("milk_sold_income_per_year"),
    function_calculated="gender_split_livestock"
)

indicator_list <- add_gendered_indicator(
    indicator_list,
    indicator_name = "milk_consumed_litres_per_year",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("milk_who_control_eating"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("milk_consumed_litres_per_year"),
    function_calculated="gender_split_livestock"
)



#----------
#  EGGS
#----------

indicator_list <- add_gendered_indicator(
    indicator_list,
    indicator_name = "eggs_sold_kg_per_year",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("eggs_who_sells"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("eggs_sold_kg_per_year"),
    function_calculated="gender_split_livestock"
)

indicator_list <- add_gendered_indicator(
    indicator_list,
    indicator_name = "eggs_income_per_year",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("eggs_who_sells"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("eggs_sold_kg_per_year"),
    function_calculated="gender_split_livestock"
)

indicator_list <- add_gendered_indicator(
    indicator_list,
    indicator_name = "eggs_consumed_kg_per_year",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("eggs_who_control_eating"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("eggs_consumed_kg_per_year"),
    function_calculated="gender_split_livestock"
)

#----------
#  HONEY
#----------

indicator_list <- add_gendered_indicator(
    indicator_list,
    indicator_name = "bees_honey_sold_kg_per_year",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("bees_who_sells"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("bees_honey_sold_kg_per_year"),
    function_calculated="gender_split_livestock"
)

indicator_list <- add_gendered_indicator(
    indicator_list,
    indicator_name = "bees_honey_sold_income",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("bees_who_sells", "bees_honey_sold_income"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="gender_split_livestock"
)

indicator_list <- add_gendered_indicator(
    indicator_list,
    indicator_name = "bees_honey_consumed_kg_per_year",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("bees_who_control_eating"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("bees_honey_consumed_kg_per_year"),
    function_calculated="gender_split_livestock"
)



#----------
#  LIVESTOCK TLU
#----------

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "livestock_tlu",
    description="",
    output_format="column", #column, loop, #table
    individual_columns_required=list(
    "livestock_other1",
     "livestock_other2",
    "livestock_other3",
    "livestock_heads_cattle",
    "livestock_heads_chicken",
    "livestock_heads_pigs",
    "livestock_heads_sheep",
    "livestock_heads_goats",
    "livestock_heads_other1_lstk",
    "livestock_heads_other2_lstk",
    "livestock_heads_other3_lstk"
    ),
    loop_columns_required=list(),
    conversion_tables_required=list("livestock_tlu_conversions"),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="livestock_tlu_calculations",
    search_term="indicator_search_livestock_tlu"
)

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Demographics
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "hh_size_members",
    description="",
    output_format="column", #column, loop, #table
    individual_columns_required=list(
     "children_under_4",
     "children_4to10",
     "males11to24",
     "females11to24",
     "males25to50",
     "female_25_to_50",
     "male_50_plus",
     "female_50_plus"),
    loop_columns_required=list("person_age", "person_gender","hh_pop_rep_num"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="calculate_household_size_members",
    search_term="indicator_search_hh_size_members")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "hh_size_mae",
    description="",
    output_format="column", #column, loop, #table
    individual_columns_required=list(
     "children_under_4",
     "children_4to10",
     "males11to24",
     "females11to24",
     "males25to50",
     "female_25_to_50",
     "male_50_plus",
     "female_50_plus"),
    loop_columns_required=list("person_age", "person_gender","hh_pop_rep_num"),
    conversion_tables_required=list("MAE_coeff"),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="calculate_household_size_members",
    search_term="indicator_search_hh_size_mae")

# Household type and education levels are directly copied from the data


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Land Size
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "land_cultivated_ha",
    description="",
    output_format="column", #column, loop, #table
    individual_columns_required=list("landcultivated","unitland"),
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="",
    search_term="indicator_search_land_cultivated_ha")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "land_owned_ha",
    description="",
    output_format="column", #column, loop, #table
    individual_columns_required=list("landowned","unitland"),
    loop_columns_required=list(),
    conversion_tables_required=list("land_unit_conversion"),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="land_size_calculation",
    search_term="indicator_search_land_owned_ha")




#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Food Security
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# food_worst_month and food_best_month come directly from the survey


indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "hfias_status",
    description="",
    output_format="column", #column, loop, #table
    individual_columns_required=list(
          "hfias_1",
          "hfias_2",
          "hfias_3",
          "hfias_4",
          "hfias_5",
          "hfias_6",
          "hfias_7",
          "hfias_8",
          "hfias_9"
     ),
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="food_security_calculations",
    search_term="indicator_search_hfias_status")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "fies_score",
    description="",
    output_format="column", #column, loop, #table
 individual_columns_required=list(
          "fies_1",
          "fies_2",
          "fies_3",
          "fies_4",
          "fies_5",
          "fies_6",
          "fies_7",
          "fies_8"
     ),     loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="food_security_calculations",
    search_term="indicator_search_fies_score")



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Dietary Diversity
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# HDDS is complicated, it sometimes relies on 14
# food groups, other times at 10.
# For now I will only include the 10



indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "hdds_good_season",
    description="",
    output_format="column", #column, loop, #table
    individual_columns_required=list(
     "grainsrootstubers_good_season",
     "legumes_good_season",
     "nuts_seeds_good_season",
     "veg_leafy_good_season",
     "vita_veg_fruit_good_season",
     "vegetables_good_season",
     "fruits_good_season",
     "meat_good_season",
     "eggs_good_season",
     "milk_dairy_good_season"),
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="hdds_calc",
    search_term="indicator_search_hdds_good_season")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "hdds_good_season_farm",
    description="",
    output_format="column", #column, loop, #table
    individual_columns_required=list(
     "grainsrootstubers_source_good",
     "legumes_source_good",
     "nuts_seeds_source_good",
     "veg_leafy_source_good",
     "vita_veg_fruit_source_good",
     "vegetables_source_good",
     "fruits_source_good",
     "meat_source_good",
     "eggs_source_good",
     "milk_dairy_source_good"),
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="",
    search_term="indicator_search_hdds_good_season_farm")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "hdds_good_season_bought",
    description="",
    output_format="column", #column, loop, #table
    individual_columns_required=list(
     "grainsrootstubers_source_good",
     "legumes_source_good",
     "nuts_seeds_source_good",
     "veg_leafy_source_good",
     "vita_veg_fruit_source_good",
     "vegetables_source_good",
     "fruits_source_good",
     "meat_source_good",
     "eggs_source_good",
     "milk_dairy_source_good"
    ),
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="hdds_calc",
    search_term="indicator_search_hdds_good_season_bought")


indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "hdds_bad_season",
    description="",
    output_format="column", #column, loop, #table
    individual_columns_required=list(
     "grainsrootstubers_bad_season",
     "legumes_bad_season",
     "nuts_seeds_bad_season",
     "veg_leafy_bad_season",
     "vita_veg_fruit_bad_season",
     "vegetables_bad_season",
     "fruits_bad_season",
     "meat_bad_season",
     "eggs_bad_season",
     "milk_dairy_bad_season"
    ),
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="hdds_calc",
    search_term="indicator_search_hdds_bad_season")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "hdds_bad_season_farm",
    description="",
    output_format="column", #column, loop, #table
    individual_columns_required=list(
      "grainsrootstubers_source_bad",
     "legumes_source_bad",
     "nuts_seeds_source_bad",
     "veg_leafy_source_bad",
     "vita_veg_fruit_source_bad",
     "vegetables_source_bad",
     "fruits_source_bad",
     "meat_source_bad",
     "eggs_source_bad",
     "milk_dairy_source_bad"
    ),
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="hdds_calc",
    search_term="indicator_search_hdds_bad_season_farm")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "hdds_bad_season_bought",
    description="",
    output_format="column", #column, loop, #table
    individual_columns_required=list(
     "grainsrootstubers_source_bad",
     "legumes_source_bad",
     "nuts_seeds_source_bad",
     "veg_leafy_source_bad",
     "vita_veg_fruit_source_bad",
     "vegetables_source_bad",
     "fruits_source_bad",
     "meat_source_bad",
     "eggs_source_bad",
     "milk_dairy_source_bad"
    ),
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="hdds_calc",
    search_term="indicator_search_hdds_bad_season_bought")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "hdds_last_month",
    description="",
    output_format="column", #column, loop, #table
    individual_columns_required=list(
     "grainsrootstubers_last_month",
     "legumes_last_month",
     "nuts_seeds_last_month",
     "veg_leafy_last_month",
     "vita_veg_fruit_last_month",
     "vegetables_last_month",
     "fruits_last_month",
     "meat_last_month",
     "eggs_last_month",
     "milk_dairy_last_month"
    ),
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="hdds_calc",
    search_term="indicator_search_hdds_last_month")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "hdds_last_month_farm",
    description="",
    output_format="column", #column, loop, #table
    individual_columns_required=list(
     "grainsrootstubers_source_last_month",
     "legumes_source_last_month",
     "nuts_seeds_source_last_month",
     "veg_leafy_source_last_month",
     "vita_veg_fruit_source_last_month",
     "vegetables_source_last_month",
     "fruits_source_last_month",
     "meat_source_last_month",
     "eggs_source_last_month",
     "milk_dairy_source_last_month"
    ),
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="hdds_calc",
    search_term="indicator_search_hdds_last_month_farm")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "hdds_last_month_bought",
    description="",
    output_format="column", #column, loop, #table
    individual_columns_required=list(
     "grainsrootstubers_source_last_month",
     "legumes_source_last_month",
     "nuts_seeds_source_last_month",
     "veg_leafy_source_last_month",
     "vita_veg_fruit_source_last_month",
     "vegetables_source_last_month",
     "fruits_source_last_month",
     "meat_source_last_month",
     "eggs_source_last_month",
     "milk_dairy_source_last_month"
    ),
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="hdds_calc",
    search_term="indicator_search_hdds_last_month_bought")


indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "hdds_last_24hr",
    description="",
    output_format="column", #column, loop, #table
    individual_columns_required=list(
     "grainsrootstubers_24hr",
     "legumes_24hr",
     "nuts_seeds_24hr",
     "veg_leafy_24hr",
     "vita_veg_fruit_24hr",
     "vegetables_24hr",
     "fruit_24hr",
     "meat_24hr",
     "eggs_24hr",
     "milk_dairy_24hr"
    ),
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="hdds_calc",
    search_term="indicator_search_hdds_last_24hr")



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Total Incomes
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------


indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "crop_income_lcu_per_year",
    description="",
    output_format="column", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("crop_income_per_year"),
    function_calculated="total_crop_income",
    search_term="indicator_search_crop_income_lcu_per_year")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "livestock_income_lcu_per_year",
    description="",
    output_format="column", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("livestock_sale_income", "meat_sold_income", "bees_honey_sold_income"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("milk_sold_income_per_year","eggs_income_per_year"),
    function_calculated="total_livestock_income",
    search_term="indicator_search_livestock_income_lcu_per_year")



indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "off_farm_income_lcu_per_year",
    description="",
    output_format="column", #column, loop, #table
    individual_columns_required=list("offfarm_income_proportion","offfarm_incomes_any"),
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("crop_income_lcu_per_year","livestock_income_lcu_per_year"),
    function_calculated="total_and_off_farm_incomes",
    search_term="indicator_search_off_farm_income_lcu_per_year")


indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "total_income_lcu_per_year",
    description="",
    output_format="column", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("off_farm_income_lcu_per_year","crop_income_lcu_per_year","livestock_income_lcu_per_year"),
    function_calculated="total_and_off_farm_incomes",
    search_term="indicator_search_total_income_lcu_per_year")



#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# VALUE CALCULATIONS
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------


# Crops

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "value_crop_consumed_lcu",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("crop_name"),
    conversion_tables_required=list("mean_crop_price_lcu_per_kg"),
    api_data_required = list(),
    indicators_required=list("crop_consumed_kg_per_year"),
    function_calculated="value_calculations",
    search_term="indicator_search_value_crop_consumed_lcu")

indicator_list <- add_gendered_indicator(
    indicator_list,
    indicator_name = "value_crop_consumed_lcu",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("crop_consume_control"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("value_crop_consumed_lcu"),
    function_calculated="value_calculations"
)

# Meat

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "value_meat_consumed_lcu",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("livestock_name"),
    conversion_tables_required=list("mean_meat_price_per_kg"),
    api_data_required = list(),
    indicators_required=list("meat_consumed_kg_per_year"),
    function_calculated="value_calculations",
    search_term="indicator_search_value_meat_consumed_lcu")

indicator_list <- add_gendered_indicator(
    indicator_list,
    indicator_name = "value_meat_consumed_lcu",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("livestock_meat_who_control_eating"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("value_meat_consumed_lcu"),
    function_calculated="value_calculations"
)

# Egg

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "value_eggs_consumed_lcu",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("livestock_name"),
    conversion_tables_required=list("mean_eggs_price_per_kg"),
    api_data_required = list(),
    indicators_required=list("eggs_consumed_kg_per_year"),
    function_calculated="value_calculations",
    search_term="indicator_search_value_eggs_consumed_lcu")

indicator_list <- add_gendered_indicator(
    indicator_list,
    indicator_name = "value_eggs_consumed_lcu",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("eggs_who_control_eating"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("value_eggs_consumed_lcu"),
    function_calculated="value_calculations"
)

# Milk

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "value_milk_consumed_lcu",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("livestock_name"),
    conversion_tables_required=list("mean_milk_price_per_litre"),
    api_data_required = list(),
    indicators_required=list("milk_consumed_litres_per_year"),
    function_calculated="value_calculations",
    search_term="indicator_search_value_milk_consumed_lcu")

indicator_list <- add_gendered_indicator(
    indicator_list,
    indicator_name = "value_milk_consumed_lcu",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("milk_who_control_eating"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("value_milk_consumed_lcu"),
    function_calculated="value_calculations"
)

# Honey

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "value_bees_honey_consumed_lcu",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("livestock_name"),
    conversion_tables_required=list("mean_bees_honey_price_per_kg"),
    api_data_required = list(),
    indicators_required=list("bees_honey_consumed_kg_per_year"),
    function_calculated="value_calculations",
    search_term="indicator_search_value_bees_honey_consumed_lcu")

indicator_list <- add_gendered_indicator(
    indicator_list,
    indicator_name = "value_bees_honey_consumed_lcu",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("bees_who_control_eating"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("value_bees_honey_consumed_lcu"),
    function_calculated="value_calculations"
)




# Totals


indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "value_crop_consumed_lcu_per_hh_per_year",
    description="",
    output_format="column", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("value_crop_consumed_lcu"),
    function_calculated="value_calculations",
    search_term="indicator_search_value_crop_consumed_lcu_per_hh_per_year")


indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "value_livestock_products_consumed_lcu_per_hh_per_year",
    description="",
    output_format="column", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list(
     "value_meat_consumed_lcu",
     "value_eggs_consumed_lcu",
     "value_milk_consumed_lcu",
     "value_bees_honey_consumed_lcu"),
    function_calculated="value_calculations",
    search_term="indicator_search_value_livestock_products_consumed_lcu_per_hh_per_year")




indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "value_farm_products_consumed_lcu_per_hh_per_year",
    description="",
    output_format="column", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list(
     "value_crop_consumed_lcu_per_hh_per_year",
     "value_livestock_products_consumed_lcu_per_hh_per_year"
     ),
    function_calculated="value_calculations",
    search_term="indicator_search_value_farm_products_consumed_lcu_per_hh_per_year")


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# CALORIE CALCULATIONS
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Crops

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "crop_calories_consumed_kcal",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("crop_name"),
    conversion_tables_required=list("crop_calories"),
    api_data_required = list(),
    indicators_required=list("crop_consumed_kg_per_year"),
    function_calculated="calorie_calculations",
    search_term="indicator_search_crop_calories_consumed_kcal")

# Meat

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "meat_calories_consumed_kcal",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("livestock_name"),
    conversion_tables_required=list("meat_calories_kcal_per_kg"),
    api_data_required = list(),
    indicators_required=list("meat_consumed_kg_per_year"),
    function_calculated="calorie_calculations",
    search_term="indicator_search_meat_calories_consumed_kcal")

# Egg

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "eggs_calories_consumed_kcal",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("livestock_name"),
    conversion_tables_required=list("eggs_calories_kcal_per_kg"),
    api_data_required = list(),
    indicators_required=list("eggs_consumed_kg_per_year"),
    function_calculated="calorie_calculations",
    search_term="indicator_search_eggs_calories_consumed_kcal")

# Milk

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "milk_calories_consumed_kcal",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("livestock_name"),
    conversion_tables_required=list("milk_calories_kcal_per_litre"),
    api_data_required = list(),
    indicators_required=list("milk_consumed_litres_per_year"),
    function_calculated="calorie_calculations",
    search_term="indicator_search_milk_calories_consumed_kcal")



# Honey

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "bees_honey_calories_consumed_kcal",
    description="",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("livestock_name"),
    conversion_tables_required=list("bees_honey_calories_kcal_per_kg"),
    api_data_required = list(),
    indicators_required=list("bees_honey_consumed_kg_per_year"),
    function_calculated="calorie_calculations",
    search_term="indicator_search_bees_honey_calories_consumed_kcal")






# Totals


indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "crop_consumed_calories_kcal_per_hh_per_year",
    description="",
    output_format="column", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("crop_calories_consumed_kcal"),
    function_calculated="calorie_calculations",
    search_term="indicator_search_crop_consumed_calories_kcal_per_hh_per_year")


indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "livestock_consumed_calories_kcal_per_hh_per_year",
    description="",
    output_format="column", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list(
           "meat_calories_consumed_kcal",
          "eggs_calories_consumed_kcal",
          "milk_calories_consumed_kcal",
          "bees_honey_calories_consumed_kcal"),
    function_calculated="calorie_calculations",
    search_term="indicator_search_livestock_consumed_calories_kcal_per_hh_per_year")




indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "farm_products_consumed_calories_kcal_per_hh_per_year",
    description="",
    output_format="column", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list(),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list(
     "crop_consumed_calories_kcal_per_hh_per_year",
     "livestock_consumed_calories_kcal_per_hh_per_year"
     ),
    function_calculated="calorie_calculations",
    search_term="indicator_search_farm_products_consumed_calories_kcal_per_hh_per_year")


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Gender Calculations All
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------



  dependent_indicator_suffixes <- list(
    "value_crop_consumed_lcu",
    "value_meat_consumed_lcu",
    "value_milk_consumed_lcu",
    "value_eggs_consumed_lcu",
    "value_bees_honey_consumed_lcu",
     "crop_income_per_year",
    "livestock_sale_income",
    "meat_sold_income",
    "milk_sold_income_per_year",
    "eggs_income_per_year",
    "bees_honey_sold_income"
  )



  gender_prefixes <- list(
     "female_youth",
     "female_adult",
     "male_youth",
     "male_adult"
  )

  for (gender in gender_prefixes){

     indicator_dependents <- paste0(gender, "_", dependent_indicator_suffixes)
     indicator_dependents <- append(indicator_dependents,"off_farm_income_lcu_per_year")


     indicator_name <- paste0(gender,"_control")
     search_term <- paste0("indicator_search_",gender,"_control")

     indicator_list <- add_indicator(
     indicator_list,
     indicator_name = indicator_name,
     description="",
    output_format="column", #column, loop, #table
     individual_columns_required=list(),
     loop_columns_required=list("off_farm_source_prop"),
     conversion_tables_required=list(),
     api_data_required = list(),
     indicators_required=list(indicator_dependents),
     function_calculated="gender_control_summary",
     search_term=search_term)
  }




plot_dependency_network(
     indicator_name="male_adult_bees_honey_sold_income",
     indicator_list=indicator_list,
)


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# LIVESTOCK CALCULATIONS
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#
# indicator_list <- add_indicator(
#     indicator_list,
#     indicator_name = "",
#     description="",
#    output_format="loop", #column, loop, #table
#     individual_columns_required=list(),
#     loop_columns_required=list(),
#     conversion_tables_required=list(),
#     api_data_required = list(),
#     indicators_required=list(),
#     function_calculated="",
#     search_term="indicator_search_")


usethis::use_data(indicator_list, overwrite = T)



#------------------------
# Plotting Example
#------------------------

jsonlite::toJSON(indicator_list, pretty=T)


find_nested_dependencies_list(
     indicator_name="crop_sold_kg_per_year",
     indicator_list=indicator_list,
     dependency_required="loop")


find_nested_dependencies_network(
     indicator_name="currency_conversion_lcu_to_ppp",
     indicator_list=indicator_list
)

plot_dependency_network(
     indicator_name="currency_conversion_lcu_to_ppp"
)





