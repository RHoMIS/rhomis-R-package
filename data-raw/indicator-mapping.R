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



#' Tags:
#' production
#' consumption
#'

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
    description="Two letter iso country code (lower case), see <a href='https://www.iban.com/country-codes' target='_blank'>here</a>",
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
    description="The amount of crop harvested per year. Calculated by converting <code>crop_yield_units</code> to a numeric conversion factor. Then multiplying <code>crop_yield_units</code> by <code>crop_yield</code>",
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
    description="The amount of crop consumed per year in kilograms. We take the proportion of crop consumed (<code>crop_consumed_prop</code>) and multiply this proportion by <code>crop_harvest_per_year</code>.",
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
    description="The amount of crop sold per year in kilograms. We take the proportion of crop sold (<code>crop_sold_prop</code>) and multiply this proportion by <code>crop_harvest_per_year</code>.",
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
    description="The income from selling a specific crop. We convert the <code>crop_sold_price_quantityunits</code> to a numeric conversion factor (e.g. <code>price_per_sack_50kg</code> would convert to <code>50</code>. We take the <code>crop_sold_kg_per_year</code> and multiple it by this conversion factor to get the annual income. Please not there is one common exceptions, when the unit is  <code>total_income_per_year<code> we simply take the value from <code>crop_sold_income</code>",
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
    description="The price of a crop per kg. If the unit <code>crop_sold_price_quantityunits</code> was a mass unit (e.g. <code>price_per_sack_50kg</code>) then the price is derived from this. If the unit was <code>total_income_per_year<code>, then the price is calculated by dividing the <code>crop_income_per_year</code> by <code>crop_sold_kg_per_year</code>",
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
    description="Gendered division of crop consumed. Taken by dividing <code>crop_consumed_kg_per_year</code> by the gendered divisions specified in the <code>crop_consume_control</code>",
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
    description="Gendered division of crop consumed. Taken by dividing <code>crop_sold_kg_per_year</code> by the gendered divisions specified in the <code>crop_who_control_revenue</code>",
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
    description="Gendered division of crop income Taken by dividing <code>crop_income_per_year</code> by the gendered divisions specified in the <code>crop_who_control_revenue</code>",
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
    description="The price per whole animal sold. Taken by dividing <code>livestock_sale_income</code> (yearly income from selling livestock) by <code>livestock_sale_income</code> (total annual income from selling that specific livestock).",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("livestock_sold","livestock_sale_income"),
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
    description="The amount of meat obtained per year (kg). Calculated by taking the number of animals <code>killed_for_meat</code>, and converting this to an amount of meat (kg), using  <code>livestock_name</code> and <code>livestock_weights</code>",
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
    description="Amount of meat sold per year (kg). The proprtion of meat sold is calculated using the variables <code>meat_sell_amount</code> and <code>meat_use</code>. This proportion is then applied to <code>meat_kg_per_year</code> to calculate <code>meat_sold_kg_per_year</code>",
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
    description="Amount of meat consumed per year (kg). The proprtion of meat sold is calculated using the variables <code>meat_consumed_amount</code> and <code>meat_use</code>. This proportion is then applied to <code>meat_kg_per_year</code> to calculate <code>meat_consumed_kg_per_year</code>",
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
    description="The price per kg of meat for a specific animal. Calculated by dividing <code>meat_sold_income</code> by <code>meat_sold_kg_per_year</code>",
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
    description="The rate of milk produced during the good season in litres/year. <code>milk_units</code> are converted (e.g. <code>day</code> is given the conversion <code>365</code>). Where units include the number of animals (e.g. <code>l/animal/day</code>), the column <code>milk_number_animals_milked</code> is included in the conversion factor. The final conversion is applied to the <code>milk_amount_good_season</code> to obtain the result",
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
    description="The rate of milk produced during the bad season in litres/year. <code>milk_units</code> are converted (e.g. <code>day</code> is given the conversion <code>365</code>). Where units include the number of animals (e.g. <code>l/animal/day</code>), the column <code>milk_number_animals_milked</code> is included in the conversion factor. The final conversion is applied to the <code>milk_amount_bad_season</code> to obtain the result",
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
    description="The average rate of milk production in litres/year. An average of <code>milk_amount_bad_season_litres_per_year</code> and <code>milk_amount_good_season_litres_per_year</code>",
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
    description="Average amount of milk sold per year (l/year). The proportion of milk sold (<code>milk_sell_amount</code>) is converted to a numeric conversion factor. This conversion factor is applied to <code>milk_collected_litres_per_year</code>",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("milk_sell_amount", "milk_use"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("milk_collected_litres_per_year"),
    function_calculated="milk_sold_and_consumed_calculations",
    search_term="indicator_search_milk_sold_litres_per_year")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "milk_consumed_litres_per_year",
    description="Average amount of milk consumed per year (l/year). The proportion of milk consumed (<code>milk_consumed_amount</code>) is converted to a numeric conversion factor. This conversion factor is applied to <code>milk_collected_litres_per_year</code>",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("milk_consumed_amount", "milk_use"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("milk_collected_litres_per_year"),
    function_calculated="milk_sold_and_consumed_calculations",
    search_term="indicator_search_milk_consumed_litres_per_year")


indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "milk_sold_income_per_year",
    description="Yearly income from milk (lcu/year). <code>milk_sold_price_timeunits</code> is converted to a numeric income conversion factor. This conversion factor is applied to <code>milk_collected_litres_per_year</code>",
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
    description="The rate of egg production during the good season in kg/year. <code>eggs_units</code> are converted (e.g. <code>day</code> is given the conversion <code>365</code>). The conversion is applied to the <code>eggs_amount_good</code> to obtain the result",
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
    description="The rate of egg production during the bad season in kg/year. <code>eggs_units</code> are converted (e.g. <code>day</code> is given the conversion <code>365</code>). The conversion is applied to the <code>eggs_amount_bad</code> to obtain the result",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("eggs_amount_bad", "eggs_units"),
    conversion_tables_required=list("eggs_unit_conversion"),
    api_data_required = list(),
    indicators_required=list(),
    function_calculated="eggs_amount_calculations",
    search_term="indicator_search_eggs_amount_bad_season_kg_per_year")

indicator_list <- add_indicator(
    indicator_list,
    indicator_name = "eggs_collected_kg_per_year",
    description="The average rate of egg production in kg/year. Calculated by average <code>eggs_amount_good_season_kg_per_year</code> and  <code>eggs_amount_bad_season_kg_per_year</code>",
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
    description="Average amount of eggs sold per year (kg/year). The proportion of eggs sold (<code>eggs_sell_amount</code>) is converted to a numeric conversion factor. This conversion factor is applied to <code>eggs_collected_kg_per_year</code>",
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
    description="Average amount of eggs consumed per year (kg/year). The proportion of eggs consumed (<code>eggs_consumed_amount</code>) is converted to a numeric conversion factor. This conversion factor is applied to <code>eggs_collected_kg_per_year</code>",
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
    description="Average amount of income gained from selling eggs (lcu/year). <code>eggs_sold_price_timeunits</code> is converted to a numeric conversion factor. This conversion factor is then applied to <code>eggs_sold_income</code> to obtain an annual income",
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
    description="The price of eggs (lcu/kg). Obtained by dividing <code>eggs_income_per_year</code> by <code>eggs_sold_kg_per_year</code>",
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
    description="The amount of honey harvested (kg/year). <code>bees_honey_production_units</code> is converted into a numeric conversion factor and applied to <code>bees_honey_production</code>.",
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
    description="The amount of honey sold (kg/year). <code>bees_honey_sell_amount</code> is converted into a numeric conversion factor and applied to <code>bees_honey_kg_per_year</code>.",
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
    description="The amount of honey consumed (kg/year). <code>bees_honey_consumed_amount</code> is converted into a numeric conversion factor and applied to <code>bees_honey_kg_per_year</code>.",
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
    description="The price of honey (lcu/year). <code>bees_honey_sold_income</code> is divided by <code>bees_honey_sold_kg_per_year</code>.",
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
    description="Gendered division of <code>livestock_sale_income</code>, split using information in <code>livestock_who_sells</code>",
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
    description="Gendered division of <code>meat_sold_income</code>, split using information in <code>livestock_meat_who_sells</code>",
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
    description="Gendered division of <code>meat_sold_kg_per_year</code>, split using information in <code>livestock_meat_who_sells</code>",
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
    description="Gendered division of <code>meat_consumed_kg_per_year</code>, split using information in <code>livestock_meat_who_control_eating</code>",
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
    description="Gendered division of <code>milk_sold_litres_per_year</code>, split using information in <code>milk_who_sells</code>",
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
    description="Gendered division of <code>milk_sold_income_per_year</code>, split using information in <code>milk_who_sells</code>",
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
    description="Gendered division of <code>milk_consumed_litres_per_year</code>, split using information in <code>milk_who_control_eating</code>",
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
    description="Gendered division of <code>eggs_sold_kg_per_year</code>, split using information in <code>eggs_who_sells</code>",
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
    description="Gendered division of <code>eggs_income_per_year</code>, split using information in <code>eggs_who_sells</code>",
    output_format="loop", #column, loop, #table
    individual_columns_required=list(),
    loop_columns_required=list("eggs_who_sells"),
    conversion_tables_required=list(),
    api_data_required = list(),
    indicators_required=list("eggs_income_per_year"),
    function_calculated="gender_split_livestock"
)

indicator_list <- add_gendered_indicator(
    indicator_list,
    indicator_name = "eggs_consumed_kg_per_year",
    description="Gendered division of <code>eggs_consumed_kg_per_year</code>, split using information in <code>eggs_who_control_eating</code>",
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
    description="Gendered division of <code>bees_honey_sold_kg_per_year</code>, split using information in <code>bees_honey_sold_kg_per_year</code>",
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
    description="Gendered division of <code>bees_honey_sold_income</code>, split using information in <code>bees_who_sells</code>",
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
    description="Gendered division of <code>bees_honey_consumed_kg_per_year</code>, split using information in <code>bees_who_control_eating</code>",
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
    description="The amount of livestock kept by a household in terms of tropical livestock units (TLUs). More info on TLUs can be found here <a href='https://fscluster.org/handbook/Section_one_TLU.html#:~:text=Tropical%20Livestock%20Units%20are%20livestock,indicator%20of%20food%20security%20risk.' target='_blank'>here</a>.",
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
    description="The number of people in the household for at least 3 months of the year",
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
    description="The number of people in the household for at least 3 months of the year in terms of male adult equivalent (MAE). More info on MAE <a href='https://journals.sagepub.com/doi/abs/10.1177/15648265120333s203?journalCode=fnba#:~:text=The%20Adult%20Male%20Equivalent%20(AME)%20was%20devel%2D%20oped%20to,of%20different%20sizes%20and%20compositions.' target='_blank' >here</a>",
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
    description="Total land cultivated by the household (hectares). <code>unitland</code> is converted to a numeric conversion factor and applied to <code>landcultivated</code> to obtain the final result",
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
    description="Total land owned by the household (hectares). <code>unitland</code> is converted to a numeric conversion factor and applied to <code>landowned</code> to obtain the final result",
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
    description="Household food insecurity status (HFIAS). More information here <a href='https://inddex.nutrition.tufts.edu/data4diets/indicator/household-food-insecurity-access-scale-hfias' target='_blank'></a>",
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
    description="Food Insecurity Experience Scale (FIES). More information here <a href='https://www.fao.org/in-action/voices-of-the-hungry/fies/en/' target='_blank'></a>",
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
    description="Household dietary diversity score (HDDS). Considering all foods consumed during the season where food is most abundant. A food group is considered to be consumed if it is consumed daily/weekly. Foods consumed only monthly are not counted. Where the respondent does not specify a good/bad season, they are asked about their consumption over the last month. For more info on HDDS see <a href='https://www.fao.org/nutrition/assessment/tools/household-dietary-diversity/en/#:~:text=Household%20dietary%20diversity%20Score%20(HDDS)%20is%20a%20qualitative%20measure%20of,dietary%20diversity%20at%20individual%20level.' target='_blank'>here</a>",
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
    description="Household dietary diversity score (HDDS). Considering all farm-sourced foods consumed during the season where food is most abundant. A food group is considered to be consumed if it is consumed daily/weekly. Foods consumed only monthly are not counted. Where the respondent does not specify a good/bad season, they are asked about their consumption over the last month. For more info on HDDS see <a href='https://www.fao.org/nutrition/assessment/tools/household-dietary-diversity/en/#:~:text=Household%20dietary%20diversity%20Score%20(HDDS)%20is%20a%20qualitative%20measure%20of,dietary%20diversity%20at%20individual%20level.' target='_blank'>here</a>",
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
    description="Household dietary diversity score (HDDS). Considering all bought foods consumed during the season where food is most abundant. A food group is considered to be consumed if it is consumed daily/weekly. Foods consumed only monthly are not counted. Where the respondent does not specify a good/bad season, they are asked about their consumption over the last month. For more info on HDDS see <a href='https://www.fao.org/nutrition/assessment/tools/household-dietary-diversity/en/#:~:text=Household%20dietary%20diversity%20Score%20(HDDS)%20is%20a%20qualitative%20measure%20of,dietary%20diversity%20at%20individual%20level.' target='_blank'>here</a>",
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
    description="Household dietary diversity score (HDDS). Considering all foods consumed during the lean season. A food group is considered to be consumed if it is consumed daily/weekly. Foods consumed only monthly are not counted. Where the respondent does not specify a good/bad season, they are asked about their consumption over the last month. For more info on HDDS see <a href='https://www.fao.org/nutrition/assessment/tools/household-dietary-diversity/en/#:~:text=Household%20dietary%20diversity%20Score%20(HDDS)%20is%20a%20qualitative%20measure%20of,dietary%20diversity%20at%20individual%20level.' target='_blank'>here</a>",
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
    description="Household dietary diversity score (HDDS). Considering all farm-sourced foods consumed during the lean season. A food group is considered to be consumed if it is consumed daily/weekly. Foods consumed only monthly are not counted. Where the respondent does not specify a good/bad season, they are asked about their consumption over the last month. For more info on HDDS see <a href='https://www.fao.org/nutrition/assessment/tools/household-dietary-diversity/en/#:~:text=Household%20dietary%20diversity%20Score%20(HDDS)%20is%20a%20qualitative%20measure%20of,dietary%20diversity%20at%20individual%20level.' target='_blank'>here</a>",
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
    description="Household dietary diversity score (HDDS). Considering all bought foods consumed during the lean season. A food group is considered to be consumed if it is consumed daily/weekly. Foods consumed only monthly are not counted. Where the respondent does not specify a good/bad season, they are asked about their consumption over the last month. For more info on HDDS see <a href='https://www.fao.org/nutrition/assessment/tools/household-dietary-diversity/en/#:~:text=Household%20dietary%20diversity%20Score%20(HDDS)%20is%20a%20qualitative%20measure%20of,dietary%20diversity%20at%20individual%20level.' target='_blank'>here</a>",
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
    description="Household dietary diversity score (HDDS). Considering all foods consumed during the last month. A food group is considered to be consumed if it is consumed daily/weekly. Foods consumed only monthly are not counted. Where the respondent does not specify a good/bad season, they are asked about their consumption over the last month. For more info on HDDS see <a href='https://www.fao.org/nutrition/assessment/tools/household-dietary-diversity/en/#:~:text=Household%20dietary%20diversity%20Score%20(HDDS)%20is%20a%20qualitative%20measure%20of,dietary%20diversity%20at%20individual%20level.' target='_blank'>here</a>",
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
    description="Household dietary diversity score (HDDS). Considering all farm-sourced foods consumed during the last month. A food group is considered to be consumed if it is consumed daily/weekly. Foods consumed only monthly are not counted. Where the respondent does not specify a good/bad season, they are asked about their consumption over the last month. For more info on HDDS see <a href='https://www.fao.org/nutrition/assessment/tools/household-dietary-diversity/en/#:~:text=Household%20dietary%20diversity%20Score%20(HDDS)%20is%20a%20qualitative%20measure%20of,dietary%20diversity%20at%20individual%20level.' target='_blank'>here</a>",
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
    description="Household dietary diversity score (HDDS). Considering all bought foods consumed during the last month. A food group is considered to be consumed if it is consumed daily/weekly. Foods consumed only monthly are not counted. Where the respondent does not specify a good/bad season, they are asked about their consumption over the last month. For more info on HDDS see <a href='https://www.fao.org/nutrition/assessment/tools/household-dietary-diversity/en/#:~:text=Household%20dietary%20diversity%20Score%20(HDDS)%20is%20a%20qualitative%20measure%20of,dietary%20diversity%20at%20individual%20level.' target='_blank'>here</a>",
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
    description="Household dietary diversity score (HDDS). Considering all foods consumed during the last 24hrs. For more info on HDDS see <a href='https://www.fao.org/nutrition/assessment/tools/household-dietary-diversity/en/#:~:text=Household%20dietary%20diversity%20Score%20(HDDS)%20is%20a%20qualitative%20measure%20of,dietary%20diversity%20at%20individual%20level.' target='_blank'>here</a>",
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
    description="Total income from crops (lcu/year). Taken by adding all of the individual crop incomes in the looped column <code>crop_income_per_year</code>",
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
    description="Total income from the sale of whole livestock and livestock products (lcu/year). Taken by adding all of the individual incomes in the looped columns <code>livestock_sale_income</code>, <code>meat_sold_income</code>, <code>bees_honey_sold_income</code>, <code>milk_sold_income_per_year</code>, <code>eggs_income_per_year</code>",
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
    description="Total income from off-farm activities. <code>offfarm_income_proportion</code> is converted to a numeric proportion. Then off-farm income is calculated using the equation: <br/>I<sub>off</sub> = (p<sub>off</sub> x I<sub>tot</sub>)/(1 - p<sub>off</sub>) <br/>. Where I<sub>off</sub> is off-farm income, p<sub>off</sub> is the proportion of income coming from off-farm activities, I<sub>tot</sub> is the income from crop and livestock sales, ",
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
    description="Total income from <code>off_farm_income_lcu_per_year</code>, <code>crop_income_lcu_per_year</code> and <code>livestock_income_lcu_per_year</code>",
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
    description="The value of crops which were consumed, ",
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
    description="The annual calories from crops produced and consumed in the household, for each individual crop loop (<code>crop_name_1</code>, <code>crop_name_2</code> ...). The <code>crop_calories</code> conversion table is applied to <code>crop_consumed_kg_per_year</code>",
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
    description="The annual calories from meat produced and consumed in the household, for each individual livestock loop (<code>livestock_name_1</code>, <code>livestock_name_1</code> ...).  The <code>meat_calories_kcal_per_kg</code> conversion table is applied to <code>meat_consumed_kg_per_year</code>",
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
    description="The annual calories from eggs produced and consumed in the household, for each individual livestock loop (<code>livestock_name_1</code>, <code>livestock_name_1</code> ...). The <code>eggs_calories_kcal_per_kg</code> conversion table is applied to <code>eggs_consumed_kg_per_year</code>",
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
    description="The annual calories from milk produced and consumed in the household, for each individual livestock loop (<code>livestock_name_1</code>, <code>livestock_name_1</code> ...). The <code>milk_calories_kcal_per_litre</code> conversion table is applied to <code>milk_consumed_litres_per_year</code>",
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
    description="The annual calories from honey produced and consumed in the household, for each individual livestock loop (<code>livestock_name_1</code>, <code>livestock_name_1</code> ...). The <code>bees_honey_calories_kcal_per_kg</code> conversion table is applied to <code>bees_honey_consumed_kg_per_year</code>",
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
    description="The total annual calories from crops produced and consumed in the household, for all crops. Calculated from calories of individual crops consumed <code>crop_calories_consumed_kcal_1</code>, <code>crop_calories_consumed_kcal_2</code> ...",
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
    description="The total annual calories from livestock products which were produced and consumed in the household, for all livestock. Calculated from calories of all livestock products consumed <code>meat_calories_consumed_kcal</code>,<code>eggs_calories_consumed_kcal</code>, <code>milk_calories_consumed_kcal</code>, <code>bees_honey_calories_consumed_kcal</code>",
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
    description="The total annual calories from crop and livestock products which were produced and consumed in the household. Calculated from the sum of <code>crop_consumed_calories_kcal_per_hh_per_year</code> and <code>livestock_consumed_calories_kcal_per_hh_per_year</code>",
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
     description=paste0("Percentage of all value of farm produce controlled by ", gender,". Calculated by looking at the value of all individual products controlled by the ",gender,". Then dividing this by the total value of all products."),
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
     indicator_name="male_adult_bees_honey_sold_income"
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





