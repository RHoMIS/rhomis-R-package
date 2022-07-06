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
    function_calculated=list("run_preliminary_calculations"),
    search_term="indicator_search_currency_conversion_lcu_to_ppp")


find_nested_dependencies_list(
     indicator_name="currency_conversion_lcu_to_ppp", 
     indicator_list=indicator_list, 
     dependency_required="individual")


dependency_network <- find_nested_dependencies_network(
     indicator_name="currency_conversion_lcu_to_ppp", 
     indicator_list=indicator_list
)





plot_dependency_network()


jsonlite::toJSON(indicator_list, pretty=T)

json_mapping <- '
{


     "currency_conversion_lcu_to_ppp":{
         "indicator_name":  "currency_conversion_lcu_to_ppp",
         "format": "column",
         "individual_columns_required": [],
         "loop_columns_required": [],
         "conversion_tables_required": [],
         "api_data_required": ["world_bank_api"],
         "indicators_required": ["iso_country_code","year"],
         "file_located":  "CurrencyConversion.R"
    },

     "survey_length_minutes":{
         "indicator_name":  "survey_length_minutes",
         "format": "column",
         "individual_columns_required": ["start_time_user","end_time_user"],
         "loop_columns_required": [],
         "conversion_tables_required": [],
         "api_data_required": [],
         "indicators_required": [],
         "file_located":  "redirectModules.R"
    },

     "crop_harvest_kg_per_year":{
         "indicator_name":  "crop_harvest_kg_per_year",
         "format": "loop",
         "individual_columns_required": [],
         "loop_columns_required": ["crop_name",
                                   "crop_yield",
                                   "crop_yield_units",
                                   "crop_yield_units_other"],
         "conversion_tables_required": ["crop_name", "crop_yield_units"],
         "api_data_required": [],
         "indicators_required": [],
         "file_located":  "CropCalculations.R"
    },

    "crop_consumed_kg_per_year":{
         "indicator_name":  "crop_consumed_kg_per_year",
         "format": "loop",
         "individual_columns_required": [],
         "loop_columns_required": ["crop_name",
                                   "crop_yield",
                                   "crop_yield_units",
                                   "crop_yield_units_other",
                                   "crop_use",
                                   "crop_consumed_prop"],
         "conversion_tables_required": ["crop_name", "crop_yield_units","proportion_conversions"],
         "api_data_required": [],
         "indicators_required": ["crop_harvest_kg_per_year"],
         "file_located":  "CropCalculations.R"
    },

    "crop_sold_kg_per_year":{
         "indicator_name":  "crop_sold_kg_per_year",
         "format": "loop",
         "individual_columns_required": [],
         "loop_columns_required": ["crop_name",
                                   "crop_yield",
                                   "crop_yield_units",
                                   "crop_yield_units_other",
                                   "crop_use",
                                   "crop_sold_prop"],
         "conversion_tables_required": ["crop_name", "crop_yield_units","proportion_conversions"],
         "api_data_required": [],
         "indicators_required": ["crop_harvest_kg_per_year"],
         "file_located":  "CropCalculations.R"
    },

    "crop_income_per_year":{
         "indicator_name":  "crop_sold_kg_per_year",
         "format": "loop",
         "individual_columns_required": [],
         "loop_columns_required": ["crop_name",
                                   "crop_yield",
                                   "crop_yield_units",
                                   "crop_yield_units_other",
                                   "crop_use",
                                   "crop_sold_prop"],
         "conversion_tables_required": ["crop_name",
                                        "crop_yield_units",
                                        "proportion_conversions",
                                        "crop_price_units"
                                       ],
         "api_data_required": [],
         "indicators_required": ["crop_harvest_kg_per_year", "crop_sold_kg_per_year"],
         "file_located":  "CropCalculations.R"
    },

    "crop_price_per_kg":{
         "indicator_name":  "crop_price_per_kg",
         "format": "loop",
         "individual_columns_required": [],
         "loop_columns_required": ["crop_name",
                                   "crop_yield",
                                   "crop_yield_units",
                                   "crop_yield_units_other",
                                   "crop_use",
                                   "crop_sold_prop",
                                   "crop_sold_price_quantityunits",
                                   "crop_price_quantityunits_other"],
         "conversion_tables_required": ["crop_name",
                                        "crop_yield_units",
                                        "proportion_conversions"],
         "api_data_required": [],
         "indicators_required": ["crop_harvest_kg_per_year", "crop_sold_kg_per_year", "crop_income_per_year"],
         "file_located":  "CropCalculations.R"
    },
}
'

jsonlite::fromJSON(json_mapping,simplifyVector = F)
