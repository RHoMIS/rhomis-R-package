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



new_indicator <- function(indicator_name,
                           format=c("column", "loop", "table"),
                           individual_columns_required=c(),
                           loop_columns_required=c(),
                           conversion_tables_required=c(),
                           api_data_required=c(),
                           indicators_required=c(),
                           file_located

){
    # Checking types
    stopifnot(is.character(indicator_name))
    format <- match.arg(format)
    stopifnot(is.atomic(individual_columns_required))
    stopifnot(is.atomic(individual_columns_required))
    stopifnot(is.atomic(individual_columns_required))
    stopifnot(is.character(file_located))


    indicator <- list(
        indicator_name=indicator_name,
        format=format,
        individual_columns_required=individual_columns_required,
        loop_columns_required=loop_columns_required,
        conversion_tables_required=conversion_tables_required,
        api_data_required=api_data_required,
        indicators_required=indicators_required,
        file_located=file_located
    )

    class(indicator) <- "Indicator"

    return(indicator)
}

add_indicator <- function(indicator_list, indicator){

    stopifnot(class(indicator)=="Indicator")
    if (indicator$indicator_name %in% names(indicator_list)){
        stop("Indicator already exists in list")
    }

    temp_list <- list(
        indicator=indicator
    )
    names(temp_list) <- indicator$indicator_name

    return(
        append(indicator_list, temp_list)
    )
}


indicator_list <- list()

iso_country_code <- new_indicator(indicator_name = "iso_country_code",format="column",individual_columns_required=c("country"), loop_columns_required=c(),conversion_tables_required=c("country_name_conversions"),api_data_required = c(),indicators_required=c(),file_located="redirectModules.R")

indicator_list <- add_indicator(indicator_list = indicator_list,
              indicator = iso_country_code)






json_mapping <- '
{
    "iso_country_code":{
         "indicator_name":  "iso_country_code",
         "format": "column",
         "individual_columns_required": ["country"],
         "loop_columns_required": [],
         "conversion_tables_required": ["country_name_conversions"],
         "api_data_required": [],
         "indicators_required": [],
         "file_located": "redirectModules.R"
    },

    "year":{
         "indicator_name":  "year",
         "format": "column",
         "individual_columns_required": ["start_time_user"],
         "loop_columns_required": [],
         "conversion_tables_required": [],
         "api_data_required": [],
         "indicators_required": [],
         "file_located": "redirectModules.R"
    },

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
