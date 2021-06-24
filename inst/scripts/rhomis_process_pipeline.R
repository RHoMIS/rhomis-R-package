

# Setup -------------------------------------------------------------------
library(rhomis)
library(knitr)

# Loading environemnt variables from .env file
readRenviron(".env")

central_url <- "https://central.rhomis.cgiar.org"
# Accessing the environemnt variables
central_email <- Sys.getenv("RHOMIS_CENTRAL_EMAIL")
central_password <- Sys.getenv("RHOMIS_CENTRAL_PASSWORD")

# The name of the project we are interested in
project_name <- "Leo Test 1"
country_code <- "VN"


# Linkning to ODK Central -------------------------------------------------

# Get a list of the different central projects
projects <-get_projects(central_url,
                        central_email,
                        central_password)

#' Identify which project ID matches the project
#' name we are interested in
projectID <- projects$id[projects$name==project_name]

forms <- get_forms(central_url,
                   central_email,
                   central_password,
                   projectID)
#kable(forms)

# We are interested in the first form from this project
formID <- forms$xmlFormId[1]

rhomis_data <- get_submission_data(central_url,
                                   central_email,
                                   central_password,
                                   projectID,
                                   formID )
## Cleaning Data and Extracting All Units/Column names present in the survey

rhomis_data <-rhomis_data %>%
    remove_extra_central_columns() %>%
    convert_all_columns_to_lower_case()


### Adding a country code column for easier indexing
iso_country_code <- tibble::as_tibble(list(iso_country_code=rep(country_code,nrow(rhomis_data))))
rhomis_data <- add_column_after_specific_column(rhomis_data,
                                                new_data = iso_country_code,
                                                new_column_name = "iso_country_code",
                                                old_column_name = "country",
                                                loop_structure = F)

all_new_values <- extract_units_data_frames(rhomis_data)

## Household Information
hh_size_members <- calculate_household_size_members(rhomis_data)
hh_size_MAE <- calculate_MAE(rhomis_data)

household_type <- rhomis_data[["household_type"]]
head_education_level <- rhomis_data[["education_level"]]

## Land Variables
land_sizes <- land_size_calculation(rhomis_data)

## Livestock Holdings
# NOT YET CALCULATED

#FoodSecMonths
worst_food_security_month <- rhomis_data[["food_worst_month"]]
best_food_security_month <- rhomis_data[["food_best_month"]]
#
#ppi_score <- ppi_score(rhomis_data, country_code_column = rhomis_data$iso_country_code)
food_security <- food_security_calculations(rhomis_data)

# HDDS scores

hdds_calc(rhomis_data)

# Crop Calculations
rhomis_data <- crop_calculations_all(rhomis_data,
                      crop_yield_units_all = crop_yield_units$unit,
                      crop_yield_unit_conversions_all = crop_yield_units$conversion,
                      crop_income_units_all = crop_price_units$unit,
                      crop_income_unit_conversions_all = crop_price_units$conversion)

#Livestock Calculaions
rhomis_data <- livestock_calculations_all(rhomis_data,
                                          livestock_weights_names = livestock_weights$animal,
                                          livestock_weights_conversions = livestock_weights$weight_kg,
                                          eggs_amount_units_all = eggs_amount_units$unit,
                                          eggs_amount_unit_conversions_all = eggs_amount_units$conversion_factor,
                                          eggs_price_time_units_all = eggs_price_time_units$unit,
                                          eggs_price_time_unit_conversions_all = eggs_price_time_units$conversion_factor,
                                          honey_amount_units_all = honey_amount_units$units,
                                          honey_amount_unit_conversions_all = honey_amount_units$conversion_factors,
                                          milk_amount_units_all = milk_amount_units$unit,
                                          milk_amount_unit_conversions_all = milk_amount_units$conversion_factor,
                                          milk_price_time_units_all = milk_price_time_units$unit,
                                          milk_price_time_unit_conversions_all = milk_price_time_units$conversion_factor)


# Total Income Calculations
crop_income <- total_crop_income(rhomis_data)
livestock_income <- total_livestock_income(rhomis_data)
total_and_off_farm_income <- total_and_off_farm_incomes(rhomis_data,
                           total_crop_income = crop_income,
                           total_livestock_income = livestock_income)
total_income <- total_and_off_farm_income$total_income
off_farm_income <- total_and_off_farm_income$off_farm_income

rhomis_data <- gendered_off_farm_income_split(rhomis_data)


# Extra Outputs

crop_prefixes <- c("crop_harvest_kg_per_year",
                   "crop_consumed_kg_per_year",
                   "crop_sold_kg_per_year",
                   "crop_income_per_year",
                   "crop_price"
                   )
data_types <- c("num",
                "num",
                "num",
                "num",
                "num")
crop_data <- map_to_wide_format(data = rhomis_data,
                                 name_column = "crop_name",
                                 column_prefixes =crop_prefixes,
                                 types = data_types)

livestock_prefixes <- c("livestock_sold",
                        "livestock_sale_income",
                        "livestock_price_per_animal",

                        "meat_kg_per_year",
                        "meat_consumed_kg_per_year",
                        "meat_sold_kg_per_year",
                        "meat_sold_income",
                        "meat_price_per_kg",

                        "milk_collected_litres_per_year",
                        "milk_consumed_litres_per_year",
                        "milk_sold_litres_per_year",
                        "milk_sold_income_per_year",
                        "milk_price_per_litre",

                        "eggs_collected_kg_per_year",
                        "eggs_consumed_kg_per_year",
                        "eggs_sold_kg_per_year",
                        "eggs_income_per_year",
                        "eggs_price_per_kg")


data_types <- c("num",
                "num",
                "num",

                "num",
                "num",
                "num",
                "num",
                "num",

                "num",
                "num",
                "num",
                "num",
                "num",

                "num",
                "num",
                "num",
                "num",
                "num")

livestock_data <- map_to_wide_format(data = rhomis_data,
                                     name_column = "livestock_name",
                                     column_prefixes =livestock_prefixes,
                                     types = data_types)

# off_farm_prefixes <- c("")
# data_types <- c("")
#
# off_farm_data <- map_to_wide_format(data = rhomis_data,
#                                      name_column = "offfarm_income_name",
#                                      column_prefixes =off_farm_prefixes,
#                                      types = data_types)




# write_new_collection(data_to_write = rhomis_data,
#                      collection = "processedData",
#                      database = "rhomis",
#                      url = "mongodb://localhost")


add_data_to_project_list(data = rhomis_data,
                         collection = "processedData",
                         database = "rhomis",
                         url = "mongodb://localhost")
