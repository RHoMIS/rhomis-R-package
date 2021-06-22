

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

# Get data on the different central users
users <- get_users(central_url,
                   central_email,
                   central_password)
users

# Get a list of the different central projects
projects <-get_projects(central_url,
                        central_email,
                        central_password)
projects

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

# Get data on the
submissions_list <- get_submissions_list(central_url,
                                         central_email,
                                         central_password,
                                         projectID,
                                         formID)
submissions_list

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




