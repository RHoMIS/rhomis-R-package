#!/bin/env Rscript

# Quick how-to  -----------------------------------------------------------


#' This is a script to run calculations on RHoMIS data. The script can be run in
#' two ways, you can run the script interactively (e.g. from Rstudio). Or you can run the
#' script from the command line. If running from the command line you will need to
#' enter the command:
#'
#' "Rscript path/to/file.R --arg1 firstargument --arg2 second argument ..."
#'
#' If you would like guidance on how to run this script from the command line, please
#' enter:
#'
#' "Rscript path/to/file.R --help"
#'
#' Some short documentation will appear, showing which arguments need to be passed and providing
#' a brief description. In both cases, "path/to/file.R" must reflect where the file is
#' located on your system

####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
# SETUP
####################################################################################################################
###################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################


#' Loading the virtual environment library
# library(renv, warn.conflicts = F)
#
# # Setup for interactive script running  -------------------------------------------------------------------
#
# #stop(interactive())
#
#
# # Checking if R running from GUI
# if (interactive()){
#     renv::load()
#
#     # Loading environment variables
#     readRenviron(".env")
#
#     # Setting options for script run interactively.
#     # These should be set manually if you are running the script interactively
#     opt <- list()
#     opt$projectName <- "test_project_from_node"
#     opt$formName <- "RHoMIS 1.6"
#     opt$dataBase <- "rhomis-data-dev"
#
# }

# Setup for running from command line -------------------------------------
# Checking if script has been run from the command line
#' if (!interactive()){
#'     # Directory setup in the .Rprofile
#'     # Identifying the file path to this script
#'
#'     #Arguments passed to the script
#'     initial.options <- commandArgs(trailingOnly = FALSE)
#'     file_option <- initial.options[grep("--file",initial.options)]
#'     file_option <- gsub("--file=", "",file_option, fixed=T)
#'
#'
#'     #project_path <- gsub("/R/process_data.R", "",file_option, fixed=T)
#'     project_path <- gsub("R/process_data.R", "",file_option, fixed=T)
#'
#'
#'     #Making sure scripts can deal with running from within the project directory
#'
#'     if (grepl("home/",project_path==F) | project_path==""){
#'
#'         project_path <- paste0("./",project_path)
#'     }
#'
#'     print(project_path)
#'
#'
#'     #' Ensures that all warnings are
#'     #' send to stdout rather than stderr
#'     sink(stdout(), type="message")
#'
#'
#'
#'     # Loading virtual environment
#'     #loading .env file
#'     readRenviron(paste0(project_path,".env"))
#'
#'     # renv::load(project_path)
#'
#'     # Ensuring that the
#'     library(optparse, warn.conflicts = F) # Library for parsing flags when the R-script is call
#'
#'     #' Setting up options for calling this script from
#'     #' the terminal or the command line
#'     option_list <- list(
#'         optparse::make_option(opt_str = c( "--projectName"),
#'                               type = "character",
#'                               # default = "hello",
#'                               help="The name for the project you would like to process on ODK central",
#'                               metavar="character"),
#'         optparse::make_option(opt_str = c("--formName"),
#'                               type = "character",
#'                               # default = "world",
#'                               help="The name of the form you would like to process on ODK central",
#'                               metavar="character"),
#'         optparse::make_option(opt_str = c("--dataBase"),
#'                               type = "character",
#'                               # default = "world",
#'                               help="The database you would like to write to",
#'                               metavar="character")
#'     )
#'
#'     # Extracting arguments
#'     opt_parser <- optparse::OptionParser(option_list = option_list)
#'     opt <- optparse::parse_args(opt_parser)
#'
#'     if (length(opt)==0){
#'         optparse::print_help(opt_parser)
#'         stop("At least one argument must be supplied (input file).n", call.=FALSE)
#'     }
#' }
#'

####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
# DATA PROCESSING
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################
####################################################################################################################


# Loading Packages and Environment Variables  --------------------------------------------------------

#' Loading libraries. Suppress masking
#' warnings so they do not get passed to
#' stdout
#'
#'
library(rhomis, warn.conflicts = F)


readRenviron(".env")

# run r functions as follows
#eval(parse(text=my_r_call))


opt <- list()

opt$projectName <- "xyz"
opt$formName <- "RHoMIS 1.6"
opt$dataBase <- "rhomis-data-test"


central_url <- Sys.getenv("CENTRALURL")
central_url <- paste0("https://",central_url)

# Accessing the environemnt variables
central_email <- Sys.getenv("CENTRALEMAIL")
central_password <- Sys.getenv("CENTRALPASSWORD")


# Test example ------------------------------------------------------------
# Write a file containing two arguments passed vis
# central_email <- Sys.getenv("RHOMIS_CENTRAL_EMAIL")
# #Calling R functions
# fileConn <- file(paste0(opt$dir,"/test_output.txt"))
# writeLines(c(opt$arg1,opt$arg2, central_email), fileConn)
# close(fileConn)
#------------------------------------------------------------------------

# Linkning to ODK Central -------------------------------------------------

project_name <- opt$projectName
form_name <- opt$formName



# Finding project information from the API
projectID <- get_project_id_from_name(project_name,
                                      central_url,
                                      central_email,
                                      central_password)

#Finding form information from the API
formID <- get_xml_form_id_from_name(form_name,
                                    projectID,
                                    central_url,
                                    central_email,
                                    central_password)

# Get form and extract metadata
central_form <- rhomis::get_xls_form(
    central_url=central_url,
    central_email=central_email,
    central_password=central_password,
    projectID=projectID,
    formID=formID,
    version = 1
)

#### Identifying the modules uses in the survey

library(tibble)
library(dplyr)



survey_modules <- unique(central_form[["module_name"]])
survey_modules <- survey_modules[!is.na(survey_modules)]
survey_modules <- tolower(survey_modules)

new_survey_modules <- survey_modules[survey_modules %in% rhomis::modules[["module_name"]]==F]

if (length(new_survey_modules)>0)
{
    new_module_tibble <- tibble::tibble(module_name=new_survey_modules)
    new_module_tibble$module_type <- "optional"
    new_module_tibble$processing_code <- NA
    new_module_tibble$dependencies <- NA


    modules_used <- dplyr::bind_rows( rhomis::modules,new_module_tibble)
}
save_data_set_to_db(data = modules_used,
                         data_type  = "moduleData",
                         database = opt$dataBase,
                         url = "mongodb://localhost",
                         projectID=project_name,
                         formID=form_name)



rhomis_data <- get_submission_data(central_url,
                                   central_email,
                                   central_password,
                                   projectID,
                                   formID )

if (sum(colnames(rhomis_data)=="deviceid")>1){
    column_to_keep <- which(colnames(rhomis_data)=="deviceid" & colSums(is.na(rhomis_data))==0)
    column_to_remove <- which(colnames(rhomis_data)=="deviceid" & colSums(is.na(rhomis_data))>0)

    rhomis_data <- rhomis_data[-column_to_remove]

}

## Cleaning Data and Extracting All Units/Column names present in the survey

#

rhomis_data <-rhomis_data %>%
    remove_extra_central_columns() %>%
    convert_all_columns_to_lower_case()


# all_new_values <- extract_units_data_frames(rhomis_data)

indicator_data <- tibble::as_tibble(list(projectName=rep(project_name,nrow(rhomis_data)),
                                         formName=rep(form_name,nrow(rhomis_data))))

## Demographics
if("demographics" %in% modules_used$module_name)
{
    indicator_data$hh_size_members <- calculate_household_size_members(rhomis_data)
    indicator_data$hh_size_MAE <- calculate_MAE(rhomis_data)

    indicator_data$household_type <- rhomis_data[["household_type"]]
    indicator_data$head_education_level <- rhomis_data[["education_level"]]

}
## Land_use
if("land_use" %in% modules_used$module_name)
{
    indicator_data <- dplyr::bind_cols(indicator_data,land_size_calculation(rhomis_data))
}
## Livestock Holdings
# To be changed soon



#FoodSecMonths
if("food_security" %in% modules_used$module_name)
{

    indicator_data$worst_food_security_month <- rhomis_data[["food_worst_month"]]
    indicator_data$best_food_security_month <- rhomis_data[["food_best_month"]]
    indicator_data <- dplyr::bind_cols(indicator_data,food_security_calculations(rhomis_data))

}

#ppi_score <- ppi_score(rhomis_data, country_code_column = rhomis_data$iso_country_code)

# HDDS scores
if("food_security" %in% modules_used$module_name)
{
    hdds_data <-  hdds_calc(rhomis_data)
}
# Crop Calculations
if ("crops" %in% modules_used$module_name)
{



    rhomis_data <- crop_calculations_all(rhomis_data,
                                         crop_yield_units_all = crop_yield_units$unit,
                                         crop_yield_unit_conversions_all = crop_yield_units$conversion,
                                         crop_income_units_all = crop_price_units$unit,
                                         crop_income_unit_conversions_all = crop_price_units$conversion)


    crop_data <- map_to_wide_format(data = rhomis_data,
                                    name_column = "crop_name",
                                    column_prefixes =c("crop_harvest_kg_per_year",
                                                       "crop_consumed_kg_per_year",
                                                       "crop_sold_kg_per_year",
                                                       "crop_income_per_year",
                                                       "crop_price"),
                                    types = c("num","num","num","num","num"))

    save_data_set_to_db(data = crop_data$crop_harvest_kg_per_year,
                        data_type = "cropData",
                        database = opt$dataBase,
                        url = "mongodb://localhost",
                        projectID=project_name,
                        formID=form_name)
}

#Livestock Calculaions
if ("livestock" %in% modules_used$module_name)
{
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


    livestock_data <- map_to_wide_format(data = rhomis_data,
                                         name_column = "livestock_name",
                                         column_prefixes =c("livestock_sold",
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
                                                            "eggs_price_per_kg"),
                                         types = c("num","num","num","num","num","num","num","num", "num","num","num","num","num","num","num","num","num","num"))


    livestock_sold <- map_to_wide_format(rhomis_data,"livestock_name","livestock_sold",types = "num")
    save_data_set_to_db(data = livestock_sold$livestock_sold,
                        data_type = "livestockData",
                        database = opt$dataBase,
                        url = "mongodb://localhost",
                        projectID=project_name,
                        formID=form_name)
}
# Totals
if ("livestock" %in% modules_used$module_name & "crops" %in% modules_used$module_name)
{
    indicator_data$crop_income <- total_crop_income(rhomis_data)
    indicator_data$livestock_income <- total_livestock_income(rhomis_data)
}

if(!is.null( indicator_data$crop_income) & !is.null(indicator_data$livestock_income) & ("off_farm_income" %in% modules_used$module_name | "off_farm_incomes" %in% modules_used$module_name ))
{
    total_and_off_farm_income <- total_and_off_farm_incomes(rhomis_data,
                                                            total_crop_income = indicator_data$crop_income,
                                                            total_livestock_income = indicator_data$livestock_income)
    indicator_data$total_income <- total_and_off_farm_income$total_income
    indicator_data$off_farm_income <- total_and_off_farm_income$off_farm_income

    rhomis_data <- gendered_off_farm_income_split(rhomis_data)
}

if("off_farm_income" %in% modules_used$module_name | "off_farm_incomes" %in% modules_used$module_name ){

    off_farm_prefixes <- c("offfarm_year_round", "offfarm_month","offfarm_who_control_revenue")
    data_types <- c("chr", "chr","chr")

    off_farm_data <- map_to_wide_format(data = rhomis_data,
                                        name_column = "offfarm_income_name",
                                        column_prefixes =off_farm_prefixes,
                                        types = data_types)


}

save_data_set_to_db(data = rhomis_data,
                    data_type = "processedData",
                    database = opt$dataBase,
                    url = "mongodb://localhost",
                    projectID=project_name,
                    formID=form_name)

save_data_set_to_db(data = indicator_data,
                    data_type = "indicatorData",
                    database = opt$dataBase,
                    url = "mongodb://localhost",
                    projectID=project_name,
                    formID=form_name)


# adding_project_to_list(database = opt$dataBase,
#                        url = "mongodb://localhost",
#                        projectID=project_name,
#                        formID=form_name)

# Finishing whole process
write("Success from Rscript", stdout())
