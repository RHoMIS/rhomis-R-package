library(readr)
library(mongolite)
library(dplyr)
library(jsonlite)

#' Process RHoMIS data
#'
#' A main function that can be used to process rhomis
#' data. Whether the dataset comes from a local csv
#' or from ODK central
#'
#' @param dataSource The type of RHoMIS data being fed into the
#' calculations, whether a local csv file or data from ODK central.
#' Options "csv" or "central".
#' @param outputType  The type of output to produce (options are "csv" or "mongodb")
#' @param coreOnly Indicating whether to only analyze the RHoMIS core information (TRUE/FALSE)
#' If TRUE, extra variables will be saved but they will not be processed into indicators.
#' If FALSE, you must either provide the survey xls file so that the modules used can be identified,
#' or ensure that the project you are accessing can be found on ODK central.
#' @param surveyFile The path to the surveyxls file. Only necessary if "coreOnly"=FALSE and dataSource="local".
#' @param extractUnits Whether or not to only extract units (TRUE/FALSE)
#' @param processDataSet Whether to process the whole dataset (TRUE/FALSE)
#' @param moduleSaving Whether or not to use the form to identify the modules which
#' were included in the survey (TRUE/FALSE)
#' @param dataFilePath The file to the data (csv format).
#' ONLY RELEVANT IF "dataSource" WAS "local".
#' @param central_url The url of the ODK-central server you are using.
#' ONLY RELEVANT IF "dataSource" WAS "central".
#' @param central_email The email of the ODK-central account you are using.
#' ONLY RELEVANT IF "dataSource" WAS "central"
#' @param central_password The password of the ODK-central account you are using.
#' ONLY RELEVANT IF "dataSource" WAS "central".
#' @param project_name The name of the ODK-central project you are processing.
#' ONLY RELEVANT IF "dataSource" WAS "central".
#' @param form_name The name of the ODK-central form you are processing.
#' ONLY RELEVANT IF "dataSource" WAS "central".
#' @param form_version The version of the ODK-central form you are processing.
#' ONLY RELEVANT IF "dataSource" WAS "central".
#' @param database The name of the database you would like to save results to
#' @param draft Whether or not the ODK for you war working with is a draft
#' or a final version. Only relevant if you are processing a project from ODK central
#'
#' @return
#' @export
#'
#' @examples
processData <- function(
    dataSource="csv",
    outputType="csv",
    coreOnly=T,
    surveyFile=NULL,
    moduleSaving=F,
    extractUnits=T,
    processDataSet=F,
    dataFilePath=NULL,
    central_url=NULL,
    central_email=NULL,
    central_password=NULL,
    project_name=NULL,
    form_name=NULL,
    form_version=NULL,
    database=NULL,
    draft=NULL){



    options(nwarnings = 10000)


    # Print warnings as they occur.
    options(warn = 1)

    # Create variable to store the warnings.
    warns <- vector("character")

    # Create connection for the sink.
    warn_connection <- textConnection("warns", "wr", local = FALSE)

    # Start collecting.
    sink(warn_connection, type = "message")



    #---------------------------------------------------------------
    # Ensuring the parameter calls are logically consistent
    #---------------------------------------------------------------
    tryCatch(
        expr ={



                replace_infinite <- function(column){
                    column[is.infinite(column)] <- NA
                    return(column)
                }

                if(extractUnits==T & processDataSet==T){
                    stop("Stated that you wanted to extract units and process the data in in the same step.\n
                 This is not possible as survey units first need to be checked.\n
                 Please work in the following order:\n
                 1. Extract new units and verify them.\n
                 3. Calculate indicators\n")
                }


                if(dataSource!="csv"&dataSource!="central"){
                    stop("Raw must come from ODK central or a local source (csv)")
                }
                if(dataSource=="csv"){
                    if(is.null(dataFilePath)){
                        stop('You specified the data was coming from a local csv but have not specified a "dataFilePath"')
                    }
                }
                # Checking if the right arguments are supplied to obtain data from ODK central
                if(dataSource=="central"){
                    # central_url <- "abc"
                    # central_email <- "def"
                    # central_password <- NULL
                    # project_name <- NULL
                    # form_name <- "name"
                    # form_version <- "version"
                    # dbURL <- "abc"
                    items_to_test <- list("central_email",
                                          "central_password",
                                          "project_name",
                                          "form_name",
                                          "form_version",
                                          "database",
                                          "draft")
                    null_variables <-sapply(items_to_test, function(x) is.null(get(x)))
                    if(any(null_variables)){
                        error_message <- paste(items_to_test[null_variables], collapse="\n")
                        stop(paste0('You specified the data was coming from a ODK central. You need to define: \n',error_message))
                    }
                }


                if(outputType!="csv"&outputType!="mongodb"){
                    stop("Must specify whether to save the output locally or in a mongodb")
                }

                #---------------------------------------------------------------
                # Loading Submission Data
                #---------------------------------------------------------------

                if(dataSource=="csv")
                {
                    rhomis_data <- readr::read_csv(dataFilePath, col_types = readr::cols(), na = c("n/a","-999","NA"))
                    colnames(rhomis_data) <- clean_column_names(colnames(rhomis_data),
                                                                repeat_columns = c("crop_repeat",
                                                                                   "livestock_repeat",
                                                                                   "offfarm_repeat",
                                                                                   "offfarm_income_repeat",
                                                                                   "hh_pop_repeat",
                                                                                   "hh_rep")) %>% tolower()



                    rhomis_data<- convert_all_columns_to_lower_case(rhomis_data)



                    indicator_data <- tibble::as_tibble(list(
                        source = rep(dataFilePath, nrow(rhomis_data))
                    ))


                }

                if(dataSource=="central")
                {
                    # Getting project and formID
                    projectID <- get_project_id_from_name(
                        project_name,
                        central_url,
                        central_email,
                        central_password
                    )

                    # Finding form information from the API
                    formID <- get_xml_form_id_from_name(
                        form_name,
                        projectID,
                        central_url,
                        central_email,
                        central_password
                    )

                    # Getting the submission data from ODK central
                    rhomis_data <- get_submission_data(
                        central_url,
                        central_email,
                        central_password,
                        projectID,
                        formID,
                        draft
                    )





                    colnames(rhomis_data) <- clean_column_names(colnames(rhomis_data),
                                                                repeat_columns = c("crop_repeat",
                                                                                   "livestock_repeat",
                                                                                   "offfarm_repeat",
                                                                                   "offfarm_income_repeat",
                                                                                   "hh_pop_repeat",
                                                                                   "hh_rep")) %>% tolower()
                    rhomis_data <- rhomis_data %>%
                        remove_extra_central_columns()


                    rhomis_data<- convert_all_columns_to_lower_case(rhomis_data)


                    indicator_data <- tibble::as_tibble(list(
                        projectName = rep(project_name, nrow(rhomis_data)),
                        formName = rep(form_name, nrow(rhomis_data)),
                        formVersion = rep(form_version, nrow(rhomis_data))
                    ))
                }


                #---------------------------------------------------------------
                # Loading Form Data
                #---------------------------------------------------------------
                if(dataSource=="csv")
                {
                    if (coreOnly==F | moduleSaving==T)
                    {
                        xls_form <- list()
                        survey_form <- readxl::read_xlsx(surveyFile,sheet = "survey")
                    }
                }

                if(dataSource=="central")
                {
                    central_form <- rhomis::get_xls_form(
                        central_url = central_url,
                        central_email = central_email,
                        central_password = central_password,
                        projectID = projectID,
                        formID = formID,
                        form_version = form_version,
                        draft=draft
                    )
                    survey_form <- central_form$survey
                }


                #---------------------------------------------------------------
                # Save Modules
                #---------------------------------------------------------------

                if (moduleSaving==T){
                    survey_modules <- unique(survey_form[["module_name"]])
                    survey_modules <- survey_modules[!is.na(survey_modules)]
                    survey_modules <- tolower(survey_modules)


                    new_survey_modules <- survey_modules[survey_modules %in% rhomis::modules[["module_name"]] == F]
                    if (length(new_survey_modules) > 0) {
                        new_module_tibble <- tibble::tibble(module_name = new_survey_modules)
                        new_module_tibble$module_type <- "optional"
                        new_module_tibble$processing_code <- NA
                        new_module_tibble$dependencies <- NA


                        modules_used <- dplyr::bind_rows(rhomis::modules, new_module_tibble)
                    }
                    if (length(new_survey_modules) == 0) {
                        modules_used <- rhomis::modules
                    }

                    if (outputType=="csv"){
                        dir.create("modules", showWarnings = F)
                        readr::write_csv(modules_used,"modules/modules.csv")
                    }

                    if (outputType=="mongodb"){
                        save_data_set_to_db(
                            data = modules_used,
                            data_type = "moduleData",
                            database = database,
                            url = "mongodb://localhost",
                            projectID = project_name,
                            formID = form_name
                        )
                    }
                }

                #---------------------------------------------------------------
                # Extract and write units
                #---------------------------------------------------------------

                if(extractUnits==T)
                {
                    units_and_conversions <- extract_units_data_frames(rhomis_data)
                    units_and_conversions <- check_existing_conversions(units_and_conversions)

                    if(outputType=="csv"){
                        write_units_to_folder(units_and_conversions)
                    }

                    if(outputType=="mongodb"){
                        save_multiple_conversions(database = database,
                                                  url = url,
                                                  projectID = project_name,
                                                  formID = form_name,
                                                  conversion_data = units_and_conversions,
                                                  conversion_types =  names(units_and_conversions)

                        )
                    }
                }


                #---------------------------------------------------------------
                # Swap "Other" with Standard values
                #---------------------------------------------------------------
                if (processDataSet==T )
                {
                    #---------------------------------------------------------------
                    # Load Conversions
                    #---------------------------------------------------------------

                    if (outputType=="csv"){
                        if (!dir.exists("./unit_conversions"))
                        {
                            stop('Specified that the units were stored locally but the path "unit_conversions" does not exist')
                        }

                        file_names <- list.files("./unit_conversions")
                        #---------------------------------------------
                        # Loading all of the unit conversions locally
                        #---------------------------------------------
                        load_local_units(file_names)
                    }


                    if (outputType=="mongodb")
                    {
                        unit_list <- find_db_units(projectID=project_name,
                                                   formID=form_name,
                                                   url = "mongodb://localhost",
                                                   collection = "projectData",
                                                   database = database)
                        # Not yet complete
                        load_all_db_units(unit_list,
                                          projectID=project_name,
                                          formID=form_name,
                                          database = database)
                    }

                    # Replacing crop and livestock names with their "other"
                    rhomis_data <- replace_crop_and_livestock_other(rhomis_data)

                    number_crop_loops <- find_number_of_loops(rhomis_data,"crop_name")
                    crop_loops <- paste0("crop_name_",1:number_crop_loops)

                    # Checks if columns are missing, if columns do not exist then they are returned
                    crop_name_in_data <- check_columns_in_data(rhomis_data, loop_columns = "crop_name")

                    if(length(crop_name_in_data)==0)
                    {
                        rhomis_data[crop_loops] <- switch_units(rhomis_data[crop_loops],
                                                                units = crop_name_conversions$survey_value,
                                                                conversion_factors = crop_name_conversions$conversion)
                    }

                    number_livestock_loops <- find_number_of_loops(rhomis_data,"livestock_name")
                    livestock_loops <- paste0("livestock_name_",1:number_livestock_loops)

                    # Checks if columns are missing, if columns do not exist then they are returned
                    livestock_name_in_data <- check_columns_in_data(rhomis_data, loop_columns = "livestock_name")

                    if(length(livestock_name_in_data)==0)
                    {
                        rhomis_data[livestock_loops] <- switch_units(rhomis_data[livestock_loops],
                                                                     units = livestock_name_conversions$survey_value,
                                                                     conversion_factors = livestock_name_conversions$conversion)
                    }

                    # Make sure "other" units are considered
                    rhomis_data <- replace_units_with_other_all(rhomis_data)

                    if (exists("country_conversions")){

                        indicator_data$iso_country_code <- toupper(switch_units(data_to_convert = rhomis_data$country,
                                                                                units =country_conversions$survey_value,
                                                                                conversion_factors = country_conversions$conversion))
                        if (all(is.na(country_conversions$conversion)) | all(is.na(indicator_data$iso_country_code))){
                            warning(paste0("\nHave not provided the ISO country codes for the survey. \nCheck the country names, and check that they are converted",
                                           "\n---------------------------------------------"))
                        }


                        if ("start_time_user" %in% colnames(rhomis_data)){
                            indicator_data$year <- substr(rhomis_data$start_time_user, start = 1,stop = 4)

                            if (any(!is.na(indicator_data$iso_country_code)) & any(!is.na(indicator_data$year)))
                            {
                                indicator_data<- convert_all_currencies(indicator_data,country_column="iso_country_code", year_column="year")
                            }

                        }

                        missing_columns <- check_columns_in_data(rhomis_data,
                                                                 individual_columns = c("start_time_user","end_time_user"),
                                                                 warning_message = "Could not calculate length of survey")
                        if (length(missing_columns)==0){
                            # Survey

                            indicator_data$survey_length_minutes <- as.POSIXct(rhomis_data[["end_time_user"]])-as.POSIXct(rhomis_data[["start_time_user"]])
                            indicator_data$survey_length_minutes <- as.character(indicator_data$survey_length_minutes)
                        }
                    }


                    #---------------------------------------------------------------
                    # Conduct Calculations
                    #---------------------------------------------------------------

                    ###############
                    # Crop calculations
                    ###############

                    rhomis_data <- crop_calculations_all(rhomis_data,
                                                         crop_yield_units_all = crop_yield_unit_conversions$survey_value,
                                                         crop_yield_unit_conversions_all = crop_yield_unit_conversions$conversion,
                                                         crop_income_units_all = crop_price_unit_conversions$survey_value,
                                                         crop_income_unit_conversions_all = crop_price_unit_conversions$conversion)


                    crop_columns <- c("crop_harvest_kg_per_year",
                                      "crop_consumed_kg_per_year",
                                      "crop_sold_kg_per_year",
                                      "crop_income_per_year",
                                      "crop_price")


                    missing_crop_columns <-check_columns_in_data(rhomis_data,
                                                                 loop_columns=crop_columns)
                    if(length(missing_crop_columns)>=0 & length(missing_crop_columns) < length(crop_columns)){
                        columns_to_widen <- crop_columns[crop_columns %in% missing_crop_columns==F]
                        crop_data <- map_to_wide_format(
                            data = rhomis_data,
                            name_column = "crop_name",
                            column_prefixes = columns_to_widen,
                            types = rep("num", length(columns_to_widen))
                        )

                    }

                    if(length(missing_crop_columns)==length(crop_columns)){
                        crop_data <- NULL
                        warning("No extra outputs generated for livestock loops")
                    }


                    if(outputType=="csv"){
                        write_list_of_df_to_folder(crop_data,"crop_data")
                        dir.create("mean_prices", showWarnings = F)

                        if ("crop_price"%in% names(crop_data))
                        {
                            crop_price <- crop_data[["crop_price"]]
                            crop_price <- crop_price %>% dplyr::mutate_all(replace_infinite) %>%  dplyr::summarise_all(mean, na.rm = TRUE)
                            readr::write_csv(crop_price,"./mean_prices/crop_prices.csv")
                        }
                    }
                    if(outputType=="mongodb"){
                        save_list_of_df_to_db(list_of_df = crop_data,
                                              projectID=project_name,
                                              formID=form_name,
                                              database=database,
                                              url="mongodb://localhost"
                        )
                    }


                    ###############
                    # Livestock calculations
                    ###############

                    rhomis_data <- livestock_calculations_all(rhomis_data,
                                                              # Need to add livestock weights to the conversions sheets
                                                              livestock_weights_names = livestock_weights$animal,
                                                              livestock_weights_conversions = livestock_weights$weight_kg,
                                                              eggs_amount_units_all = eggs_unit_conversion$survey_value,
                                                              eggs_amount_unit_conversions_all = eggs_unit_conversion$conversion,
                                                              eggs_price_time_units_all = eggs_price_unit_conversion$survey_value,
                                                              eggs_price_time_unit_conversions_all = eggs_price_unit_conversion$conversion,
                                                              honey_amount_units_all = honey_unit_conversion$survey_value,
                                                              honey_amount_unit_conversions_all = honey_unit_conversion$conversion,
                                                              milk_amount_units_all = milk_unit_conversion$survey_value,
                                                              milk_amount_unit_conversions_all = milk_unit_conversion$conversion,
                                                              milk_price_time_units_all = milk_price_unit_conversion$survey_value,
                                                              milk_price_time_unit_conversions_all = milk_price_unit_conversion$conversion)


                    livestock_loop_columns <- c(
                        "livestock_sold",
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
                        "eggs_price_per_kg"
                    )

                    missing_livestock_columns <-check_columns_in_data(rhomis_data,
                                                                      loop_columns=livestock_loop_columns,
                                                                      warning_message = "Could not write extra outputs for these columns")

                    if(length(missing_livestock_columns)>=0 & length(missing_livestock_columns) < length(livestock_loop_columns)){
                        columns_to_widen <- livestock_loop_columns[livestock_loop_columns %in% missing_livestock_columns==F]
                        livestock_data <- map_to_wide_format(
                            data = rhomis_data,
                            name_column = "livestock_name",
                            column_prefixes = columns_to_widen,
                            types = rep("num", length(columns_to_widen))
                        )

                    }

                    if(length(missing_livestock_columns)==length(livestock_loop_columns)){
                        livestock_data <- NULL
                        warning("No extra outputs generated for livestock loops")
                    }

                    if(outputType=="csv"){
                        write_list_of_df_to_folder(livestock_data,"livestock_data")
                        dir.create("mean_prices", showWarnings = F)

                        if ("eggs_price_per_kg"%in% names(livestock_data))
                        {

                            egg_price <- livestock_data$eggs_price_per_kg %>% dplyr::mutate_all(replace_infinite) %>% dplyr::summarise_all(mean, na.rm = TRUE)
                            readr::write_csv(egg_price, "./mean_prices/egg_price_per_kg.csv")
                        }

                        if ("meat_price_per_kg"%in% names(livestock_data))
                        {
                            meat_price <- livestock_data$meat_price_per_kg %>% dplyr::mutate_all(replace_infinite) %>% dplyr::summarise_all(mean, na.rm = TRUE)
                            readr::write_csv(meat_price, "./mean_prices/meat_price_per_kg.csv")
                        }


                        if ("milk_price_per_litre"%in% names(livestock_data))
                        {
                            milk_price <- livestock_data$milk_price_per_litre %>% dplyr::mutate_all(replace_infinite) %>% dplyr::summarise_all(mean, na.rm = TRUE)
                            readr::write_csv(milk_price, "./mean_prices/milk_price_per_litre.csv")
                        }

                        if ("livestock_price_per_animal"%in% names(livestock_data))
                        {
                            livestock_price <- livestock_data$livestock_price_per_animal %>% dplyr::mutate_all(replace_infinite) %>% dplyr::summarise_all(mean, na.rm = TRUE)
                            readr::write_csv(livestock_price, "./mean_prices/livestock_price_per_animal.csv")
                        }

                    }

                    if(outputType=="mongodb"){
                        save_list_of_df_to_db(list_of_df = livestock_data,
                                              projectID=project_name,
                                              formID=form_name,
                                              database=database,
                                              url="mongodb://localhost"
                        )


                    }

                    ###############
                    # Demographics
                    ###############

                    if(length(check_columns_in_data(rhomis_data,"hh_pop_rep_num"))==0)
                    {
                        indicator_data$hh_size_members <- calculate_household_size_members(rhomis_data)
                    }
                    if(length(check_columns_in_data(rhomis_data,"hh_pop_rep_num"))==0)
                    {
                        indicator_data$hh_size_MAE <- calculate_MAE(rhomis_data)
                    }

                    if ("household_type"%in%colnames(rhomis_data)==T)
                    {
                        indicator_data$household_type <- rhomis_data[["household_type"]]
                    }
                    if ("household_type"%in%colnames(rhomis_data)==F)
                    {
                        warning('"household_type" does not exist in dataset')
                    }

                    if ("education_level"%in%colnames(rhomis_data)==T)
                    {
                        indicator_data$head_education_level <- rhomis_data[["education_level"]]

                    }
                    if ("education_level"%in%colnames(rhomis_data)==F)
                    {
                        warning('"education_level" does not exist in dataset')
                    }


                    ###############
                    # Land use
                    ###############

                    if (all(c("unitland","landcultivated","landowned")%in%colnames(rhomis_data)))
                    {
                        indicator_data <- dplyr::bind_cols(indicator_data, land_size_calculation(rhomis_data))
                    }

                    ###############
                    # Food security
                    ###############
                    if ("food_worst_month"%in%colnames(rhomis_data)==T)
                    {
                        indicator_data$worst_food_security_month <- rhomis_data[["food_worst_month"]]
                    }
                    if ("food_worst_month"%in%colnames(rhomis_data)==F)
                    {
                        warning('"food_worst_month" does not exist in dataset')
                    }

                    if ("food_best_month"%in%colnames(rhomis_data)==T)
                    {
                        indicator_data$best_food_security_month <- rhomis_data[["food_best_month"]]
                    }
                    if ("food_best_month"%in%colnames(rhomis_data)==F)
                    {
                        warning('"food_best_month" does not exist in dataset')
                    }

                    indicator_data <- dplyr::bind_cols(indicator_data, food_security_calculations(rhomis_data))

                    ###############
                    # Dietary diversity
                    ###############

                    hdds_data <- hdds_calc(rhomis_data)
                    indicator_data <- dplyr::bind_cols(indicator_data,hdds_data)

                    #---------------------------------------------------------------
                    # Totals
                    #---------------------------------------------------------------

                    missing_columns <- check_columns_in_data(rhomis_data,loop_columns = "crop_income_per_year",
                                                             warning_message = "Could not calculate crop income")
                    if(length(missing_columns)==0){
                        indicator_data$crop_income <- total_crop_income(rhomis_data)
                    }


                    indicator_data$livestock_income <- total_livestock_income(rhomis_data)


                    if (!is.null(indicator_data$crop_income) & !is.null(indicator_data$livestock_income) & "offfarm_income_proportion" %in% colnames(rhomis_data)){
                        total_and_off_farm_income <- total_and_off_farm_incomes(rhomis_data,
                                                                                total_crop_income = indicator_data$crop_income,
                                                                                total_livestock_income = indicator_data$livestock_income
                        )
                        indicator_data$total_income <- total_and_off_farm_income$total_income
                        indicator_data$off_farm_income <- total_and_off_farm_income$off_farm_income

                        rhomis_data <- gendered_off_farm_income_split(rhomis_data)
                    }

                    # Off farm incomes

                    off_farm_columns <- c("offfarm_income_name","offfarm_year_round", "offfarm_month", "offfarm_who_control_revenue")

                    missing_off_farm_columns <-check_columns_in_data(rhomis_data,
                                                                     loop_columns=off_farm_columns)
                    if(length(missing_off_farm_columns)>=0 & length(missing_off_farm_columns) < length(off_farm_columns)){
                        columns_to_widen <- off_farm_columns[off_farm_columns %in% missing_off_farm_columns==F]
                        off_farm_data <- map_to_wide_format(
                            data = rhomis_data,
                            name_column = "offfarm_income_name",
                            column_prefixes = columns_to_widen,
                            types = rep("chr", length(columns_to_widen))
                        )

                    }

                    if(length(missing_crop_columns)==length(crop_columns)){
                        off_farm_data <- NULL
                        warning("No extra outputs generated for off-farm loops")
                    }

                    if(outputType=="csv"){
                        write_list_of_df_to_folder(off_farm_data,"off_farm_data")

                    }
                    if(outputType=="mongodb"){
                        save_list_of_df_to_db(list_of_df = off_farm_data,
                                              projectID=project_name,
                                              formID=form_name,
                                              database=database,
                                              url="mongodb://localhost"
                        )


                    }

                    if (outputType=="csv")
                    {
                        dir.create("indicators",showWarnings = F)
                        readr::write_csv(indicator_data,"./indicators/indicators.csv")


                        dir.create("processed_data",showWarnings = F)
                        readr::write_csv(rhomis_data,"./processed_data/processed_data.csv")
                    }

                    if (outputType=="mongodb")
                    {
                        save_data_set_to_db(
                            data = rhomis_data,
                            data_type = "processedData",
                            database = database,
                            url = "mongodb://localhost",
                            projectID = project_name,
                            formID = form_name
                        )

                        save_data_set_to_db(
                            data = indicator_data,
                            data_type = "indicatorData",
                            database = database,
                            url = "mongodb://localhost",
                            projectID = project_name,
                            formID = form_name
                        )

                    }

                    sink(type = "message")

                    # Close the connection.
                    close(warn_connection)

                    # Restore default warning behavior.
                    options(warn = 0)
                    warns <- paste0(warns, collapse="\n")
                    if (outputType=="csv"){
                        dir.create("log", showWarnings = F)
                        write(warns, "log/warnings.log")
                    }

                    if (outputType=="mongodb"){

                        connection <-connect_to_db("projectData",database=database, url=url)




                        query <-paste0('{"projectID":"',project_name,'", "formID":"',form_name,'"}')
                        query <- gsub('\\"','"',query, fixed=T)
                        query <- gsub('"{','{',query, fixed=T)
                        query <- gsub('}"','}',query, fixed=T)
                        query <- gsub('\n','\\n',query, fixed=T)


                        update <-paste0('{"$push":{"log":{"date":"',Sys.time(),'", "message":"',warns,'"}}}')
                        update <- gsub('\\"','"',update, fixed=T)
                        update <- gsub('"{','{',update, fixed=T)
                        update <- gsub('}"','}',update, fixed=T)
                        update <- gsub('\n','\\n',update, fixed=T)

                        options(warn=0)


                        connection$update(
                            query=query,
                            update=update)

                        connection$disconnect()
                    }

                    return(warns)

                }
            },

        error=function(e){
            print(e)
            sink(type = "message")

            # Close the connection.
            close(warn_connection)
            options(warn = 0)

            warns <- paste0(warns, collapse="\n")

            return(warns)

        })
}





#' Generate Data
#'
#' Generate fake data and submit it to a test project
#'
#' @param central_url The URL of the central server holding the data
#' @param central_email The email of the administrative user
#' @param central_password The password of the administrative user
#' @param project_name The name of the project to generate data for
#' @param form_name The name of the form to generate data for
#' @param number_of_responses The number of responses to generate
#' @param form_version The version of the form to upload
#' @param draft Whether or not the form is a draft or finalized
#'
#' @return
#' @export
#'
#' @examples
generateData <- function(central_url,
                         central_email,
                         central_password,
                         project_name,
                         form_name,
                         number_of_responses,
                         form_version,
                         draft=T) {


    # Finding project information from the API
    projects <- get_projects(
        central_url,
        central_email,
        central_password
    )
    projectID <- projects$id[projects$name == project_name]


    # Get central formID
    forms <- get_forms(
        central_url,
        central_email,
        central_password,
        projectID
    )
    formID <- forms$xmlFormId[forms$name == form_name]

    xls_form <- rhomis::get_xls_form(
        central_url = central_url,
        central_email = central_email,
        central_password = central_password,
        projectID = projectID,
        formID = formID,
        # file_destination=form_destination,
        form_version = form_version,
        draft=draft
    )


    # Get number of responses to generate
    for (response_index in 1:number_of_responses)
    {
        mock_response <- rhomis::generate_mock_response(
            survey = xls_form$survey,
            choices = xls_form$choices,
            metadata = xls_form$settings
        )
        mock_response <- gsub(">\n", ">\r\n", mock_response, fixed = T)

        submit_xml_data(
            mock_response,
            central_url,
            central_email,
            central_password,
            projectID = projectID,
            formID = formID,
            draft=draft
        )
    }
    # Delete the xls file
    write("Success in generating responses", stdout())
}


