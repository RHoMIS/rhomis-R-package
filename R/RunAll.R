library(readr)


#' Process central Project and save
#'
#' Process a project from ODK central and save it
#' to a mongoDB database
#'
#' @param central_url The URL of the ODK central server
#' @param central_email The email of for your ODK central account
#' @param central_password The password for the ODK central account
#' @param project_name The name of the project you would like to process
#' @param form_name The name of the form you would like to process
#' @param form_version The verion of the form you would like to process
#' @param database The name of the database you would like to save your data to
#'
#' @return
#' @export
#'
#' @examples
processCentralProject <- function(central_url,
                                  central_email,
                                  central_password,
                                  project_name,
                                  form_name,
                                  form_version,
                                  database) {

    # Finding project information from the API
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

    # Get form and extract metadata
    central_form <- rhomis::get_xls_form(
        central_url = central_url,
        central_email = central_email,
        central_password = central_password,
        projectID = projectID,
        formID = formID,
        # file_destination=form_destination,
        form_version = form_version
    )
    central_form <- central_form$survey
    #### Identifying the modules uses in the survey

    survey_modules <- unique(central_form[["module_name"]])
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

    save_data_set_to_db(
        data = modules_used,
        data_type = "moduleData",
        database = database,
        url = "mongodb://localhost",
        projectID = project_name,
        formID = form_name
    )

    rhomis_data <- get_submission_data(
        central_url,
        central_email,
        central_password,
        projectID,
        formID
    )

    if (sum(colnames(rhomis_data) == "deviceid") > 1) {
        column_to_keep <- which(colnames(rhomis_data) == "deviceid" & colSums(is.na(rhomis_data)) == 0)
        column_to_remove <- which(colnames(rhomis_data) == "deviceid" & colSums(is.na(rhomis_data)) > 0)

        rhomis_data <- rhomis_data[-column_to_remove]
    }

    ## Cleaning Data and Extracting All Units/Column names present in the survey

    #

    rhomis_data <- rhomis_data %>%
        remove_extra_central_columns() %>%
        convert_all_columns_to_lower_case()


    # all_new_values <- extract_units_data_frames(rhomis_data)

    indicator_data <- tibble::as_tibble(list(
        projectName = rep(project_name, nrow(rhomis_data)),
        formName = rep(form_name, nrow(rhomis_data))
    ))

    ## Demographics
    if ("demographics" %in% modules_used$module_name) {
        indicator_data$hh_size_members <- calculate_household_size_members(rhomis_data)
        indicator_data$hh_size_MAE <- calculate_MAE(rhomis_data)

        indicator_data$household_type <- rhomis_data[["household_type"]]
        indicator_data$head_education_level <- rhomis_data[["education_level"]]
    }
    ## Land_use
    if ("land_use" %in% modules_used$module_name) {
        indicator_data <- dplyr::bind_cols(indicator_data, land_size_calculation(rhomis_data))
    }
    ## Livestock Holdings
    # To be changed soon



    # FoodSecMonths
    if ("food_security" %in% modules_used$module_name) {
        indicator_data$worst_food_security_month <- rhomis_data[["food_worst_month"]]
        indicator_data$best_food_security_month <- rhomis_data[["food_best_month"]]
        indicator_data <- dplyr::bind_cols(indicator_data, food_security_calculations(rhomis_data))
    }

    # ppi_score <- ppi_score(rhomis_data, country_code_column = rhomis_data$iso_country_code)

    # HDDS scores
    if ("food_security" %in% modules_used$module_name) {
        hdds_data <- hdds_calc(rhomis_data)
    }
    # Crop Calculations
    if ("crops" %in% modules_used$module_name) {
        rhomis_data <- crop_calculations_all(rhomis_data,
                                             crop_yield_units_all = crop_yield_units$unit,
                                             crop_yield_unit_conversions_all = crop_yield_units$conversion,
                                             crop_income_units_all = crop_price_units$unit,
                                             crop_income_unit_conversions_all = crop_price_units$conversion
        )


        crop_data <- map_to_wide_format(
            data = rhomis_data,
            name_column = "crop_name",
            column_prefixes = c(
                "crop_harvest_kg_per_year",
                "crop_consumed_kg_per_year",
                "crop_sold_kg_per_year",
                "crop_income_per_year",
                "crop_price"
            ),
            types = c("num", "num", "num", "num", "num")
        )

        save_data_set_to_db(
            data = crop_data$crop_harvest_kg_per_year,
            data_type = "cropData",
            database = database,
            url = "mongodb://localhost",
            projectID = project_name,
            formID = form_name
        )
    }

    # Livestock Calculaions
    if ("livestock" %in% modules_used$module_name) {
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
                                                  milk_price_time_unit_conversions_all = milk_price_time_units$conversion_factor
        )


        livestock_data <- map_to_wide_format(
            data = rhomis_data,
            name_column = "livestock_name",
            column_prefixes = c(
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
            ),
            types = c("num", "num", "num", "num", "num", "num", "num", "num", "num", "num", "num", "num", "num", "num", "num", "num", "num", "num")
        )


        livestock_sold <- map_to_wide_format(rhomis_data, "livestock_name", "livestock_sold", types = "num")
        save_data_set_to_db(
            data = livestock_sold$livestock_sold,
            data_type = "livestockData",
            database = database,
            url = "mongodb://localhost",
            projectID = project_name,
            formID = form_name
        )
    }
    # Totals
    if ("livestock" %in% modules_used$module_name & "crops" %in% modules_used$module_name) {
        indicator_data$crop_income <- total_crop_income(rhomis_data)
        indicator_data$livestock_income <- total_livestock_income(rhomis_data)
    }

    if (!is.null(indicator_data$crop_income) & !is.null(indicator_data$livestock_income) & ("off_farm_income" %in% modules_used$module_name | "off_farm_incomes" %in% modules_used$module_name)) {
        total_and_off_farm_income <- total_and_off_farm_incomes(rhomis_data,
                                                                total_crop_income = indicator_data$crop_income,
                                                                total_livestock_income = indicator_data$livestock_income
        )
        indicator_data$total_income <- total_and_off_farm_income$total_income
        indicator_data$off_farm_income <- total_and_off_farm_income$off_farm_income

        rhomis_data <- gendered_off_farm_income_split(rhomis_data)
    }

    if ("off_farm_income" %in% modules_used$module_name | "off_farm_incomes" %in% modules_used$module_name) {
        off_farm_prefixes <- c("offfarm_year_round", "offfarm_month", "offfarm_who_control_revenue")
        data_types <- c("chr", "chr", "chr")

        off_farm_data <- map_to_wide_format(
            data = rhomis_data,
            name_column = "offfarm_income_name",
            column_prefixes = off_farm_prefixes,
            types = data_types
        )
    }

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

    # Finishing whole process
    write("Success from Rscript", stdout())
}


#' Process RHoMIS data
#'
#' A main function that can be used to process rhomis
#' data. Whether the dataset comes from a local csv
#' or from ODK central
#'
#' @param dataSource The type of RHoMIS data being fed into the
#' calculations, whether a local csv file or data from ODK central.
#' Options "local" or "central".
#' @param outputType  The type of output to produce (options are "csv" or "mongodb")
#' @param coreOnly Indicating whether to only analyze the RHoMIS core information (TRUE/FALSE)
#' If TRUE, extra variables will be saved but they will not be processed into indicators.
#' If FALSE, you must either provide the survey xls file so that the modules used can be identified,
#' or ensure that the project you are accessing can be found on ODK central.
#' @param surveyFile The path to the surveyxls file. Only necessary if "coreOnly"=FALSE and dataSource="local".
#' @param extractUnits Whether or not to only extract units (TRUE/FALSE)
#' @param calculatePrices Whether or not to only calculate the new prices (TRUE/FALSE)
#' @param processData Whether to process the whole dataset (TRUE/FALSE)
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
    calculatePrices=F,
    processData=F,
    dataFilePath=NULL,
    central_url=NULL,
    central_email=NULL,
    central_password=NULL,
    project_name=NULL,
    form_name=NULL,
    form_version=NULL,
    database=NULL){



    dataSource <- "local"
    dataFilePath <- "./inst/extdata/projects/VN_TST_2020/data/raw_data.csv"
    outputType <- "csv"
    coreOnly=T
    surveyFile=NULL
    moduleSaving=F
    extractUnits=F
    calculatePrices=T
    processData=F
    central_url=NULL
    central_email=NULL
    central_password=NULL
    project_name=NULL
    form_name=NULL
    form_version=NULL
    database=NULL
    #---------------------------------------------------------------
    # Ensuring the parameter calls are logically consistent
    #---------------------------------------------------------------
    if(dataSource!="local"&dataSource!="central"){
        stop("Raw must come from ODK central or a local source (csv)")
    }
    if(dataSource=="local"){
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
                              "dbURL")
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

    if(dataSource=="local")
    {
        rhomis_data <- readr::read_csv(dataFilePath, col_types = cols(), na = c("n/a","-999","NA"))
        colnames(rhomis_data) <- tolower(clean_column_names(colnames(rhomis_data),
                                                            seperator = "/",
                                                            repeat_columns = c("crop_repeat",
                                                                               "livestock_repeat",
                                                                               "offfarm_repeat",
                                                                               "hh_pop_repeat",
                                                                               "hh_rep")))

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
            formID
        )
    }


    #---------------------------------------------------------------
    # Loading Form Data
    #---------------------------------------------------------------
    if(dataSource=="local")
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
            form_version = form_version
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
            save_multiple_conversions(
                database,
                url,
                projectID,
                formID,
                units_and_conversions,
                names(units_and_conversions)
            )
        }
        return()

    }
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
        load_local_units()



    }


    if (outputType=="mongodb")
    {

        # Not yet complete
        load_db_units()
    }






    #---------------------------------------------------------------
    # Swap "Other" with Standard values
    #---------------------------------------------------------------

    # Replacing crop and livestock names with their "other"
    rhomis_data <- replace_crop_and_livestock_other(rhomis_data)

    number_crop_loops <- find_number_of_loops(rhomis_data,"crop_name")
    crop_loops <- paste0("crop_name_",1:number_crop_loops)
    rhomis_data[crop_loops] <- switch_units(rhomis_data[crop_loops],
                                units = crop_name_conversions$survey_value,
                                conversion_factors = crop_name_conversions$conversion)

    number_livestock_loops <- find_number_of_loops(rhomis_data,"livestock_name")
    livestock_loops <- paste0("livestock_name_",1:number_livestock_loops)
    rhomis_data[livestock_loops] <- switch_units(rhomis_data[livestock_loops],
                                units = livestock_name_conversions$survey_value,
                                conversion_factors = livestock_name_conversions$conversion)

    # Make sure "other" units are considered
    rhomis_data <- replace_units_with_other_all(rhomis_data)







    #---------------------------------------------------------------
    # Conduct Calculations
    #---------------------------------------------------------------



    rhomis_data <- crop_calculations_all(rhomis_data,
                                         crop_yield_units_all = crop_yield_unit_conversions$survey_value,
                                         crop_yield_unit_conversions_all = crop_yield_unit_conversions$conversion,
                                         crop_income_units_all = crop_price_unit_conversions$survey_value,
                                         crop_income_unit_conversions_all = crop_price_unit_conversions$conversion)

    # rhomis_data$crop_price_1
    crop_price_data <- map_to_wide_format(
        data = rhomis_data,
        name_column = "crop_name",
        column_prefixes = c(
            "crop_price"
        ),
        types = c("num")
    )


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



    #---------------------------------------------------------------
    # Save outputs
    #---------------------------------------------------------------





}



