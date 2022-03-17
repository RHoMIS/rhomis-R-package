


load_data_odk <- function(
        url,
        central_url,
        central_email){

}


#' Make ID Columns
#'
#' Make ID columns for form, project, and household
#'
#' @param data The rhomis data set as a tibble
#' @param country_column The name of the column containing the country string, as collected in the survey
#' @param id_type The type of ID you would like to enter for projects and forms. If you select "string", then fill in the proj_id and form_id arguments, with the project id and form id you would like to use. If selecting "column", enter the name of the column (proj_id) containing the project ID you would like to use, and the name of the column (form_id) containing the form ids you would like to use.
#' @param proj_id Either a single string to be used as the project ID for all households, or the name of the column containing the project IDs (depending on id_type)
#' @param form_id Either a single string to be used as the form ID for all households, or the name of the column containing the form IDs (depending on id_type)
#' @param unique_id_col The name of the column containing unique id record. This is produced by the server accepting ODK records
#' @param hh_id_col The household ID column
#'
#' @return
#' @export
#'
#' @examples
make_id_columns <- function(data,
                            country_column= "country",
                            unique_id_col="_uuid",
                            hh_id_col=NULL,
                            id_type=c("string","column"),  # list of allowed values for argument, default is first element in vector
                            proj_id,
                            form_id){



    #' Check validity of argument and print error if unknown type is supplied
    id_type <- match.arg(id_type)

    #' Check whether id columns in list below exist in loaded rhomis data
    id_columns <- c(country_column, hh_id_col, unique_id_col)

    #' loop over column names
    for (cname in id_columns){
        #' if column is not found in dataset throw an error
        if ( !(cname %in% colnames(data)) ){
            stop(paste('Expected column',cname,'does not exist in the input dataset.'))
        }
    }

    #' make sure that the unique_id_col does indeed contain unique values
    if (any(duplicated(data[unique_id_col]))){
        stop('The unique_id_col you provided exists in the data, but contains duplicate entries')
    }

    #' if form and proj ids are provided as strings, create new columns filled with these string values
    if (id_type=="string"){

        data$id_proj <- rep(proj_id, nrow(data))
        data$id_form <- rep(form_id, nrow(data))


    } else {

        #' loop over proj and form id arguments
        for (cname in c(proj_id, form_id)){

            #' confirm that these columns exist in the dataset, otherwise bail and print error
            if ( !(cname %in% colnames(data)) ){
                stop(paste0("Expected id column",cname,"does not exist in the dataset provided"))
            }
        }

        #' copy the contents of these columns into id_proj and id_form columns
        data$id_proj <- data[[proj_id]]
        data$id_form <- data[[form_id]]
    }

    #' create a unique project, form, country, ID column
    proj_form_id_col <- paste0(data[["id_proj"]],data[["id_form"]],data[[country_column]])
    proj_form_id_col <- unname(sapply(proj_form_id_col, function(x) digest::digest(x)))

    #' add new column to dataset
    data$id_rhomis_dataset <- proj_form_id_col

    #' create unique household id
    if (is.null(hh_id_col))
    {
        household_id <- paste0(data[["id_proj"]], data[["id_form"]], c(1:nrow(data)))
        household_id <- unname(sapply(household_id, function(x) digest::digest(x)))

    } else {

        household_id <- unname(sapply(data[[hh_id_col]], function(x) digest::digest(x)))
    }

    #' add household and unique id columns to dataset
    data$id_hh <- household_id
    data$id_unique <- data[[unique_id_col]]

    #' shift column ordering so that id columns are the left-most columns
    for (i in c("id_proj", "id_form", "id_rhomis_dataset", "id_hh", "id_unique")){
        data <- data %>% dplyr::relocate(i)
    }

    return (data)


}

make_string_id_columns <- function(){


}


#' Load RHoMIS CSV
#'
#' Load a Raw RHoMIS csv file, collected using ODK, and
#' convert the column names into a shortened, standardised
#' version.
#'
#' @param file_path The filepath of the RHoMIS csv
#' @param country_column The name of the column containing the country
#' @param id_type Indicator of whether you are providing a single ID
#' @param proj_id Either a single string to be used as the project ID for all households, or the name of the column containing the project IDs (depending on id_type)
#' @param form_id Either a single string to be used as the form ID for all households, or the name of the column containing the form IDs (depending on id_type)
#' @param hh_id_col The household ID column
#' @param overwrite True if you would like to overwrite previous ID column, false if would not like to overwrite existing IDs
#' @param unique_id_col
#' @param hh_id_col
#' @param repeat_column_names The types of repeat column name
#'
#' @return A tibble of RHoMIS data
#' @export
#'
#' @examples
#'
load_rhomis_csv <- function(file_path,
                            country_column= "country",
                            unique_id_col="_uuid",
                            hh_id_col=NULL,
                            id_type=c("string","column"),  # list of allowed values for argument, default is first element in vector
                            proj_id=NULL,
                            form_id=NULL,
                            overwrite=FALSE) {


    # read in the input csv file
    rhomis_data <- readr::read_csv(file_path, col_types = readr::cols(), na = c("n/a","-999","NA"))

    # simplify column names to more readable format
    colnames(rhomis_data) <- clean_column_names(colnames(rhomis_data), pkg.env$repeat_columns)

    # ensure all data entries are lower case for consistency / easier data analysis
    rhomis_data<- convert_all_columns_to_lower_case(rhomis_data)

    # temp manual intervention to account for non-standard/missing column fields
    rhomis_data <- make_id_columns(
        data=rhomis_data,
        country_column,
        unique_id_col=unique_id_col,
        hh_id_col=hh_id_col,
        id_type=id_type,
        proj_id=proj_id,
        form_id=form_id)

    return(rhomis_data)

}

#' Extract New Values
#'
#' Extract all of the new values from a RHoMIS data frame,
#' if they have unit conversions in the package, then convert them.
#'
#' @param data A RHoMIS tibble
#'
#' @return
#' @export
#'
#' @examples
extract_all_new_values <- function(data){
    units_and_conversions <- extract_units_data_frames(rhomis_data)
    units_and_conversions <- check_existing_conversions(units_and_conversions)

    return(units_and_conversions)
}


#' Replace Infinite
#'
#' Replace infinite values with NA in a specific column
#'
#' @param column The column where infinite values need to be replaced
#'
#' @return
#' @export
#'
#' @examples
replace_infinite <- function(column){
    column[is.infinite(column)] <- NA
    return(column)
}




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
#' @param extractUnitsOnly Whether or not to only extract units (TRUE/FALSE)
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
#' @param proj_id An ID for your project
#' @param form_id An ID for your form
#'
#' @return
#' @export
#'
#' @examples
processData <- function(
        proj_id,
        form_id,
        dataSource=c("csv", "central"), # list of allowed values for argument, default is first element in vector (csv),
        outputType=c("csv", "mongodb"), # list of allowed values for argument, default is first element in vector (csv),
        coreOnly=T,
        surveyFile=NULL,
        moduleSaving=F,
        extractUnitsOnly=T,
        dataFilePath=NULL,
        central_url=NULL,
        central_email=NULL,
        central_password=NULL,
        project_name=NULL,
        form_name=NULL,
        form_version=NULL,
        database=NULL,
        draft=NULL
        ){


    # Check validity of OutputTypes and print error if unknown OutputType is supplied
    outputType <- match.arg(outputType)
    dataSource <- match.arg(dataSource)




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


    #---------------------------------------------------------------
    # Loading Submission Data
    #---------------------------------------------------------------

    if(dataSource=="csv")
    {

        rhomis_data <- load_rhomis_csv(
            file_path=dataFilePath,
            country_column= "country",
            unique_id_col="_uuid",
            hh_id_col=NULL,
            id_type="string",
            proj_id=proj_id,
            form_id=form_id
        )



        indicator_data <- make_new_dataset(rhomis_data)




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





        colnames(rhomis_data) <- clean_column_names(colnames(rhomis_data), pkg.env$repeat_columns)
        rhomis_data <- rhomis_data %>%
            remove_extra_central_columns()



        rhomis_data<- convert_all_columns_to_lower_case(rhomis_data)

        rhomis_data <- sapply(rhomis_data, function(x){
            x[as.numeric(x)==-999]<-NA
            x
        }, simplify = F) %>% tibble::as_tibble()


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

    if(extractUnitsOnly==T)
    {
        units_and_conversions <- extract_values_by_project(rhomis_data)
        units_and_conversions <- check_existing_conversions(list_of_df = units_and_conversions)

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
    if (!extractUnitsOnly)
    {
        #---------------------------------------------------------------
        # Load Conversions
        #---------------------------------------------------------------

        if (outputType=="csv"){
            if (!dir.exists("./unit_conversions"))
            {
                stop('Specified that the units were stored locally but the path "unit_conversions" does not exist')
            }

            #---------------------------------------------
            # Loading all of the unit conversions locally
            #---------------------------------------------
            load_local_units("./unit_conversions/", id_rhomis_dataset = rhomis_data[["id_rhomis_dataset"]])

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
                                                    unit_tibble = crop_name_conversions,
                                                    id_vector = rhomis_data[["id_rhomis_dataset"]]
            )
        }

        number_livestock_loops <- find_number_of_loops(rhomis_data,"livestock_name")
        livestock_loops <- paste0("livestock_name_",1:number_livestock_loops)

        # Checks if columns are missing, if columns do not exist then they are returned
        livestock_name_in_data <- check_columns_in_data(rhomis_data, loop_columns = "livestock_name")

        if(length(livestock_name_in_data)==0)
        {
            rhomis_data[livestock_loops] <- switch_units(rhomis_data[livestock_loops],
                                                         unit_tibble = livestock_name_conversions,
                                                         id_vector = rhomis_data[["id_rhomis_dataset"]]
            )
        }

        # Make sure "other" units are considered
        rhomis_data <- replace_units_with_other_all(rhomis_data)

        if (exists("country_conversions")){

            indicator_data$iso_country_code <- toupper(switch_units(data_to_convert = rhomis_data$country,
                                                                    unit_tibble = country_conversions,
                                                                    id_vector = rhomis_data[["id_rhomis_dataset"]]))
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
                                             crop_yield_units_conv_tibble = crop_yield_unit_conversions,
                                             crop_income_units_conv_tibble = crop_price_unit_conversions)


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
                                                  livestock_weights_conv_tibble = make_per_project_conversion_tibble(rhomis_data[["id_rhomis_dataset"]],unit_conv_tibble =livestock_weights ),
                                                  eggs_amount_unit_conv_tibble = eggs_unit_conversion,
                                                  eggs_price_time_units_conv_tibble = eggs_price_unit_conversion,
                                                  honey_amount_unit_conv_tibble = honey_unit_conversion,
                                                  milk_amount_unit_conv_tibble = milk_unit_conversion,
                                                  milk_price_time_unit_conv_tibble = milk_price_unit_conversion,)


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
            indicator_data <- dplyr::bind_cols(indicator_data, land_size_calculation(rhomis_data, unit_conv_tibble = land_unit_conversion))
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

            rhomis_data <- gendered_off_farm_income_split(rhomis_data,gender_categories = pkg.env$gender_categories)
        }

        # Off farm incomes

        off_farm_columns <- c("offfarm_income_name","offfarm_year_round", "offfarm_month", "offfarm_who_control_revenue")

        missing_off_farm_columns <-check_columns_in_data(rhomis_data,
                                                         loop_columns=off_farm_columns)
        if(length(missing_off_farm_columns)>=0 & length(missing_off_farm_columns) < length(off_farm_columns) & "offfarm_income_name" %in% missing_off_farm_columns==F){
            columns_to_widen <- off_farm_columns[off_farm_columns %in% missing_off_farm_columns==F]
            off_farm_data <- map_to_wide_format(
                data = rhomis_data,
                name_column = "offfarm_income_name",
                column_prefixes = columns_to_widen,
                types = rep("chr", length(columns_to_widen))
            )

        }

        if(length(missing_off_farm_columns)==length(missing_off_farm_columns)){
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

        return(rhomis_data)



    }
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


