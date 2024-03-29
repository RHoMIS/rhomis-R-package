#' Make ID Columns
#'
#' Make ID columns for form, project, and household
#'
#' Rpackage file: RunAll.R
#'
#' @param data The rhomis data set as a tibble
#' @param country_column The name of the column containing the country string, as collected in the survey
#' @param id_type The type of ID you would like to enter for projects and forms. If you select "string", then fill in the proj_id and form_id arguments, with the project id and form id you would like to use. If selecting "column", enter the name of the column (proj_id) containing the project ID you would like to use, and the name of the column (form_id) containing the form ids you would like to use.
#' @param proj_id Either a single string to be used as the project ID for all households, or the name of the column containing the project IDs (depending on id_type)
#' @param form_id Either a single string to be used as the form ID for all households, or the name of the column containing the form IDs (depending on id_type)
#' @param unique_id_col The name of the column containing unique id record. This is produced by the server accepting ODK records
#' @param hh_id_col The household ID column
#' @param overwrite Boolean indicating whether or not to overwrite existing household id columns.
#'
#' @return
#' @export
#'
#' @examples
make_id_columns <- function(data,
                            country_column = "country",
                            unique_id_col = "_uuid",
                            hh_id_col = NULL,
                            id_type = c("string", "column"), # list of allowed values for argument, default is first element in vector
                            proj_id,
                            form_id,
                            overwrite=F) {

    # Check validity of argument and print error if unknown type is supplied
    id_type <- match.arg(id_type)

    # Check whether id columns in list below exist in loaded rhomis data
    id_columns <- c(country_column, hh_id_col, unique_id_col)

    # Issues with country column
    if (country_column %in% colnames(data)==F){
        warning(paste("Expected column '", country_column, "' does not exist in the input dataset. This will mean units will not be convertable per-country"))
        data[[country_column]] <- NA
    }


    # Issues with hhid column
    if (!is.null(hh_id_col)){
        if (hh_id_col %in% colnames(data)==F){
            warning(paste("Expected column '", hh_id_col, "' does not exist in the input dataset. This will mean we cannot identify individual households"))
            data[[hh_id_col]] <- NA
        }
    }


    # Issues with proj id
    if (id_type=="column"){
        if (proj_id %in% colnames(data)==F){
            warning(paste("Expected column '", proj_id, "' does not exist in the input dataset. Will use a single project id for this dataset"))
            data[["id_proj"]] <- NA
        }else{
            data[["id_proj"]] <- data[[proj_id]]
        }
    }
    # Issues with form id
    if (id_type=="column"){
        if (form_id %in% colnames(data)==F){
            warning(paste("Expected column '", form_id, "' does not exist in the input dataset. Will use a single project id for this dataset"))
            data[["id_form"]] <- NA

        }else{
            data[["id_form"]] <- data[[form_id]]
        }
    }

    if (id_type == "string") {
        data$id_proj <- rep(proj_id, nrow(data))
        data$id_form <- rep(form_id, nrow(data))
    }





    # make sure that the unique_id_col does indeed contain unique values
    # Issues with unique id column
    if (unique_id_col %in% colnames(data)==F){
        warning(paste("Expected column '", unique_id_col, "' does not exist in the input dataset. We will use a combination of form id and row number"))
        if("id_form" %in% colnames(data)){
            data[[unique_id_col]] <- paste0(data$id_form, row.names(data))

        }else{
            data[[unique_id_col]] <- NA
        }

    }




    # create a unique project, form, country, ID column
    proj_form_id_col <- paste0(data[["id_proj"]], data[["id_form"]], data[[country_column]])
    proj_form_id_col <- unname(sapply(proj_form_id_col, function(x) digest::digest(x)))

    # add new column to dataset
    data$id_rhomis_dataset <- proj_form_id_col

    # create unique household id
    if (overwrite==T | is.null(hh_id_col)){
    if (is.null(hh_id_col)) {
        household_id <- paste0(data[["id_proj"]], data[["id_form"]], c(1:nrow(data)))
        household_id <- unname(sapply(household_id, function(x) digest::digest(x)))
    } else {
        household_id <- unname(sapply(data[[hh_id_col]], function(x) digest::digest(x)))
    }
    }

    if (overwrite==F){
        if (!is.null(hh_id_col) )
        {
            if (hh_id_col %in% colnames(data)){
                household_id <- data[[hh_id_col]]
            }

        }

    }

    # add household and unique id columns to dataset
    data$id_hh <- household_id
    data$id_unique <- data[[unique_id_col]]

    # shift column ordering so that id columns are the left-most columns
    for (i in c("id_proj", "id_form", "id_rhomis_dataset", "id_hh", "id_unique")) {
        data <- data %>% dplyr::relocate(all_of(i))
    }
    return(data)
}


#' Load RHoMIS CSV
#'
#' Load a Raw RHoMIS csv file, collected using ODK, and
#' convert the column names into a shortened, standardised
#' version.
#'
#' Rpackage file: RunAll.R
#'
#' @param file_path The filepath of the RHoMIS csv
#' @param country_column The name of the column containing the country
#' @param id_type Indicator of whether you are providing a single ID
#' @param proj_id Either a single string to be used as the project ID for all households, or the name of the column containing the project IDs (depending on id_type)
#' @param form_id Either a single string to be used as the form ID for all households, or the name of the column containing the form IDs (depending on id_type)
#' @param hh_id_col The household ID column
#' @param overwrite True if you would like to overwrite previous ID column, false if would not like to overwrite existing IDs
#' @param unique_id_col The column in the dataset which contains unique IDs (usually _uuid)
#' @param hh_id_col The column containing household IDs
#'
#' @return A tibble of RHoMIS data
#' @export
#'
#' @examples
load_rhomis_csv <- function(file_path,
                            country_column = "country",
                            unique_id_col = "_uuid",
                            hh_id_col = NULL,
                            id_type = c("string", "column"), # list of allowed values for argument, default is first element in vector
                            proj_id = NULL,
                            form_id = NULL,
                            overwrite = FALSE
)
{



    # read in the input csv file
    rhomis_data <- readr::read_csv(file_path, col_types = readr::cols(),
                                   na = c("na","n/a", "-999", "-99", "NA"),
                                   locale = readr::locale(encoding = "UTF8"),
                                   show_col_types=F)

    # simplify column names to more readable format
    newcolnames <-  tolower(clean_column_names(colnames(rhomis_data)))
    colnames(rhomis_data) <- newcolnames

    # ensure all data entries are lower case for consistency / easier data analysis
    # duplicated_column_names <- colnames(rhomis_data)[duplicated(tolower(colnames(rhomis_data)))]
    # duplicated_indices <- which(duplicated(tolower(colnames(rhomis_data))))

    # if (length(duplicated_column_names)>0){
    #    warning(paste0('Column "',duplicated_column_names, '" is duplicated. Column had to be removed for calculations to proceed\n '))
    #    rhomis_data <- rhomis_data[-duplicated_indices]
    # }

    rhomis_data <- convert_all_columns_to_lower_case(rhomis_data)

    if (is.null(hh_id_col)){
        if ("hhid" %in% colnames(rhomis_data)){
            hh_id_col <- "hhid"
        }
        if ("householdid" %in% colnames(rhomis_data)){
            hh_id_col <- "householdid"
        }

    }

    # temp manual intervention to account for non-standard/missing column fields
    rhomis_data <- make_id_columns(
        data = rhomis_data,
        country_column,
        unique_id_col = unique_id_col,
        hh_id_col = hh_id_col,
        id_type = id_type,
        proj_id = proj_id,
        form_id = form_id,
        overwrite = overwrite
    )

    # Checks whether NTFP columns exist, and does some preprocessing to
    # Reformat data
    rhomis_data <- ntfp_preprocessing(rhomis_data)

    return(rhomis_data)
}

#' Extract New Values
#'
#' Extract all of the new values from a RHoMIS data frame,
#' if they have unit conversions in the package, then convert them.
#'
#' Rpackage file: RunAll.R
#'
#' @param data A RHoMIS tibble
#'
#' @return
#' @export
#'
#' @examples
extract_all_new_values <- function(data) {
    units_and_conversions <- extract_units_data_frames(rhomis_data)
    units_and_conversions <- check_existing_conversions(units_and_conversions)

    return(units_and_conversions)
}


#' Replace Infinite
#'
#' Replace infinite values with NA in a specific column
#'
#' Rpackage file: RunAll.R
#'
#' @param column The column where infinite values need to be replaced
#'
#' @return
#' @export
#'
#' @examples
replace_infinite <- function(column) {
    column[is.infinite(column)] <- NA
    return(column)
}




#' Process RHoMIS data
#'
#' A main function that can be used to process rhomis
#' data. Whether the dataset comes from a local csv
#' or from ODK central.
#'
#' RHoMIS datasets go through 4 stages of processing:
#'
#' 1. Initial Cleaning and Extracting Units
#' 2. Calculation of initial indicators
#' 3. Calculation of final indicators, including food availability,
#' gender, and value of products consumed
#'
#' Rpackage file: RunAll.R
#'
#' @param extractUnitsOnly Whether or not to only extract units (TRUE/FALSE)
#' @param calculateInitialIndicatorsOnly Whether or not to only calculate
#' initial indicators (TRUE/FALSE)
#' @param calculateFinalIndicatorsOnly Whether or not to only calculate
#' final indicators
#' @param dataSource The type of RHoMIS data being fed into the
#' calculations, whether a local csv file or data from ODK central.
#' Options "csv" or "central".
#' @param base_path The path where all of the data processing should take place
#' @param outputType  The type of output to produce (options are "csv"
#' or "mongodb")
#' @param dataFilePath The file to the data (csv format).
#' ONLY RELEVANT IF "dataSource" WAS "local".
#' @param id_type The type of ID you would like to use ("string" or "column")
#' @param proj_id An ID for your project
#' @param form_id An ID for your form
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
#' @param central_test_case This flag is used for running a test-sample dataset from ODK the inst/sample_central_project/ folder
#' @param database The name of the database you would like to save results to
#' @param isDraft Whether or not the ODK form you are working with is a draft
#' or a final version. Only relevant if you are processing a project from ODK central
#' @param uuid_local The column in a local dataset containing uuids (usually _uuid)
#' @param gender_categories The gender categories present in the data which is to be processed
#' @return
#' @export
#'
#' @examples
processData <- function( # Arguments to indicate the stage of analysis
    extractUnitsOnly = T, # The stage of data processing
    calculateInitialIndicatorsOnly = F,
    calculateFinalIndicatorsOnly = F,
    # Arguments to indicate the type of processing being done (local or on server)
    dataSource = c("csv", "central"), # list of allowed values for argument, default is first element in vector (csv),
    outputType = c("csv", "mongodb"), # list of allowed values for argument, default is first element in vector (csv),
    # Arguments used for processing local data sets
    base_path = "./", # The path to the folder where outputs will be written
    dataFilePath = NULL,
    id_type = c("string", "column"),
    proj_id,
    form_id,
    uuid_local = pkg.env$identification_column_list$uuid_local,
    # Arguments for if processing from ODK central
    central_url = NULL,
    central_email = NULL,
    central_password = NULL,
    project_name = NULL,
    form_name = NULL,
    database = NULL,
    isDraft = NULL,
    central_test_case = FALSE,
    gender_categories = pkg.env$gender_categories) {


    #----------------------------------------
    # Checking the validity of the Arguments
    #----------------------------------------



    # Check validity of OutputTypes and print error if unknown OutputType is supplied
    outputType <- match.arg(outputType)
    dataSource <- match.arg(dataSource)

    #---------------------------------------------------------------
    # Loading Submission Data
    #---------------------------------------------------------------

    # If local csv specified, load the csv
    # add some identification columns and
    # clean the column names
    if (dataSource == "csv" & !calculateFinalIndicatorsOnly) {

        # If the user specified a csv, then they must provide a file path
        # for the dataset they are loading
        if(is.null(dataFilePath)){
            stop('You specified the data was coming from a local csv but have not specified a "dataFilePath"')
        }

        rhomis_data <- load_rhomis_csv(
            file_path = dataFilePath,
            country_column = pkg.env$identification_column_list$country,
            unique_id_col = uuid_local,
            hh_id_col = NULL,
            id_type = id_type,
            proj_id = proj_id,
            form_id = form_id,
        )
    }

    # If central dataset specified,
    # identify the relevant projects,
    # and load the zip files.
    # add some identification columns and
    # clean the column names
    if(dataSource == "central")
    {

        rhomis_data <- load_rhomis_central(
            central_url=central_url,
            central_email=central_email,
            central_password=central_password,
            project_name=project_name,
            form_name=form_name,
            database=database,
            isDraft=isDraft,
            central_test_case=central_test_case
        )
    }



    # Make an empty indicator dataset with
    # matching ID columns
    if(calculateFinalIndicatorsOnly==F){

        indicator_data <- make_new_dataset(rhomis_data)
    }

    #---------------------------------------------------------------
    # Extract and write units
    #---------------------------------------------------------------

    if (extractUnitsOnly) {

        # Extract the new units, and replace them with units
        # which are stored in the package where possible.
        units_and_conversions <- extract_values_by_project(rhomis_data)
        units_and_conversions <- check_existing_conversions(list_of_df = units_and_conversions)

        if (outputType == "csv") {
            units_folder_dest <- paste0(base_path, ".original_units")
            write_units_to_folder(
                list_of_df = units_and_conversions,
                folder = units_folder_dest
            )

            new_units_dest <- paste0(base_path, "units_and_conversions")


            write_units_to_folder(
                list_of_df = units_and_conversions,
                folder = new_units_dest
            )

            return(units_and_conversions)
        }

        if (outputType == "mongodb") {
            save_multiple_conversions(
                database = database,
                url = url,
                projectID = project_name,
                formID = form_name,
                conversion_data = units_and_conversions,
                conversion_types = names(units_and_conversions),
                collection = "unmodified_units"
            )

            save_multiple_conversions(
                database = database,
                url = url,
                projectID = project_name,
                formID = form_name,
                conversion_data = units_and_conversions,
                conversion_types = names(units_and_conversions),
                collection = "units_and_conversions",
                converted_values=T

            )
            set_project_tag_to_true(database = database,
                                    url = url,
                                    projectID=project_name,
                                    formID=form_name,
                                    project_tag="unitsExtracted")
        }
    } else {






        if (calculateInitialIndicatorsOnly == T) {

            #---------------------------------------------------------------
            # Load Conversions
            #---------------------------------------------------------------
            if (outputType == "csv") {
                units_folder <- paste0(base_path, "units_and_conversions/")

                if (!dir.exists(units_folder))
                {
                    stop('Specified that the units were stored locally but the path ',units_folder,' does not exist')
                }

                #---------------------------------------------
                # Loading all of the unit conversions locally
                #---------------------------------------------
                units <- load_local_units(units_folder, id_rhomis_dataset = rhomis_data[["id_rhomis_dataset"]])
            }
            if (outputType == "mongodb") {
                unit_list <- find_db_units(
                    projectID = project_name,
                    formID = form_name,
                    url = "mongodb://localhost",
                    collection = "projectData",
                    database = database
                )
                units <- load_all_db_units(unit_list,
                                           projectID = project_name,
                                           formID = form_name,
                                           database = database,
                                           id_rhomis_dataset = rhomis_data[["id_rhomis_dataset"]]
                )
            }

            # Run all of the preliminary calculations that can
            # be done without price verification and without
            # verification of calory values
            # This function can be found in the
            # R/redirectModules.R file.
            # From this function we receive a list of
            # data frames. These include processed_data,
            # indicator_data, and extra_outputs
            results <- run_preliminary_calculations(
                rhomis_data = rhomis_data,
                gender_categories = gender_categories,
                units = units
            )

            # If the desired output format is csv,
            # Write the processed data, indicator
            # data and extra outputs
            # will be written to the relevant files
            #
            #
            #-------------------------------------------------------------
            # NEED TO SIMPLIFY
            # Below we write all of the data
            # which is processed during the initial calculations
            #-------------------------------------------------------------

            lapply(names(results), function(x) {
                data_to_write <- results[[x]]
                if(length(data_to_write)==0){
                    return()
                }
                if (outputType == "csv") {
                    new_folder <- paste0(base_path, x)
                    if (x == "original_prices") {
                        return()
                    }
                    dir.create(new_folder, showWarnings = F)

                    if (x == "processed_data" | x == "indicator_data") {
                        path <- paste0(new_folder, "/", x, ".csv")
                        readr::write_csv(data_to_write, path)
                        return()
                    }

                    write_list_of_df_to_folder(list_of_df = data_to_write, folder = new_folder)
                }

                if (outputType == "mongodb") {
                    if (x == "processed_data") {
                        save_data_set_to_db(
                            data = data_to_write,
                            data_type = "processedData",
                            database = database,
                            url = "mongodb://localhost",
                            projectID = project_name,
                            formID = form_name
                        )
                        return()
                    }
                    if (x == "indicator_data") {
                        save_data_set_to_db(
                            data = data_to_write,
                            data_type = "indicatorData",
                            database = database,
                            url = "mongodb://localhost",
                            projectID = project_name,
                            formID = form_name
                        )
                        return()
                    }

                    if (x == "original_prices") {
                        save_multiple_conversions(
                            database = database,
                            url = "mongodb://localhost",
                            projectID = project_name,
                            formID = form_name,
                            conversion_data = data_to_write,
                            conversion_types = names(data_to_write),
                            collection="units_and_conversions",
                            converted_values=T
                        )

                        save_multiple_conversions(
                            database = database,
                            url = "mongodb://localhost",
                            projectID = project_name,
                            formID = form_name,
                            conversion_data = data_to_write,
                            conversion_types = names(data_to_write),
                            collection="unmodified_units"
                        )
                        return()
                    }
                    save_list_of_df_to_db(
                        list_of_df = data_to_write,
                        projectID = project_name,
                        formID = form_name,
                        database = database,
                        url = "mongodb://localhost"
                    )
                    set_project_tag_to_true(database = database,
                                            url = url,
                                            projectID=project_name,
                                            formID=form_name,
                                            project_tag="pricesCalculated")
                    return()
                }


            })




            if ("processed_data" %in% names(results)) {
                calorie_conversions_dfs <- check_existing_calorie_conversions(results$processed_data)
                calorie_conversions_dfs$staple_crop <- make_per_project_conversion_tibble(proj_id_vector = rhomis_data[["id_rhomis_dataset"]], unit_conv_tibble = list(
                    "staple_crop" = c("maize")
                ))


                if (outputType == "csv") {
                    original_calorie_values_folder <- paste0(base_path, ".original_calorie_conversions")
                    write_list_of_df_to_folder(list_of_df = calorie_conversions_dfs, folder = original_calorie_values_folder)

                    converted_calorie_conversions_folder <- paste0(base_path, "calorie_conversions")
                    write_list_of_df_to_folder(list_of_df = calorie_conversions_dfs, folder = converted_calorie_conversions_folder,converted_values=T)


                    data_to_write <- results[["original_prices"]]
                    original_mean_prices_folder <- paste0(base_path, ".original_mean_prices_conversions")
                    write_list_of_df_to_folder(list_of_df = data_to_write, folder = original_mean_prices_folder)

                    converted_prices_folder <- paste0(base_path, "mean_prices")
                    write_list_of_df_to_folder(list_of_df = data_to_write, folder = converted_prices_folder,converted_values=T)
                }

                if (outputType == "mongodb") {
                    save_multiple_conversions(
                        database = database,
                        url = "mongodb://localhost",
                        projectID = project_name,
                        formID = form_name,
                        conversion_data = calorie_conversions_dfs,
                        conversion_types = names(calorie_conversions_dfs),
                        collection="units_and_conversions",
                        converted_values=T

                    )

                    save_multiple_conversions(
                        database = database,
                        url = "mongodb://localhost",
                        projectID = project_name,
                        formID = form_name,
                        conversion_data = calorie_conversions_dfs,
                        conversion_types = names(calorie_conversions_dfs),
                        collection = "unmodified_units"
                    )
                }
            }


            return(results)
        }
        if (calculateFinalIndicatorsOnly == T) {

            if (outputType == "csv") {
                # Read in the processed csvs and check everything exists
                processed_data <- read_folder_of_csvs(folder = paste0(base_path, "processed_data/"))[[1]]
                indicator_data <- read_folder_of_csvs(folder = paste0(base_path, "indicator_data/"))[[1]]
                units <- load_local_units(paste0(base_path, "units_and_conversions/"), id_rhomis_dataset = processed_data[["id_rhomis_dataset"]])

                prices <- read_folder_of_csvs(folder = paste0(base_path, "mean_prices/"))
                calorie_conversions <- read_folder_of_csvs(folder = paste0(base_path, "calorie_conversions/"))
            }
            if (outputType == "mongodb") {
                # Read in the mongodb values and check everything exists
                processed_data <- read_in_db_dataset(
                    collection = "data",
                    database = database,
                    project_name = project_name,
                    form_name = form_name,
                    data_set_name = "processedData"
                )

                indicator_data <- read_in_db_dataset(
                    collection = "data",
                    database = database,
                    project_name = project_name,
                    form_name = form_name,
                    data_set_name = "indicatorData"
                )

                conversion_factors_list <- find_db_units(
                    projectID = project_name,
                    formID = form_name,
                    url = "mongodb://localhost",
                    collection = "projectData",
                    database = database
                )

                prices_conversion_list <- conversion_factors_list[conversion_factors_list %in% pkg.env$price_conversion_list]
                prices <- sapply(prices_conversion_list, function(price_conversion) {
                    extract_units_from_db(database,
                                          url = "mongodb://localhost",
                                          projectID = project_name,
                                          formID = form_name,
                                          conversion_type = price_conversion,
                                          collection = "units_and_conversions"
                    )
                }, simplify = F)

                calorie_conversion_list <- conversion_factors_list[conversion_factors_list %in% pkg.env$calorie_conversion_list]
                calorie_conversions <- sapply(calorie_conversion_list, function(calorie_conversion) {
                    extract_units_from_db(database,
                                          url = "mongodb://localhost",
                                          projectID = project_name,
                                          formID = form_name,
                                          conversion_type = calorie_conversion,
                                          collection = "units_and_conversions"
                    )
                }, simplify = F)

                if (outputType == "mongodb") {
                    unit_list <- find_db_units(
                        projectID = project_name,
                        formID = form_name,
                        url = "mongodb://localhost",
                        collection = "projectData",
                        database = database
                    )
                    # Not yet complete
                    load_all_db_units(unit_list,
                                      projectID = project_name,
                                      formID = form_name,
                                      database = database,
                                      id_rhomis_dataset = processed_data[["id_rhomis_dataset"]]
                    )
                }
            }

            results <- value_gender_fa_calculations(
                processed_data = processed_data,
                indicator_data = indicator_data,
                calorie_conversions = calorie_conversions,
                prices = prices,
                gender_categories = gender_categories,
                units = units
            )



            lapply(names(results), function(x) {
                data_to_write <- results[[x]]
                if(length(data_to_write)==0){
                    return()
                }
                if (outputType == "csv") {
                    if (x == "processed_data" | x == "indicator_data") {
                        new_folder <- paste0(base_path, x)
                        dir.create(new_folder, showWarnings = F)

                        path <- paste0(new_folder, "/", x, ".csv")
                        readr::write_csv(data_to_write, path)
                        return()
                    }


                    if (x == "extra_outputs") {
                        write_list_of_df_to_folder(list_of_df = data_to_write, folder = base_path)
                    }
                }

                if (outputType == "mongodb") {
                    if (x == "processed_data") {
                        save_data_set_to_db(
                            data = data_to_write,
                            data_type = "processedData",
                            database = database,
                            url = "mongodb://localhost",
                            projectID = project_name,
                            formID = form_name
                        )


                        return()
                    }

                    if (x == "indicator_data") {
                        save_data_set_to_db(
                            data = data_to_write,
                            data_type = "indicatorData",
                            database = database,
                            url = "mongodb://localhost",
                            projectID = project_name,
                            formID = form_name
                        )
                        return()
                    }


                    if (x == "extra_outputs") {
                        save_list_of_df_to_db(
                            list_of_df = data_to_write,
                            projectID = project_name,
                            formID = form_name,
                            database = database,
                            url = "mongodb://localhost"
                        )
                    }
                    set_project_tag_to_true(database = database,
                                            url = url,
                                            projectID=project_name,
                                            formID=form_name,
                                            project_tag="finalIndicators")
                }
            })


            return(results)
        }

        # return(rhomis_data)
    }
}










#' Generate Data
#'
#' Generate fake data and submit it to a test project
#'
#' Rpackage file: RunAll.R
#'
#' @param central_url The URL of the central server holding the data
#' @param central_email The email of the administrative user
#' @param central_password The password of the administrative user
#' @param project_name The name of the project to generate data for
#' @param form_name The name of the form to generate data for
#' @param number_of_responses The number of responses to generate
#' @param isDraft Whether or not this is a draft state project
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
                         isDraft = T) {


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


    xls_form <- get_xls_form(central_url,
                             central_email,
                             central_password,
                             projectID,
                             formID,
                             isDraft,file_destination = )

    xls_form$settings$version <-forms$version[forms$name == form_name]

    # Get number of responses to generate
    for (response_index in 1:number_of_responses)
    {
        mock_response <- generate_mock_response(
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
            isDraft = isDraft
        )
    }
    # Delete the xls file
    write("Success in generating responses", stdout())
}
