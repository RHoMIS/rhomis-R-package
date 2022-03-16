


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
                            overwrite=FALSE,
                            repeat_columns=pkg.env$repeat_columns) {


    # read in the input csv file
    rhomis_data <- readr::read_csv(file_path, col_types = readr::cols(), na = c("n/a","-999","NA"))

    # simplify column names to more readable format
    colnames(rhomis_data) <- clean_column_names(colnames(rhomis_data), repeat_columns)

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
#' or from ODK central.
#'
#' RHoMIS datasets go through 4 stages of processing:
#'
#' 1. Initial Cleaning and Extracting Units
#' 2. Calculation of initial indicators
#' 3. Calculation of final indicators, including food availability, gender, and value of products consumed
#'
#' @param extractUnitsOnly Whether or not to only extract units (TRUE/FALSE)
#' @param calculateInitialIndicatorsOnly Whether or not to only calcu,ate initial indicators (TRUE/FALSE)
#' @param calculateFinalIndicatorsOnly Whether or not to only calculate final indicators
#' @param dataSource The type of RHoMIS data being fed into the
#' calculations, whether a local csv file or data from ODK central.
#' Options "csv" or "central".
#' @param base_path
#' @param outputType  The type of output to produce (options are "csv" or "mongodb")
#' @param dataFilePath The file to the data (csv format).
#' ONLY RELEVANT IF "dataSource" WAS "local".
#' @param overwrite Whether or not to overwrite the converted values
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
#' @param form_version The version of the ODK-central form you are processing.
#' ONLY RELEVANT IF "dataSource" WAS "central".
#' @param database The name of the database you would like to save results to
#' @param draft Whether or not the ODK for you war working with is a draft
#' or a final version. Only relevant if you are processing a project from ODK central
#' @param repeat_columns The columns which are looped in the datasets being processed
#'
#' @return
#' @export
#'
#' @examples
processData <- function(
        # Arguments to indicate the stage of analysis
    extractUnitsOnly=T, # The stage of data processing
    calculateInitialIndicatorsOnly=F,
    calculateFinalIndicatorsOnly=F,

    # Arguments to indicate the type of processing being done (local or on server)
    dataSource=c("csv", "central"), # list of allowed values for argument, default is first element in vector (csv),
    outputType=c("csv", "mongodb"), # list of allowed values for argument, default is first element in vector (csv),

    # Arguments used for processing local data sets
    base_path="./",
    dataFilePath=NULL,
    id_type=c("string", "column"),
    proj_id,
    form_id,
    overwrite=F,

    # Arguments for if processing from ODK central
    central_url=NULL,
    central_email=NULL,
    central_password=NULL,
    project_name=NULL,
    form_name=NULL,
    form_version=NULL,
    database=NULL,
    draft=NULL,
    repeat_columns=pkg.env$repeat_columns,
    gender_categories=pkg.env$gender_categories
){


    #----------------------------------------
    # Checking the validity of the Arguments
    #----------------------------------------

    # Check validity of OutputTypes and print error if unknown OutputType is supplied
    outputType <- match.arg(outputType)
    dataSource <- match.arg(dataSource)



    # If the user specified a csv, then they must provide a file path
    # for the dataset they are loading
    if(dataSource=="csv"){
        if(is.null(dataFilePath)){
            stop('You specified the data was coming from a local csv but have not specified a "dataFilePath"')
        }
    }

    # Checking if the right arguments are supplied to obtain data from ODK central
    if(dataSource=="central"){
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

    #' If local csv specified, load the csv
    #' add some identification columns and
    #' clean the column names
    if(dataSource=="csv")
    {
        rhomis_data <- load_rhomis_csv(
            file_path=dataFilePath,
            country_column= pkg.env$identification_column_list$country,
            unique_id_col=pkg.env$identification_column_list$uuid_local,
            hh_id_col=NULL,
            id_type=id_type,
            proj_id=proj_id,
            form_id=form_id,
            repeat_columns = repeat_columns

        )
    }

    #' If central dataset specified,
    #' identify the relevant projects,
    #' and load the zip files.
    #' add some identification columns and
    #' clean the column names
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

        # Cleaning the column names
        colnames(rhomis_data) <- clean_column_names(colnames(rhomis_data), pkg.env$repeat_columns)
        #' There are some extra central columns
        #' which are problematic, these need to be removed
        rhomis_data <- rhomis_data %>%
            remove_extra_central_columns()

        rhomis_data <- make_id_columns(
            data=rhomis_data,
            country_column =pkg.env$identification_column_list$country,
            unique_id_col=unique_id_col,
            hh_id_col=hh_id_col,
            id_type=id_type,
            proj_id=project_name,
            form_id=form_name)

        # Convert the IDs to lower case
        rhomis_data<- convert_all_columns_to_lower_case(rhomis_data)

        # An extra step to ensure all -999 values are set to NA,
        # This wasn't always happening with ODK central datasets.
        rhomis_data <- sapply(rhomis_data, function(x){
            x[as.numeric(x)==-999]<-NA
            x
        }, simplify = F) %>% tibble::as_tibble()
    }


    #' Make an empty indicator dataset with
    #' matching ID columns

    indicator_data <- make_new_dataset(rhomis_data)

    #---------------------------------------------------------------
    # Extract and write units
    #---------------------------------------------------------------

    if(extractUnitsOnly==T)
    {

        # Extract the new units, and replace them with units
        # which are stored in the package where possible.
        units_and_conversions <- extract_values_by_project(rhomis_data)
        units_and_conversions <- check_existing_conversions(list_of_df = units_and_conversions)

        if(outputType=="csv"){

            units_folder_dest <- paste0(base_path,"/original_units")
            write_units_to_folder(list_of_df = units_and_conversions,
                                  folder=units_folder_dest)

            new_units_dest <- paste0(base_path,"/converted_units")

            if (dir.exists(new_units_dest)==F){
                write_units_to_folder(list_of_df = units_and_conversions,
                                      folder=new_units_dest)
            }
            if (overwrite==T){
                write_units_to_folder(list_of_df = units_and_conversions,
                                      folder=new_units_dest)
            }

            return(units_and_conversions)

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
        units_folder <- paste0(base_path,"converted_units/")
        if (outputType=="csv"){
            if (!dir.exists(units_folder))
            {
                stop('Specified that the units were stored locally but the path "unit_conversions" does not exist')
            }

            file_names <- list.files(units_folder)
            #---------------------------------------------
            # Loading all of the unit conversions locally
            #---------------------------------------------
            load_local_units(units_folder, id_rhomis_dataset = rhomis_data[["id_rhomis_dataset"]])

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


        if (calculateInitialIndicatorsOnly==T){

            #' Run all of the preliminary calculations that can
            #' be done without price verification and without
            #' verification of calory values
            #' This function can be found in the
            #' R/redirectModules.R file.
            #' From this function we receive a list of
            #' data frames. These include processed_data,
            #' indicator_data, and extra_outputs
            results <- run_preliminary_calculations(rhomis_data = rhomis_data,
                                                    gender_categories = gender_categories)

            #' If the desired output format is csv,
            #' Write the processed data, indicator
            #' data and extra outputs
            #' will be written to the relevant files
            #'
            #'
            #-------------------------------------------------------------
            #' NEED TO SIMPLIFY
            #' Below we write all of the data
            #' which is processed during the initial calculations
            #-------------------------------------------------------------

            lapply(names(results), function(x){
                new_folder <- paste0(base_path,x)
                dir.create(new_folder, showWarnings = F)
                data_to_write <- results[[x]]

                if (outputType=="csv"){
                    if (x=="processed_data" | x=="indicator_data"){
                        path <- paste0(new_folder,"/",x,".csv")
                        readr::write_csv(data_to_write,path)
                        return()
                    }
                    write_list_of_df_to_folder(list_of_df = data_to_write,folder = new_folder)
                }

                if (outputType=="mongodb"){
                    if (x=="processed_data"){

                        save_data_set_to_db(
                            data = data_to_write,
                            data_type = "processedData",
                            database = database,
                            url = "mongodb://localhost",
                            projectID = project_name,
                            formID = form_name
                        )
                    }
                    if (x=="indicator_data"){

                        save_data_set_to_db(
                            data = data_to_write,
                            data_type = "indicatorData",
                            database = database,
                            url = "mongodb://localhost",
                            projectID = project_name,
                            formID = form_name
                        )
                    }
                }

                return()

                save_list_of_df_to_db(list_of_df = data_to_write,
                                      projectID=project_name,
                                      formID=form_name,
                                      database=database,
                                      url="mongodb://localhost"
                )
            })




            if ("processed_data" %in% names(results)){

                calorie_conversions_dfs <- check_existing_calorie_conversions(results$processed_data)
                calorie_conversions_dfs$staple_crop <- make_per_project_conversion_tibble(proj_id_vector = rhomis_data[["id_rhomis_dataset"]], unit_conv_tibble = list(
                    "staple_crop"=c("maize")
                ))

                original_calorie_values_folder <- paste0(base_path,"original_calorie_conversions")

                if (outputType=="csv"){
                    write_list_of_df_to_folder(list_of_df = calorie_conversions_dfs,folder = original_calorie_values_folder)
                    converted_calorie_conversions_folder <- paste0(base_path,"completed_calorie_conversions")
                    if (!dir.exists(converted_calorie_conversions_folder))
                    {
                        write_list_of_df_to_folder(list_of_df = calorie_conversions_dfs,folder = converted_calorie_conversions_folder)
                    }
                }

                if (outputType=="mongodb"){
                    save_list_of_df_to_db(list_of_df = calorie_conversions_dfs,
                                          projectID=project_name,
                                          formID=form_name,
                                          database=database,
                                          url="mongodb://localhost"
                    )
                }
            }

            converted_prices_folder <- paste0(base_path,"converted_prices")
            if (!dir.exists(converted_prices_folder))
            {
                dir.create(converted_prices_folder, showWarnings = F)
                data_to_write <- results[["original_prices"]]
                write_list_of_df_to_folder(list_of_df = data_to_write,folder = converted_prices_folder)
            }

            return(results)


        }
        if (calculateFinalIndicatorsOnly==T){

            if (outputType=="csv"){
                # Read in the processed csvs and check everything exists
                processed_data <- read_folder_of_csvs(folder = paste0(base_path,"processed_data/"))[[1]]
                indicator_data <- read_folder_of_csvs(folder = paste0(base_path,"indicator_data/"))[[1]]
                load_local_units(paste0(base_path,"converted_units/"), id_rhomis_dataset = processed_data[["id_rhomis_dataset"]])

                prices <- read_folder_of_csvs(folder = paste0(base_path,"converted_prices/"))
                calorie_conversions <- read_folder_of_csvs(folder = paste0(base_path,"completed_calorie_conversions/"))
            }
            if (outputType=="mongodb"){
                # Read in the mongodb values and check everything exists
            }



            results <- value_gender_fa_calculations(processed_data = processed_data,
                                                    indicator_data = indicator_data,
                                                    calorie_conversions = calorie_conversions,
                                                    prices = prices,
                                                    gender_categories = gender_categories
            )



            lapply(names(results), function(x){

                data_to_write <- results[[x]]

                if (outputType=="csv")
                {
                    if (x=="processed_data" | x=="indicator_data"){
                        new_folder <- paste0(base_path,x)
                        dir.create(new_folder, showWarnings = F)

                        path <- paste0(new_folder,"/",x,".csv")
                        readr::write_csv(data_to_write,path)
                        return()
                    }


                    if (x=="extra_outputs"){
                        write_list_of_df_to_folder(list_of_df = data_to_write,folder = base_path)
                    }
                }

                if (outputType=="mongodb")
                {
                    if (x=="processed_data"){

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

                    if (x=="indicator_data"){
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


                    if (x=="extra_outputs"){
                        save_list_of_df_to_db(list_of_df = data_to_write,
                                              projectID=project_name,
                                              formID=form_name,
                                              database=database,
                                              url="mongodb://localhost"
                        )                    }
                }
            })


            return(results)

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


