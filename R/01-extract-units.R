

#' Extract Units and Conversions
#' 
#' A function to extract units and 
#' conversion factors from the 
#' RHoMIS dataset
#' 
#' Rpackage file: 01-extract-units.R
#' 
#' @param rhomis_data A "raw" rhomis dataset
#' 
#' @return
#' @export
#'
#' @examples
extract_units_and_conversions <- function(  
    rhomis_data
){
    # Go through the RHoMIS dataset, check extract all of the values 
    # for columns which need a human to convert/verify
    units_and_conversions <- extract_values_by_project(rhomis_data)

    # Go through the conversion lists extracted, check the 
    # data stored in the R-package to see if we are able 
    # to convert any of the 
    units_and_conversions <- check_existing_conversions(list_of_df = units_and_conversions)

    return(units_and_conversions)
}

#' Extract Units and Conversions CSV
#' 
#' Rpackage file: 01-extract-units.R
#' 
#' @param file_path Filepath to a RHoMIS Survey CSV
#' 
#' @return
#' @export
#'
#' @examples
extract_units_and_conversions_csv <- function(
    file_path
){

    if (!file.exists(file_path)){
        stop("No file found at filepath provided")
    }

    rhomis_data <- load_survey_csv("file")
    units_and_conversions <- extract_units_and_conversions(rhomis_data)

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


#' Extract Units and Conversions CSV
#' 
#' Rpackage file: 01-extract-units.R
#' 
#' @param central_url The url of the ODK-central
#' @param central_email The email of the ODK-central account being used
#' @param central_password The password of the ODK-central account being used
#' @param project_name The name of the ODK-central project being processed
#' @param form_name The name of the ODK-central form being processed
#' @param form_version The version of the ODK-central form being processed
#' @param central_test_case This flag is used for running a test-sample dataset from ODK the inst/sample_central_project/ folder
#' @param database The name of the database you would like to save results to
#' @param isDraft Whether or not the ODK form you are working with is a draft
#' or a final version. Only relevant if you are processing a project from ODK central
#' @param repeat_columns ODK has a "repeat loop" structure to ask repeated questions. 
#' 
#' @return
#' @export
#'
#' @examples
extract_units_and_conversions_server <- function(
    central_url,
    central_email,
    central_password,
    project_name,
    form_name,
    database,
    isDraft,
    central_test_case = FALSE,
    repeat_columns = pkg.env$repeat_columns
){

 
    # Load the RHoMIS Dataset from ODK central
    rhomis_data <- load_rhomis_central(
            central_url,
            central_email,
            central_password,
            project_name,
            form_name,
            database,
            isDraft,
            central_test_case,
            repeat_columns
        )    


    units_and_conversions <- extract_units_and_conversions(rhomis_data)

    # Save the "unmodified_units" in
    # one part of the db
    save_multiple_conversions(
        database = database,
        url = url,
        projectID = project_name,
        formID = form_name,
        conversion_data = units_and_conversions,
        conversion_types = names(units_and_conversions),
        collection = "unmodified_units"
    )

    # Save the units which will be converted
    # in another part of the database
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

    # Tag the project in the database
    # to show that units have been extracted
    set_project_tag_to_true(database = database,
        url = url,
        projectID=project_name,
        formID=form_name,
        project_tag="unitsExtracted")
        

    return(units_and_conversions)
}