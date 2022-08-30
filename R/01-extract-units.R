

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
#' Extract all units from a RHoMIS CSV file, and write these
#' unit conversion tables to be a file, which will be verified by the user.
#'
#'
#' Rpackage file: 01-extract-units.R
#'
#' @param file_path Filepath to a RHoMIS Survey CSV
#' @param base_path The folder where you want to save any outputs, usually your current working directory ("./")
#' @param id_type RHoMIS surveys have form and project IDs. Sometimes the form and project IDs are included as a column in the dataset (id_type="column"), or the IDs are specified by the user at the point of processing (id_type="string")
#' @param proj_id If ID type was string, this should be a string, if ID type was column, this should be a column name containing project IDs
#' @param form_id If ID type was string, this should be a string, if ID type was column, this should be a column name containing form IDs
#' @param repeat_columns The types of repeat column name
#'
#' @return
#' @export
#'
#' @examples
extract_units_and_conversions_csv <- function(
        base_path="./",
        file_path,
        id_type=c("string", "column"),
        proj_id,
        form_id,
        repeat_columns = pkg.env$repeat_columns

){

    rhomis_data <- load_rhomis_csv(
        file_path = file_path,
        id_type = id_type,
        proj_id = proj_id,
        form_id = form_id,
        repeat_columns=repeat_columns
    )
    units_and_conversions <- extract_units_and_conversions(rhomis_data)

    units_folder_dest <- paste0(base_path, ".original_stage_1_conversions")
    write_units_to_folder(
        list_of_df = units_and_conversions,
        folder = units_folder_dest
    )

    new_units_dest <- paste0(base_path, "conversions_stage_1")


    write_units_to_folder(
        list_of_df = units_and_conversions,
        folder = new_units_dest
    )
    return(units_and_conversions)
}


#' Extract Units and Conversions from Server
#'
#' Rpackage file: 01-extract-units.R
#'
#' @param central_url The url of the ODK-central
#' @param central_email The email of the ODK-central account being used
#' @param central_password The password of the ODK-central account being used
#' @param project_name The name of the ODK-central project being processed
#' @param form_name The name of the ODK-central form being processed
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
        central_url=central_url,
        central_email=central_email,
        central_password=central_password,
        project_name=project_name,
        form_name=form_name,
        database=database,
        isDraft=isDraft,
        central_test_case=central_test_case,
        repeat_columns=repeat_columns
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
