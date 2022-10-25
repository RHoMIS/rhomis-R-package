


#' Extract Raw Data
#'
#' This function can be used to extract raw data from
#' an ODK central server, and save it to a RHoMIS
#' mongoDB database.
#'
#' @param central_url The url of the ODK-central
#' @param central_email The email of the ODK-central account being used
#' @param central_password The password of the ODK-central account being used
#' @param project_name The name of the ODK-central project being processed
#' @param form_name The name of the ODK-central form being processed
#' @param database The name of the database you would like to save results to
#' @param isDraft Whether or not the ODK form you are working with is a draft
#' or a final version. Only relevant if you are processing a project from ODK central
#' @return
#' @export
#'
#' @examples
extract_raw_data_server <- function(
        central_url,
        central_email,
        central_password,
        project_name,
        form_name,
        database,
        isDraft
){
    rhomis_data <- load_rhomis_central(
        central_url=central_url,
        central_email=central_email,
        central_password=central_password,
        project_name=project_name,
        form_name=form_name,
        database=database,
        isDraft=isDraft
    )

    save_data_set_to_db(
        data = rhomis_data,
        data_type = "rawData",
        database = database,
        url = "mongodb://localhost",
        projectID = project_name,
        formID = form_name
    )

    set_project_tag_to_true(database = database,
                            url = url,
                            projectID=project_name,
                            formID=form_name,
                            project_tag="rawDataExtracted")
    return()

}
