

load_rhomis_csvs <- function(
    project_path="./",
    units_folder="units_and_conversions",
    calories_folder="calorie_conversions"

){

    units_path <- paste0(base_path, units_folder)
    calories_path <- paste0(base_path, calories_folder)


}




#' Load Survey CSV
#'
#' Load a Raw or processed RHoMIS survey csv file, collected using ODK,
#' and convert the column names into a shortened, standardised
#' version. This function can either be used to read
#'
#' Rpackage file: utils-csv-data.R
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
#' @param repeat_columns The types of repeat column name
#'
#' @return A tibble of RHoMIS data
#' @export
#'
#' @examples
load_survey_csv <- function(file_path,
                            country_column = "country",
                            unique_id_col = "_uuid",
                            hh_id_col = NULL,
                            id_type = c("string", "column"), # list of allowed values for argument, default is first element in vector
                            proj_id = NULL,
                            form_id = NULL,
                            overwrite = FALSE,
                            repeat_columns = pkg.env$repeat_columns)
{

    # read in the input csv file
    rhomis_data <- readr::read_csv(file_path,
        col_types = readr::cols(),
        na = c("n/a", "-999", "NA", "-99", "na", ""),
        locale = readr::locale(encoding = "UTF8")
        )

    # simplify column names to more readable format
    colnames(rhomis_data) <- clean_column_names(colnames(rhomis_data))

    # ensure all data entries are lower case for consistency / easier data analysis


    rhomis_data <- convert_all_columns_to_lower_case(rhomis_data)

    # temp manual intervention to account for non-standard/missing column fields

    rhomis_data <- make_id_columns(
        data = rhomis_data,
        country_column,
        unique_id_col = unique_id_col,
        hh_id_col = hh_id_col,
        id_type = id_type,
        proj_id = proj_id,
        form_id = form_id
    )

    return(rhomis_data)
}
