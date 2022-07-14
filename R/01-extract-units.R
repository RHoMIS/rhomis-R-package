

#' Extract Units and Conversions
#' 
#' A function to extract units from the 
#' RHoMIS dataset, store them 
#' 
#' @param rhomis_data A "raw" rhomis dataset
#' @param gender_categories
#' 
#' @return
#' @export
#'
#' @examples
extract_units_and_conversions <- function(
    rhomis_data,
    gender_categories = pkg.env$gender_categories
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