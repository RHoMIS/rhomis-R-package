
calculate_prices <- function(
    rhomis_data,
    units_and_conversions,
    gender_categories = pkg.env$gender_categories
){

    results <- run_preliminary_calculations(
                rhomis_data = rhomis_data,
                gender_categories = gender_categories,
                units = units
            )
    

    return(results)
}