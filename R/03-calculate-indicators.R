
#' Calculate Indicators
#'
#' Calculate rhomis indicators. In
#' order to calculate indicators, it
#' is best to make sure that you have
#' first extracted units. That you
#' then verify prices and calories
#' for products in the
#'
#' Rpackage file: 03-calculate-indicators.R
#'
#' @param rhomis_data a rhomis dataset
#' @param units_and_conversions A list of units and conversions
#' @param prices A list of price conversions (mean price per kg)
#' @param calories A list of calorie conversions for different products
#' @param gender_categories The different categories of people (e.g. male_youth, female_youth, male_adult, female_adult)
#'
#' @return
#' @export
#'
#' @examples
calculate_indicators <- function(
    rhomis_data,
    units_and_conversions,
    prices,
    calories,
    gender_categories
){

     results <- value_gender_fa_calculations(
                processed_data = processed_data,
                indicator_data = indicator_data,
                calorie_conversions = calorie_conversions,
                prices = prices,
                gender_categories = gender_categories,
                units = units
            )

    return(results)
}
