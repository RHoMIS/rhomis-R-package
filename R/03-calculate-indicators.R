
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