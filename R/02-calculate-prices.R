#' Get Price Calorie and TLU Conversions
#' 
#' Rpackage file: 02-calculate-prices.R
#' 
#' @param rhomis_data A "raw" rhomis dataset
#' 
#' @return
#' @export
#'
#' @examples
get_price_calorie_and_tlu_conversions <- function(
    rhomis_data,
    units_and_conversions,
    gender_categories = pkg.env$gender_categories
){

  # Replace all of the "other1", "other2"... options
  # in the survey with their actual values.
  # We will then replace mispelt values 
  # using the conversions tables
  rhomis_data <- replace_crop_and_livestock_other(rhomis_data)

  results <- list()
  prices <- list()
  crop_outputs <- list()
  livestock_outputs <- list()

  # Check for the number of crop loops
  # and identify the "crop_name" columns
  number_crop_loops <- find_number_of_loops(rhomis_data, "crop_name")
  crop_loops <- paste0("crop_name_", 1:number_crop_loops)

  # Checks if columns are missing, if columns do not exist then they are returned
  crop_name_in_data <- check_columns_in_data(rhomis_data, loop_columns = "crop_name")

  # If there are no "crop_name" columns missing. Take the entries
  # in crop_name and replace them using crop_name conversions
  if (length(crop_name_in_data) == 0) {
    rhomis_data[crop_loops] <- switch_units(rhomis_data[crop_loops],
      unit_tibble = units$crop_name_conversions,
      rhomis_data[["id_rhomis_dataset"]]
    )
  }
  
  # Also check through livestock loops and replace conversions
  number_livestock_loops <- find_number_of_loops(rhomis_data, "livestock_name")
  livestock_loops <- paste0("livestock_name_", 1:number_livestock_loops)

  # Checks if columns are missing, if columns do not exist then they are returned
  livestock_name_in_data <- check_columns_in_data(rhomis_data, loop_columns = "livestock_name")

  if (length(livestock_name_in_data) == 0) {
    rhomis_data[livestock_loops] <- switch_units(rhomis_data[livestock_loops],
      unit_tibble = units$livestock_name_conversions,
      id_vector = rhomis_data[["id_rhomis_dataset"]]
    )
  }

  # Make sure "other" units are considered
  rhomis_data <- replace_units_with_other_all(rhomis_data)

  # Run all crop calculations.
  # Essentially all crop calculations
  # have to be run to get crop prices.
  # After this function is run, the RHoMIS

  rhomis_data <- crop_calculations_all(
    rhomis_data,
    crop_yield_units_conv_tibble = units$crop_yield_unit_conversions,
    crop_income_units_conv_tibble = units$crop_price_unit_conversions,
    gender_categories = gender_categories
  )

  missing_crop_columns <- check_columns_in_data(rhomis_data,
    loop_columns = "crop_price"
  )
  crop_data <- map_to_wide_format(
    data = rhomis_data,
    name_column = "crop_name",
    column_prefixes = "crop_price",
    types = rep("num")
  )

  crop_price <- crop_data[["crop_price"]]
  crop_price$id_rhomis_dataset <- rhomis_data[["id_rhomis_dataset"]]
  crop_price <- crop_price %>%
        dplyr::mutate_all(replace_infinite) %>%
        dplyr::group_by(id_rhomis_dataset) %>%
        dplyr::summarise_all(mean, na.rm = TRUE)

      crop_price <- crop_price %>% tidyr::pivot_longer(!id_rhomis_dataset, names_to = "survey_value", values_to = "conversion")
      crop_price$unit_type <- "crop_price_lcu_per_kg"
      prices$mean_crop_price_lcu_per_kg <- crop_price
    }

    data_to_bind <- make_new_dataset(rhomis_data)
    crop_data <- lapply(crop_data, function(x) {
      dplyr::bind_cols(data_to_bind, x)
    })
  }

  if (length(missing_crop_columns) == length(crop_columns)) {
    crop_data <- NULL
    warning("No extra outputs generated for crop loops")
  }
    # results <- run_preliminary_calculations(
    #             rhomis_data = rhomis_data,
    #             gender_categories = gender_categories,
    #             units = units
    #         )
    

    return(results)
}


calculate_prices_csv <- function(
    project_folder="./"
){

}