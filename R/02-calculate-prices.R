#' Get Secondary Conversions
#'
#' Rpackage file: 02-calculate-prices.R
#'
#' @param rhomis_data A "raw" rhomis dataset
#'
#' @return
#' @export
#'
#' @examples
get_secondary_conversions <- function(
        rhomis_data,
        units_and_conversions,
        gender_categories = pkg.env$gender_categories
){

    # Replace all of the "other1", "other2"... options
    # in the survey with their actual values.
    # We will then replace mispelt values
    # using the conversions tables

    # Replace all livestock names with "other"
    rhomis_data <- replace_crop_and_livestock_other(rhomis_data)

    # Create empty lists to fill with results
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
    if (length(crop_name_in_data) == 0 & "crop_name_conversions" %in% names(units_and_conversions)) {
        rhomis_data[crop_loops] <- switch_units(rhomis_data[crop_loops],
                                                unit_tibble = units_and_conversions$crop_name_conversions,
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
                                                     unit_tibble = units_and_conversions$livestock_name_conversions,
                                                     id_vector = rhomis_data[["id_rhomis_dataset"]]
        )
    }

    # Make sure "other" units are considered
    rhomis_data <- replace_units_with_other_all(rhomis_data)

    # Run all crop calculations.
    # Essentially all crop calculations
    # have to be run to get crop prices.
    # After this function is run,

    rhomis_data <- crop_calculations_all(
        rhomis_data,
        crop_yield_units_conv_tibble = units$crop_yield_unit_conversions,
        crop_income_units_conv_tibble = units$crop_price_unit_conversions,
        gender_categories = gender_categories
    )

    missing_crop_columns <- check_columns_in_data(rhomis_data,
                                                  loop_columns = "crop_price",
                                                  warning_message = "Could not export crop prices"
    )

    if (length(missing_crop_columns)==0)
    {
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

        data_to_bind <- make_new_dataset(rhomis_data)
        crop_data <- lapply(crop_data, function(x) {
            dplyr::bind_cols(data_to_bind, x)
        })
    }


    # results <- run_preliminary_calculations(
    #             rhomis_data = rhomis_data,
    #             gender_categories = gender_categories,
    #             units = units
    #         )


    livestock_weights <- make_per_project_conversion_tibble(proj_id_vector = rhomis_data[["id_rhomis_dataset"]], unit_conv_tibble = units$livestock_weights)

    rhomis_data <- livestock_calculations_all(rhomis_data,
                                              livestock_weights_conv_tibble = units$livestock_weights,
                                              eggs_amount_unit_conv_tibble = units$eggs_unit_conversion,
                                              honey_amount_unit_conv_tibble = units$honey_unit_conversion,
                                              milk_amount_unit_conv_tibble = units$milk_unit_conversion,
                                              milk_price_time_unit_conv_tibble = units$milk_price_unit_conversion,
                                              gender_categories = gender_categories
                                              # Need to add livestock weights to the conversions sheets
    )




    price_datasets <- c(
        "livestock_price_per_animal",
        "meat_price_per_kg",
        "milk_price_per_litre",
        "eggs_price_per_kg",
        "bees_honey_price_per_kg"
    )

    missing_livestock_columns <- check_columns_in_data(rhomis_data,
                                                       loop_columns = livestock_loop_columns,
                                                       warning_message = "Could not write extra outputs for these columns"
    )



    for (price_data_set in price_datasets) {
        if (price_data_set %in% names(livestock_data)) {
            price_df <- livestock_data[[price_data_set]]
            price_df$id_rhomis_dataset <- rhomis_data[["id_rhomis_dataset"]]
            mean_price_df <- price_df %>%
                dplyr::mutate_all(replace_infinite) %>%
                dplyr::group_by(id_rhomis_dataset) %>%
                dplyr::summarise_all(mean, na.rm = TRUE)

            mean_price_df <- mean_price_df %>% tidyr::pivot_longer(!id_rhomis_dataset, names_to = "survey_value", values_to = "conversion")
            mean_price_df$unit_type<- paste0("mean_", price_data_set)
            prices[[paste0("mean_", price_data_set)]] <- mean_price_df
        }
    }



    # Adding Extra units to convert
    # TLU Conversions
    # Livestock Weights
    # Calorie Conversions






    return(results)
}








#' Calculate Prices CSV
#'
#' Rpackage file: 02-calculate-prices.R
#'ß
#' @param project_folder
#'
#' @return
#' @export
#'
#' @examples
calculate_prices_csv <- function(
        project_folder="./"
){

}


#' Calculate_prices_server
#'
#' Rpackage file: 02-calculate-prices.R
#'
#' @return
#' @export
#'
#' @examples
calculate_prices_server <- function(){


}
