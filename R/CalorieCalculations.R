#' These functions are designed to help a user calculate
#' the calories from farm products which are consumed

#' Calorie Calculations
#'
#' Conducting the calorie calculations
#' for a whole project
#'
#' Rpackage file: CalorieCalculations.R
#'
#' @param processed_data A processed dataset
#' @param indicator_data An indicator dataset
#' @param calorie_conversions A tibble of calorie conversions
#'
#' @return
#' @export
#'
#' @examples
calorie_calculations <- function(processed_data,
                                 indicator_data,
                                 calorie_conversions) {
    extra_outputs <- list()
    # Crop value calcs
    missing_columns <- check_columns_in_data(processed_data,
                                             loop_columns = c("crop_name", "crop_consumed_kg_per_year"),
                                             warning_message = "Could not calculate kcal crop consumed"
    )
    # indicator_search_crop_calories_consumed_kcal
    if ("crop_calories" %in% names(calorie_conversions) & length(missing_columns) == 0) {
        if (!is.null(calorie_conversions[["crop_calories"]])){
            processed_data <- value_or_calorie_calculations_item_consumed(
                data = processed_data,
                name_column = "crop_name",
                amount_consumed_column = "crop_consumed_kg_per_year",
                conversion_tibble = calorie_conversions[["crop_calories"]],
                price_column_name = "crop_calories_kcal_per_kg",
                converted_column_name = "crop_calories_consumed_kcal"
            )


            extra_outputs$crop_calories_consumed_kcal <- map_to_wide_format(
                data = processed_data, name_column = "crop_name", column_prefixes = "crop_calories_consumed_kcal",
                types = "num"
            )[[1]]
        }
    }

    if ("crop_calories" %in% names(calorie_conversions) == F) {
        warning("Unable to calculate the calories of crops consumed, no calorie conversions for crops loaded")
    }

    # Meat value calcs
    missing_columns <- check_columns_in_data(processed_data,
                                             loop_columns = c("livestock_name", "meat_consumed_kg_per_year"),
                                             warning_message = "Could not calculate kcal meat consumed"
    )
    # indicator_search_meat_calories_consumed_kcal
    if ("meat_calories" %in% names(calorie_conversions) & length(missing_columns) == 0) {
        if (!is.null(calorie_conversions[["meat_calories"]])){

            processed_data <- value_or_calorie_calculations_item_consumed(
                data = processed_data,
                name_column = "livestock_name",
                amount_consumed_column = "meat_consumed_kg_per_year",
                conversion_tibble = calorie_conversions[["meat_calories"]],
                price_column_name = "meat_calories_kcal_per_kg",
                converted_column_name = "meat_calories_consumed_kcal"
            )

            extra_outputs$meat_calories_consumed_kcal <- map_to_wide_format(
                data = processed_data, name_column = "livestock_name", column_prefixes = "meat_calories_consumed_kcal",
                types = "num"
            )[[1]]
        }
    }

    if ("meat_calories" %in% names(calorie_conversions) == F) {
        warning("Unable to calculate the calories of meat consumed, no calorie conversions for meat loaded")
    }

    # Egg value calcs
    # indicator_search_eggs_calories_consumed_kcal
    missing_columns <- check_columns_in_data(processed_data,
                                             loop_columns = c("livestock_name", "eggs_consumed_kg_per_year"),
                                             warning_message = "Could not calculate kcal eggs consumed"
    )
    if ("eggs_calories" %in% names(calorie_conversions) & length(missing_columns) == 0) {
        if (!is.null(calorie_conversions[["eggs_calories"]])){

            processed_data <- value_or_calorie_calculations_item_consumed(
                data = processed_data,
                name_column = "livestock_name",
                amount_consumed_column = "eggs_consumed_kg_per_year",
                conversion_tibble = calorie_conversions[["eggs_calories"]],
                price_column_name = "eggs_calories_kcal_per_kg",
                converted_column_name = "eggs_calories_consumed_kcal"
            )

            extra_outputs$eggs_calories_consumed_kcal <- map_to_wide_format(
                data = processed_data, name_column = "livestock_name", column_prefixes = "eggs_calories_consumed_kcal",
                types = "num"
            )[[1]]
        }
    }

    if ("eggs_calories" %in% names(calorie_conversions) == F) {
        warning("Unable to calculate the calories of eggs consumed, no calorie conversions for eggs loaded")
    }


    # Milk value calcs
    # indicator_search_milk_calories_consumed_kcal
    missing_columns <- check_columns_in_data(processed_data,
                                             loop_columns = c("livestock_name", "milk_consumed_litres_per_year"),
                                             warning_message = "Could not calculate kcal milk consumed"
    )
    if ("milk_calories" %in% names(calorie_conversions) & length(missing_columns) == 0) {
        if (!is.null(calorie_conversions[["milk_calories"]])){

            processed_data <- value_or_calorie_calculations_item_consumed(
                data = processed_data,
                name_column = "livestock_name",
                amount_consumed_column = "milk_consumed_litres_per_year",
                conversion_tibble = calorie_conversions[["milk_calories"]],
                price_column_name = "milk_calories_kcal_per_litre",
                converted_column_name = "milk_calories_consumed_kcal"
            )

            extra_outputs$milk_calories_consumed_kcal <- map_to_wide_format(
                data = processed_data, name_column = "livestock_name", column_prefixes = "milk_calories_consumed_kcal",
                types = "num"
            )[[1]]
        }
    }

    if ("milk_calories" %in% names(calorie_conversions) == F) {
        warning("Unable to calculate the calories of milk consumed, no calorie conversions for milk loaded")
    }


    # Honey calorie calcs
    # indicator_search_bees_honey_calories_consumed_kcal
    missing_columns <- check_columns_in_data(processed_data,
                                             loop_columns = c("livestock_name", "bees_honey_consumed_kg_per_year"),
                                             warning_message = "Could not calculate kcal honey consumed"
    )
    if ("honey_calories" %in% names(calorie_conversions) & length(missing_columns) == 0) {
        if (!is.null(calorie_conversions[["honey_calories"]])){

            processed_data <- value_or_calorie_calculations_item_consumed(
                data = processed_data,
                name_column = "livestock_name",
                amount_consumed_column = "bees_honey_consumed_kg_per_year",
                conversion_tibble = calorie_conversions[["honey_calories"]],
                price_column_name = "bees_honey_calories_kcal_per_kg",
                converted_column_name = "bees_honey_calories_consumed_kcal"
            )

            extra_outputs$bees_honey_calories_consumed_kcal <- map_to_wide_format(
                data = processed_data,
                name_column = "livestock_name",
                column_prefixes = "bees_honey_calories_consumed_kcal",
                types = "num"
            )[[1]]
        }
    }

    if ("honey_calories" %in% names(calorie_conversions) == F) {
        warning("Unable to calculate the calories of honey consumed, no calorie conversions for honey loaded")
    }

    # result <- list()
    # result$processed_data <- processed_data
    # result$extra_outputs <- extra_outputs



    result <- list()


    if (length(extra_outputs) > 0) {
        total_calories_consumed_by_category <- lapply(extra_outputs, function(x) {
            rowSums(x, na.rm = T)
        }) %>% tibble::as_tibble()

        extra_outputs$calories_per_product_kcal_per_year <- total_calories_consumed_by_category

        # indicator_search_crop_consumed_calories_kcal_per_hh_per_year
        if ("crop_calories_consumed_kcal" %in% colnames(total_calories_consumed_by_category)) {
            indicator_data$crop_consumed_calories_kcal_per_hh_per_year <- total_calories_consumed_by_category$crop_calories_consumed_kcal
        }

        # indicator_search_livestock_consumed_calories_kcal_per_hh_per_year
        if (any(c("meat_calories_consumed_kcal", "eggs_calories_consumed_kcal", "milk_calories_consumed_kcal", "bees_honey_calories_consumed_kcal") %in% colnames(total_calories_consumed_by_category))) {
            indicator_data$livestock_consumed_calories_kcal_per_hh_per_year <- rowSums(total_calories_consumed_by_category[colnames(total_calories_consumed_by_category) %in%
                                                                                                                               c(
                                                                                                                                   "meat_calories_consumed_kcal",
                                                                                                                                   "eggs_calories_consumed_kcal",
                                                                                                                                   "milk_calories_consumed_kcal",
                                                                                                                                   "bees_honey_calories_consumed_kcal"
                                                                                                                               )],
                                                                                       na.rm = T
            )
        }

        # indicator_search_farm_products_consumed_calories_kcal_per_hh_per_year
        indicator_data$farm_products_consumed_calories_kcal_per_hh_per_year <- rowSums(total_calories_consumed_by_category, na.rm = T)
    }

    id_cols <- make_new_dataset(processed_data)
    extra_outputs <- lapply(extra_outputs, function(x) {
        dplyr::bind_cols(id_cols, x)
    })

    result$processed_data <- processed_data
    result$consumption_kcal_values <- extra_outputs
    result$indicator_data <- indicator_data


    return(result)
}
