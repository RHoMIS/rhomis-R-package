#' Value or Calorie Calculations
#'
#' Calculate the value of an item in terms of calories or monetary value.
#'
#' @param data The whole processed rhomis dataset
#' @param name_column The loop column containing the name pattern (e.g "crop_name_1", "crop_name_2", "crop_name_3" would be "crop_name")
#' @param amount_consumed_column The loop column containing the consumed amount pattern (e.g. "crop_consumed_kg_per_year_1"... would be "crop_consumed_kg_per_year".)
#' @param conversion_tibble The dataframe for the conversion (e.g. price or calorie conversion sheet)
#' @param price_column_name The name you would like to include when adding the mean prices to the dataframe (e.g. mean_crop_price_per_kg)
#' @param converted_column_name The name of the converted column you would like to give (e.g.value_crop_consumed_lcu)
#'
#' @return
#' @export
#'
#' @examples
value_or_calorie_calculations_item_consumed <- function(data,
                                                        name_column,
                                                        amount_consumed_column,
                                                        conversion_tibble,
                                                        price_column_name,
                                                        converted_column_name) {
  missing_columns <- check_columns_in_data(data, loop_columns = c(name_column, amount_consumed_column), individual_columns = "id_rhomis_dataset")
  if (length(missing_columns) == 0) {
    number_of_loops <- find_number_of_loops(data, amount_consumed_column)

    names_columns <- paste0(name_column, "_", c(1:number_of_loops))
    prices_columns <- paste0(price_column_name, "_", c(1:number_of_loops))
    amounts_columns <- paste0(amount_consumed_column, "_", c(1:number_of_loops))
    new_columns <- paste0(converted_column_name, "_", c(1:number_of_loops))

    names_df <- data[names_columns]
    amounts_df <- data[amounts_columns]



    mean_prices_df <- switch_units(names_df, unit_tibble = conversion_tibble, id_vector = data[["id_rhomis_dataset"]])
    colnames(mean_prices_df) <- prices_columns

    converted_tibble <- mean_prices_df * amounts_df
    colnames(converted_tibble) <- new_columns



    if (all(prices_columns %in% colnames(data) == F)) {
      data <- add_column_after_specific_column(data,
        new_data = mean_prices_df,
        new_column_name = price_column_name,
        old_column_name = amount_consumed_column,
        loop_structure = T
      )
    }

    data <- add_column_after_specific_column(data,
      new_data = converted_tibble,
      new_column_name = converted_column_name,
      old_column_name = price_column_name,
      loop_structure = T
    )
  }

  if (length(missing_columns) > 0) {
    warning(paste0("Cannot calculate value of ", amount_consumed_column, ". Missing the following columns: ", missing_columns))
  }

  return(data)
}

# remove_existing_loop_if_exists <- function(data, name) {
#   already_existent_loop <- find_number_of_loops(processed_data, data)
#   if (already_existent_loop > 0) {
#     columns_to_remove <- paste0(name, c(1:already_existent_loop))
#     data <- data[colnames(data) %in% columns_to_remove == F]
#   }


  return(data)
}

#' Value Calculations
#'
#' Calculations of the value of products consumed
#'
#' @param processed_data A processed rhomis dataset
#' @param prices A list of tibbles including prices
#' @param gender_categories The gender categories to examing
#' @param indicator_data An indicator tibble
#'
#' @return
#' @export
#'
#' @examples
value_calculations <- function(processed_data,
                               indicator_data,
                               prices,
                               gender_categories) {
  extra_outputs <- list()

  # Crop value calcs
  if ("mean_crop_price_lcu_per_kg" %in% names(prices)) {

    # processed_data <- remove_existing_loop_if_exists(processed_data, "value_crop_consumed_lcu")
    processed_data <- value_or_calorie_calculations_item_consumed(
      data = processed_data,
      name_column = "crop_name",
      amount_consumed_column = "crop_consumed_kg_per_year",
      conversion_tibble = prices[["mean_crop_price_lcu_per_kg"]],
      price_column_name = "mean_crop_price_lcu_per_kg",
      converted_column_name = "value_crop_consumed_lcu"
    )


    missing_columns <- check_columns_in_data(processed_data,
      loop_columns = c(
        "value_crop_consumed_lcu",
        "crop_consume_control"
      ),
      warning_message = "Unable to calculate gendered control of crop consumption values"
    )
    if (length(missing_columns) == 0) {
      processed_data <- insert_gender_columns_in_core_data(
        data = processed_data,
        original_column = "value_crop_consumed_lcu",
        control_column = "crop_consume_control",
        loop_structure = T, gender_control_categories = gender_categories
      )
    }

    extra_outputs$value_crop_consumed_lcu <- map_to_wide_format(
      data = processed_data, name_column = "crop_name", column_prefixes = "value_crop_consumed_lcu",
      types = "num"
    )[[1]]
  }

  if ("mean_crop_price_lcu_per_kg" %in% names(prices) == F) {
    warning("Unable to calculate the value of crops consumed, no mean prices for crops loaded")
  }

  # Meat value calcs

  if ("mean_meat_price_per_kg" %in% names(prices)) {
    processed_data <- value_or_calorie_calculations_item_consumed(
      data = processed_data,
      name_column = "livestock_name",
      amount_consumed_column = "meat_consumed_kg_per_year",
      conversion_tibble = prices[["mean_meat_price_per_kg"]],
      price_column_name = "mean_meat_price_per_kg",
      converted_column_name = "value_meat_consumed_lcu"
    )

    missing_columns <- check_columns_in_data(processed_data,
      loop_columns = c(
        "value_meat_consumed_lcu",
        "livestock_meat_who_control_eating"
      ),
      warning_message = "Unable to calculate gendered control of meat consumption values"
    )
    if (length(missing_columns) == 0) {
      processed_data <- insert_gender_columns_in_core_data(
        data = processed_data,
        original_column = "value_meat_consumed_lcu",
        control_column = "livestock_meat_who_control_eating",
        loop_structure = T, gender_control_categories = gender_categories
      )
    }

    extra_outputs$value_meat_consumed_lcu <- map_to_wide_format(
      data = processed_data, name_column = "livestock_name", column_prefixes = "value_meat_consumed_lcu",
      types = "num"
    )[[1]]
  }

  if ("mean_meat_price_per_kg" %in% names(prices) == F) {
    warning("Unable to calculate the value of meat consumed, no mean prices for meat loaded")
  }

  # Egg value calcs

  if ("mean_eggs_price_per_kg" %in% names(prices)) {
    processed_data <- value_or_calorie_calculations_item_consumed(
      data = processed_data,
      name_column = "livestock_name",
      amount_consumed_column = "eggs_consumed_kg_per_year",
      conversion_tibble = prices[["mean_eggs_price_per_kg"]],
      price_column_name = "mean_eggs_price_per_kg",
      converted_column_name = "value_eggs_consumed_lcu"
    )

    missing_columns <- check_columns_in_data(processed_data,
      loop_columns = c(
        "value_eggs_consumed_lcu",
        "eggs_who_control_eating"
      ),
      warning_message = "Unable to calculate gendered control of egg consumption values"
    )
    if (length(missing_columns) == 0) {
      processed_data <- insert_gender_columns_in_core_data(
        data = processed_data,
        original_column = "value_eggs_consumed_lcu",
        control_column = "eggs_who_control_eating",
        loop_structure = T, gender_control_categories = gender_categories
      )
    }

    extra_outputs$value_eggs_consumed_lcu <- map_to_wide_format(
      data = processed_data, name_column = "livestock_name", column_prefixes = "value_eggs_consumed_lcu",
      types = "num"
    )[[1]]
  }

  if ("mean_eggs_price_per_kg" %in% names(prices) == F) {
    warning("Unable to calculate the value of eggs consumed, no mean prices for eggs loaded")
  }


  # Milk value calcs

  if ("mean_milk_price_per_litre" %in% names(prices)) {
    processed_data <- value_or_calorie_calculations_item_consumed(
      data = processed_data,
      name_column = "livestock_name",
      amount_consumed_column = "milk_consumed_litres_per_year",
      conversion_tibble = prices[["mean_milk_price_per_litre"]],
      price_column_name = "mean_milk_price_per_litre",
      converted_column_name = "value_milk_consumed_lcu"
    )

    missing_columns <- check_columns_in_data(processed_data,
      loop_columns = c(
        "value_milk_consumed_lcu",
        "milk_who_control_eating"
      ),
      warning_message = "Unable to calculate gendered control of milk consumption values"
    )
    if (length(missing_columns) == 0) {
      processed_data <- insert_gender_columns_in_core_data(
        data = processed_data,
        original_column = "value_milk_consumed_lcu",
        control_column = "milk_who_control_eating",
        loop_structure = T, gender_control_categories = gender_categories
      )
    }


    extra_outputs$value_milk_consumed_lcu <- map_to_wide_format(
      data = processed_data, name_column = "livestock_name", column_prefixes = "value_milk_consumed_lcu",
      types = "num"
    )[[1]]
  }

  if ("mean_milk_price_per_litre" %in% names(prices) == F) {
    warning("Unable to calculate the value of milk consumed, no mean prices for milk loaded")
  }


  # Milk value calcs

  if ("mean_bees_honey_price_per_kg" %in% names(prices)) {
    processed_data <- value_or_calorie_calculations_item_consumed(
      data = processed_data,
      name_column = "livestock_name",
      amount_consumed_column = "bees_honey_consumed_kg_per_year",
      conversion_tibble = prices[["mean_bees_honey_price_per_kg"]],
      price_column_name = "mean_bees_honey_price_per_kg",
      converted_column_name = "value_bees_honey_consumed_lcu"
    )

    missing_columns <- check_columns_in_data(processed_data,
      loop_columns = c(
        "value_bees_honey_consumed_lcu",
        "bees_who_control_eating"
      ),
      warning_message = "Unable to calculate gendered control of honey consumption values"
    )
    if (length(missing_columns) == 0) {
      processed_data <- insert_gender_columns_in_core_data(
        data = processed_data,
        original_column = "value_bees_honey_consumed_lcu",
        control_column = "bees_who_control_eating",
        loop_structure = T, gender_control_categories = gender_categories
      )
    }

    extra_outputs$value_bees_honey_consumed_lcu <- map_to_wide_format(
      data = processed_data, name_column = "livestock_name", column_prefixes = "value_bees_honey_consumed_lcu",
      types = "num"
    )[[1]]
  }

  if ("mean_bees_honey_price_per_kg" %in% names(prices) == F) {
    warning("Unable to calculate the value of honey consumed, no mean prices for honey loaded")
  }

  result <- list()


  if (length(extra_outputs) > 0) {
    total_value_consumed_by_category <- lapply(extra_outputs, function(x) {
      rowSums(x, na.rm = T)
    }) %>% tibble::as_tibble()

    extra_outputs$calories_per_product_kcal_per_year <- total_value_consumed_by_category


    if ("value_crop_consumed_lcu" %in% colnames(total_value_consumed_by_category)) {
      indicator_data$value_crop_consumed_lcu_per_hh_per_year <- total_value_consumed_by_category$value_crop_consumed_lcu
    }

    if (any(c("value_meat_consumed_lcu", "value_eggs_consumed_lcu", "value_milk_consumed_lcu", "value_bees_honey_consumed_lcu") %in% colnames(total_value_consumed_by_category))) {
      indicator_data$value_livestock_products_consumed_lcu_per_hh_per_year <- rowSums(total_value_consumed_by_category[colnames(total_value_consumed_by_category) %in%
        c(
          "value_meat_consumed_lcu",
          "value_eggs_consumed_lcu",
          "value_milk_consumed_lcu",
          "value_bees_honey_consumed_lcu"
        )],
      na.rm = T
      )
    }
    indicator_data$value_farm_products_consumed_lcu_per_hh_per_year <- rowSums(total_value_consumed_by_category, na.rm = T)
  }

  id_cols <- make_new_dataset(processed_data)
  extra_outputs <- lapply(extra_outputs, function(x) {
    dplyr::bind_cols(id_cols, x)
  })

  result$processed_data <- processed_data
  result$consumption_lcu_calues <- extra_outputs
  result$indicator_data <- indicator_data

  return(result)
}