



#' Calculate Livestock Price
#'
#' Calculate the prices of whole livestock sold
#'
#' @param data Dataset containing livestock sold and livestock sale income
#'
#' @return
#' @export
#'
#' @examples
price_per_livestock <- function(data) {
  number_of_loops <- find_number_of_loops(data, name_column = "livestock_sold")
  sold_columns <- paste0("livestock_sold", "_", c(1:number_of_loops))
  income_columns <- paste0("livestock_sale_income", "_", c(1:number_of_loops))
  price_columns <- paste0("livestock_price_per_animal", "_", c(1:number_of_loops))

  sold_data <- data[sold_columns]
  sold_data <- sold_data %>% dplyr::mutate_all(as.numeric)
  income_data <- data[income_columns]
  income_data <- income_data %>% dplyr::mutate_all(as.numeric)


  livestock_sale_prices <- income_data / sold_data
  colnames(livestock_sale_prices) <- price_columns

  data <- add_column_after_specific_column(data,
    new_data = livestock_sale_prices,
    new_column_name = "livestock_price_per_animal",
    old_column_name = "livestock_sale_income",
    loop_structure = T
  )

  return(data)
}


#' Meat amount calculations
#'
#' A function to calculate the amount of meat
#' produced based on the number of animals killed,
#' and some conversion factors which can calculate
#' how much meat can be collected from an animal which
#' has been killed.
#'
#' @param data The dataset for which meat amounts need to be calculated
#' @param unit_conv_tibble A conversion table for animal names and their weights
#' @return
#' @export
#'
#' @examples
meat_amount_calculation <- function(data,
                                    unit_conv_tibble = NULL) {
  if ("id_rhomis_dataset" %in% colnames(data) == F) {
    stop("Missing the id_rhomis_dataset column in RHoMIS data")
  }

  if (is.null(unit_conv_tibble)) {
    unit_conv_tibble <- make_per_project_conversion_tibble(
      proj_id_vector = data[["id_rhomis_dataset"]],
      unit_conv_tibble = livestock_weights
    )
  }

  number_of_loops <- find_number_of_loops(data, "livestock_name")
  livestock_name_columns <- paste0("livestock_name", "_", c(1:number_of_loops))
  animals_killed_columns <- paste0("killed_for_meat", "_", c(1:number_of_loops))

  livestock_name_data <- data[livestock_name_columns]
  killed_for_meat_data <- data[animals_killed_columns]
  killed_for_meat_data <- killed_for_meat_data %>% dplyr::mutate_all(as.numeric)


  livestock_weight_data <- switch_units(livestock_name_data,
    unit_tibble = unit_conv_tibble,
    id_vector = data[["id_rhomis_dataset"]]
  )


  meat_weight_kg <- livestock_weight_data * killed_for_meat_data
  colnames(meat_weight_kg) <- paste0("meat_kg_per_year", "_", c(1:number_of_loops))


  data <- add_column_after_specific_column(
    data = data,
    new_data = meat_weight_kg,
    new_column_name = "meat_kg_per_year",
    old_column_name = "killed_for_meat",
    loop_structure = T
  )


  return(data)
}

#' Meat Use Calculations
#'
#' Calculating the numeric proportions of meat used for
#' eating and selling
#'
#' @param data The RHoMIS data containing livestock loops
#'
#' @return
#' @export
#'
#' @examples
meat_uses <- function(data) {
  number_of_loops <- find_number_of_loops(data, "livestock_name")

  missing_columns <- check_columns_in_data(data,
    loop_columns = c(
      "meat_kg_per_year",
      "meat_sell_amount",
      "livestock_name"
    ),
    warning_message = "Could not calculate amounts of meat sold or consumed"
  )
  if (length(missing_columns) == 0) {
    meat_sold_props_numeric <- sapply(c(1:number_of_loops), function(x) proportions_calculation(data, use = "sell", use_column = "meat_use", prop_column = "meat_sell_amount", loop_number = x))
    colnames(meat_sold_props_numeric) <- paste0("meat_sold_props_numeric", "_", c(1:number_of_loops))
    meat_sold_props_numeric <- tibble::as_tibble(meat_sold_props_numeric)


    data <- add_column_after_specific_column(
      data = data,
      new_data = meat_sold_props_numeric,
      new_column_name = "meat_sold_props_numeric",
      old_column_name = "meat_sell_amount",
      loop_structure = T
    )
  }


  missing_columns <- check_columns_in_data(data,
    loop_columns = c(
      "meat_kg_per_year",
      "meat_consumed_amount",
      "livestock_name"
    ),
    warning_message = "Could not calculate amounts of meat sold or consumed"
  )
  if (length(missing_columns) == 0) {
    meat_consumed_props_numeric <- sapply(c(1:number_of_loops), function(x) proportions_calculation(data, use = "eat", use_column = "meat_use", prop_column = "meat_consumed_amount", loop_number = x))
    colnames(meat_consumed_props_numeric) <- paste0("meat_consumed_props_numeric", "_", c(1:number_of_loops))
    meat_consumed_props_numeric <- tibble::as_tibble(meat_consumed_props_numeric)
    data <- add_column_after_specific_column(
      data = data,
      new_data = meat_consumed_props_numeric,
      new_column_name = "meat_consumed_props_numeric",
      old_column_name = "meat_consumed_amount",
      loop_structure = T
    )
  }

  return(data)
}

#' Meat Sold and Consumed Calculation
#'
#' Calculate the amount of meat sold and consumed in kg.
#' Note this only works if you have calculated the meat collected
#' in kg.
#'
#' @param data Data containing "livestock_name", "meat_kg_per_year",
#' "meat_sold_props_numeric","meat_consumed_props_numeric".
#'
#' @return
#' @export
#'
#' @examples
meat_sold_and_consumed_calculation <- function(data) {
  data <- meat_uses(data)
  number_of_loops <- find_number_of_loops(data, name_column = "livestock_name")
  amount_columns <- paste0("meat_kg_per_year", "_", c(1:number_of_loops))
  sold_columns <- paste0("meat_sold_props_numeric", "_", c(1:number_of_loops))

  if (all(amount_columns %in% colnames(data)) == F | all(sold_columns %in% colnames(data)) == F) {
    warning("Have not calculated the amount of meat collected in kg or the proportions of meat sold. Calculate amounts collected before calculating amounts sold")
  }

  if (all(amount_columns %in% colnames(data)) == T & all(sold_columns %in% colnames(data)) == T) {
    meat_amount_data <- data[amount_columns]
    sold_prop_data <- data[sold_columns]

    amount_sold_kg <- tibble::as_tibble(meat_amount_data * sold_prop_data)
    colnames(amount_sold_kg) <- paste0("meat_sold_kg_per_year", "_", c(1:number_of_loops))

    data <- add_column_after_specific_column(
      data = data,
      new_data = amount_sold_kg,
      new_column_name = "meat_sold_kg_per_year",
      old_column_name = "meat_sold_props_numeric",
      loop_structure = T
    )
  }

  number_of_loops <- find_number_of_loops(data, name_column = "livestock_name")
  amount_columns <- paste0("meat_kg_per_year", "_", c(1:number_of_loops))
  consumed_columns <- paste0("meat_consumed_props_numeric", "_", c(1:number_of_loops))

  if (all(amount_columns %in% colnames(data)) == F | all(consumed_columns %in% colnames(data)) == F) {
    warning("Have not calculated the amount of meat collected in kg or the proportions of meat consumed. Calculate amounts collected before calculating amounts consumed")
  }

  if (all(amount_columns %in% colnames(data)) == T & all(consumed_columns %in% colnames(data)) == T) {
    meat_amount_data <- data[amount_columns]
    consumed_prop_data <- data[consumed_columns]

    amount_consumed_kg <- tibble::as_tibble(meat_amount_data * consumed_prop_data)
    colnames(amount_consumed_kg) <- paste0("meat_consumed_kg_per_year", "_", c(1:number_of_loops))

    data <- add_column_after_specific_column(
      data = data,
      new_data = amount_consumed_kg,
      new_column_name = "meat_consumed_kg_per_year",
      old_column_name = "meat_consumed_props_numeric",
      loop_structure = T
    )
  }

  return(data)
}

#' Meat price per kg
#'
#' Calculating the price of meat which was sold.
#'
#' @param data RHoMIS data including columns "meat_sold_kg_per_year" and "meat_sold_income".
#'
#' @return
#' @export
#'
#' @examples
meat_prices <- function(data) {
  number_of_loops <- find_number_of_loops(data, "meat_sold_income")
  sold_income_columns <- paste0("meat_sold_income", "_", c(1:number_of_loops))
  sold_amount_columns <- paste0("meat_sold_kg_per_year", "_", c(1:number_of_loops))
  price_columns <- paste0("meat_price_per_kg", "_", c(1:number_of_loops))

  sold_income_data <- data[sold_income_columns]
  sold_income_data <- sold_income_data %>% dplyr::mutate_all(as.numeric)

  sold_amount_data <- data[sold_amount_columns]
  sold_amount_data <- sold_amount_data %>% dplyr::mutate_all(as.numeric)




  price_data <- sold_income_data / sold_amount_data
  colnames(price_data) <- price_columns

  data <- add_column_after_specific_column(
    data = data,
    new_data = price_data,
    new_column_name = "meat_price_per_kg",
    old_column_name = "meat_sold_income",
    loop_structure = T
  )

  return(data)
}



#' Milk amount calculations
#'
#' Calculating the amount of milk collected each year
#'
#' @param data The dataset containing livestock loops for RHoMIS
#' @param unit_conv_tibble The units for milk amounts in a conversion tibble
#'
#' @return
#' @export
#'
#' @examples
milk_amount_calculations <- function(data,
                                     unit_conv_tibble = NULL) {
  if ("id_rhomis_dataset" %in% colnames(data) == F) {
    stop("Missing the id_rhomis_dataset column in RHoMIS data")
  }

  if (is.null(unit_conv_tibble)) {
    unit_conv_tibble <- make_per_project_conversion_tibble(
      proj_id_vector = data[["id_rhomis_dataset"]],
      unit_conv_tibble = milk_amount_units
    )
  }

  number_of_loops <- find_number_of_loops(data, "milk_amount_good_season")

  milk_amount_good_season_columns <- paste0("milk_amount_good_season", "_", c(1:number_of_loops))
  milk_amount_bad_season_columns <- paste0("milk_amount_bad_season", "_", c(1:number_of_loops))
  milk_units_columns <- paste0("milk_units", "_", c(1:number_of_loops))
  milk_number_of_animals_milked_columns <- paste0("milk_number_animals_milked", "_", c(1:number_of_loops))

  milk_amount_good_season_data <- data[milk_amount_good_season_columns]
  milk_amount_good_season_data <- milk_amount_good_season_data %>% dplyr::mutate_all(as.numeric)

  milk_amount_bad_season_data <- data[milk_amount_bad_season_columns]
  milk_amount_bad_season_data <- milk_amount_bad_season_data %>% dplyr::mutate_all(as.numeric)

  milk_units_data <- data[milk_units_columns]

  if (all(milk_number_of_animals_milked_columns %in% colnames(data)) == F) {
    empty_df <- matrix(NA, nrow = nrow(data), ncol = length(milk_number_of_animals_milked_columns))
    colnames(empty_df) <- milk_number_of_animals_milked_columns
    milk_number_of_animals_milked_data <- tibble::as_tibble(empty_df)
    colnames(milk_number_of_animals_milked_data) <- milk_number_of_animals_milked_columns
  }
  if (all(milk_number_of_animals_milked_columns %in% colnames(data))) {
    milk_number_of_animals_milked_data <- data[milk_number_of_animals_milked_columns]
  }

  # Changing units to numeric conversions
  milk_units_data <- switch_units(milk_units_data, unit_tibble = unit_conv_tibble, id_vector = data[["id_rhomis_dataset"]])
  milk_amount_conversions <- sapply(c(1:length(milk_units_data)), function(x) {
    milk_swap_per_animal_units(
      units_column = milk_units_data[[x]],
      number_of_animals_milked_column = milk_number_of_animals_milked_data[[x]]
    )
  })
  colnames(milk_amount_conversions) <- paste0("milk_amount_units_numeric", "_", c(1:number_of_loops))
  milk_amount_conversions <- tibble::as_tibble(milk_amount_conversions) %>% dplyr::mutate_all(as.numeric)
  milk_amount_conversions <- milk_amount_conversions %>% dplyr::mutate_all(as.numeric)

  data <- add_column_after_specific_column(
    data = data,
    new_data = milk_amount_conversions,
    new_column_name = "milk_amount_units_numeric",
    old_column_name = "milk_units",
    loop_structure = T
  )


  # Milk amounts good and bad season
  milk_amount_good_season_litres_per_year <- tibble::as_tibble(milk_amount_good_season_data * milk_amount_conversions)
  colnames(milk_amount_good_season_litres_per_year) <- paste0("milk_amount_good_season_litres_per_year", "_", c(1:number_of_loops))

  data <- add_column_after_specific_column(
    data = data,
    new_data = milk_amount_good_season_litres_per_year,
    new_column_name = "milk_amount_good_season_litres_per_year",
    old_column_name = "milk_amount_good_season",
    loop_structure = T
  )

  milk_amount_bad_season_litres_per_year <- tibble::as_tibble(milk_amount_bad_season_data * milk_amount_conversions)
  colnames(milk_amount_bad_season_litres_per_year) <- paste0("milk_amount_bad_season_litres_per_year", "_", c(1:number_of_loops))

  data <- add_column_after_specific_column(
    data = data,
    new_data = milk_amount_bad_season_litres_per_year,
    new_column_name = "milk_amount_bad_season_litres_per_year",
    old_column_name = "milk_amount_bad_season",
    loop_structure = T
  )
  # Averaging for milk collected per year
  milk_collected_litres_per_year <- sapply(c(1:length(milk_amount_good_season_litres_per_year)), function(x) {
    average_good_and_bad_season(good_season_amount = milk_amount_good_season_litres_per_year[[x]], bad_season_amount = milk_amount_bad_season_litres_per_year[[x]])
  })
  colnames(milk_collected_litres_per_year) <- paste0("milk_collected_litres_per_year", "_", c(1:number_of_loops))
  milk_collected_litres_per_year <- tibble::as_tibble(milk_collected_litres_per_year)

  data <- add_column_after_specific_column(
    data = data,
    new_data = milk_collected_litres_per_year,
    new_column_name = "milk_collected_litres_per_year",
    old_column_name = "milk_amount_good_season_litres_per_year",
    loop_structure = T
  )

  return(data)
}


#' Milk Proportions All
#'
#' A function to calculate the numeric proportions of milk
#' sold and milk consumed
#'
#' @param data Data containing livestock loops necessary to calculate milk sold and consumed proportions
#'
#' @return
#' @export
#'
#' @examples
milk_proportions_all <- function(data) {
  number_of_loops <- find_number_of_loops(data, name_column = "livestock_name")

  if (all(paste0("milk_consumed_amount_", 1:number_of_loops) %in% colnames(data))) {
    milk_consumed_proportions_numeric <- sapply(c(1:number_of_loops), function(x) proportions_calculation(data, use = "use", use_column = "milk_use", prop_column = "milk_consumed_amount", loop_number = x))
    colnames(milk_consumed_proportions_numeric) <- paste0("milk_consumed_prop_numeric", "_", c(1:number_of_loops))
    milk_consumed_proportions_numeric <- tibble::as_tibble(milk_consumed_proportions_numeric)
    data <- add_column_after_specific_column(
      data = data,
      new_data = milk_consumed_proportions_numeric,
      new_column_name = "milk_consumed_prop_numeric",
      old_column_name = "milk_consumed_amount",
      loop_structure = T
    )
  }


  if (all(paste0("milk_sell_amount_", 1:number_of_loops) %in% colnames(data))) {
    milk_sold_proportions_numeric <- sapply(c(1:number_of_loops), function(x) proportions_calculation(data, use = "sell", use_column = "milk_use", prop_column = "milk_sell_amount", loop_number = x))
    colnames(milk_sold_proportions_numeric) <- paste0("milk_sold_prop_numeric", "_", c(1:number_of_loops))
    milk_sold_proportions_numeric <- tibble::as_tibble(milk_sold_proportions_numeric)
    data <- add_column_after_specific_column(
      data = data,
      new_data = milk_sold_proportions_numeric,
      new_column_name = "milk_sold_prop_numeric",
      old_column_name = "milk_sell_amount",
      loop_structure = T
    )
  }

  return(data)
}

#' Milk Sold and Consumed
#'
#' Function to calculate the amounts of milk sold
#' and consumed in litres.
#'
#' @param data RHoMIS data including livestock loop information
#'
#' @return
#' @export
#'
#' @examples
milk_sold_and_consumed_calculations <- function(data) {
  data <- milk_proportions_all(data)
  number_of_loops <- find_number_of_loops(data, name_column = "milk_sell_amount")
  amount_columns <- paste0("milk_collected_litres_per_year", "_", c(1:number_of_loops))
  sold_columns <- paste0("milk_sold_prop_numeric", "_", c(1:number_of_loops))

  if (all(amount_columns %in% colnames(data)) == F | all(sold_columns %in% colnames(data)) == F) {
    warning("Have not calculated the amount of milk collected or amount of milk sold in litres Calculate amounts collected before calculating amounts sold")
  }
  if (all(amount_columns %in% colnames(data)) == T & all(sold_columns %in% colnames(data)) == T) {
    milk_amount_data <- data[amount_columns]
    sold_prop_data <- data[sold_columns]

    amount_sold__litres <- tibble::as_tibble(milk_amount_data * sold_prop_data)
    colnames(amount_sold__litres) <- paste0("milk_sold_litres_per_year", "_", c(1:number_of_loops))

    data <- add_column_after_specific_column(
      data = data,
      new_data = amount_sold__litres,
      new_column_name = "milk_sold_litres_per_year",
      old_column_name = "milk_sold_prop_numeric",
      loop_structure = T
    )
  }
  number_of_loops <- find_number_of_loops(data, name_column = "milk_consumed_amount")
  amount_columns <- paste0("milk_collected_litres_per_year", "_", c(1:number_of_loops))
  consumed_columns <- paste0("milk_consumed_prop_numeric", "_", c(1:number_of_loops))

  if (all(amount_columns %in% colnames(data)) == F | all(consumed_columns %in% colnames(data)) == F) {
    warning("Have not calculated the amount of milk collected or amount of milk consumed in litres Calculate amounts collected before calculating amounts sold")
  }
  if (all(amount_columns %in% colnames(data)) == T & all(consumed_columns %in% colnames(data)) == T) {
    milk_amount_data <- data[amount_columns]
    consumed_prop_data <- data[consumed_columns]

    amount_consumed_litres <- tibble::as_tibble(milk_amount_data * consumed_prop_data)
    colnames(amount_consumed_litres) <- paste0("milk_consumed_litres_per_year", "_", c(1:number_of_loops))

    data <- add_column_after_specific_column(
      data = data,
      new_data = amount_consumed_litres,
      new_column_name = "milk_consumed_litres_per_year",
      old_column_name = "milk_consumed_prop_numeric",
      loop_structure = T
    )
  }

  return(data)
}

#' Milk Income Calculations
#'
#' @param data Dataset containing all milk income information
#' @param unit_conv_tibble A tibble containing all of the milk income conversion factors
#'
#' @return
#' @export
#'
#' @examples
milk_income_calculations <- function(data, unit_conv_tibble = NULL) {
  if ("id_rhomis_dataset" %in% colnames(data) == F) {
    stop("Missing the id_rhomis_dataset column in RHoMIS data")
  }

  if (is.null(unit_conv_tibble)) {
    unit_conv_tibble <- make_per_project_conversion_tibble(
      proj_id_vector = data[["id_rhomis_dataset"]],
      unit_conv_tibble = milk_price_time_units
    )
  }

  number_of_loops <- find_number_of_loops(data, "milk_sold_price_timeunits")

  milk_price_units_columns <- paste0("milk_sold_price_timeunits", "_", c(1:number_of_loops))
  milk_sold_income_columns <- paste0("milk_sold_income", "_", c(1:number_of_loops))
  milk_sold_amount_columns <- paste0("milk_sold_litres_per_year", "_", c(1:number_of_loops))

  milk_price_unit_data <- data[milk_price_units_columns]
  milk_sold_income_data <- data[milk_sold_income_columns]
  milk_sold_income_data <- milk_sold_income_data %>% dplyr::mutate_all(as.numeric)
  milk_sold_amount_data <- data[milk_sold_amount_columns]
  milk_sold_amount_data <- milk_sold_amount_data %>% dplyr::mutate_all(as.numeric)

  milk_income_conversions <- sapply(c(1:number_of_loops), function(x) {
    milk_price_time_units_conversion(
      id_rhomis_dataset = data[["id_rhomis_dataset"]],
      units_column = milk_price_unit_data[[x]],
      sold_amount_column = milk_sold_amount_data[[x]],
      unit_conv_tibble = unit_conv_tibble
    )
  })
  colnames(milk_income_conversions) <- paste0("milk_income_conversions", "_", c(1:number_of_loops))
  milk_income_conversions <- tibble::as_tibble(milk_income_conversions)

  milk_income_per_year <- tibble::as_tibble(milk_sold_income_data * milk_income_conversions)
  colnames(milk_income_per_year) <- paste0("milk_sold_income_per_year", "_", c(1:number_of_loops))

  data <- add_column_after_specific_column(
    data = data,
    new_data = milk_income_per_year,
    new_column_name = "milk_sold_income_per_year",
    old_column_name = "milk_sold_income",
    loop_structure = T
  )

  milk_price_columns <- paste0("milk_price_per_litre", "_", c(1:number_of_loops))
  milk_price_per_litre <- tibble::as_tibble(milk_income_per_year / milk_sold_amount_data)
  colnames(milk_price_per_litre) <- milk_price_columns
  data <- add_column_after_specific_column(
    data = data,
    new_data = milk_price_per_litre,
    new_column_name = "milk_price_per_litre",
    old_column_name = "milk_sold_income_per_year",
    loop_structure = T
  )

  return(data)
}


#' EggsAmount Calculations
#'
#' Calculate the amount of eggs harvested
#'
#' @param data Data containing crop loop information
#' @param unit_conv_tibble A tibble of units and conversion factor, and project IDs
#'
#' @return
#' @export
#'
#' @examples
eggs_amount_calculations <- function(data, unit_conv_tibble = NULL) {
  if ("id_rhomis_dataset" %in% colnames(data) == F) {
    stop("Missing the id_rhomis_dataset column in RHoMIS data")
  }

  if (is.null(unit_conv_tibble)) {
    unit_conv_tibble <- make_per_project_conversion_tibble(
      proj_id_vector = data[["id_rhomis_dataset"]],
      unit_conv_tibble = eggs_amount_units
    )
  }

  egg_weight_kg <- 0.0496
  number_of_loops <- find_number_of_loops(data, "eggs_amount_good")

  eggs_amount_good_season_columns <- paste0("eggs_amount_good", "_", c(1:number_of_loops))
  eggs_amount_bad_season_columns <- paste0("eggs_amount_bad", "_", c(1:number_of_loops))
  eggs_units_columns <- paste0("eggs_units", "_", c(1:number_of_loops))
  livestock_heads_columns <- grep("livestock_heads", colnames(data), value = T)
  livestock_names_columns <- paste0("livestock_name", "_", c(1:number_of_loops))


  eggs_amount_good_season_data <- data[eggs_amount_good_season_columns]
  eggs_amount_good_season_data <- eggs_amount_good_season_data %>% dplyr::mutate_all(as.numeric)
  eggs_amount_bad_season_data <- data[eggs_amount_bad_season_columns]
  eggs_amount_bad_season_data <- eggs_amount_bad_season_data %>% dplyr::mutate_all(as.numeric)

  eggs_units_data <- data[eggs_units_columns]
  livestock_heads_data <- data[livestock_heads_columns]
  livestock_names_data <- data[livestock_names_columns]

  # Changing units to numeric conversions
  eggs_units_data <- switch_units(eggs_units_data, unit_tibble = unit_conv_tibble, id_vector = data[["id_rhomis_dataset"]])
  eggs_amount_conversions <- sapply(c(1:length(eggs_units_data)), function(x) {
    eggs_swap_per_animal_units(
      units_column = eggs_units_data[[x]],
      livestock_name_column = livestock_names_data[[x]],
      livestock_heads_df = livestock_heads_data
    )
  }) %>%
    magrittr::set_colnames(paste0("eggs_amount_units_numeric", "_", c(1:number_of_loops))) %>%
    tibble::as_tibble() %>%
    dplyr::mutate_all(as.numeric)

  eggs_amount_conversions_kg <- eggs_amount_conversions * egg_weight_kg

  # eggs amounts good and bad season
  eggs_amount_good_season_kg_per_year <- tibble::as_tibble(eggs_amount_good_season_data * eggs_amount_conversions_kg)
  colnames(eggs_amount_good_season_kg_per_year) <- paste0("eggs_amount_good_season_kg_per_year", "_", c(1:number_of_loops))

  data <- add_column_after_specific_column(
    data = data,
    new_data = eggs_amount_good_season_kg_per_year,
    new_column_name = "eggs_amount_good_season_kg_per_year",
    old_column_name = "eggs_amount_good",
    loop_structure = T
  )

  eggs_amount_bad_season_kg_per_year <- tibble::as_tibble(eggs_amount_bad_season_data * eggs_amount_conversions_kg)
  colnames(eggs_amount_bad_season_kg_per_year) <- paste0("eggs_amount_bad_season_kg_per_year", "_", c(1:number_of_loops))

  data <- add_column_after_specific_column(
    data = data,
    new_data = eggs_amount_bad_season_kg_per_year,
    new_column_name = "eggs_amount_bad_season_kg_per_year",
    old_column_name = "eggs_amount_bad",
    loop_structure = T
  )
  # Averaging for eggs collected per year
  eggs_collected_kg_per_year <- sapply(c(1:length(eggs_amount_good_season_kg_per_year)), function(x) {
    average_good_and_bad_season(good_season_amount = eggs_amount_good_season_kg_per_year[[x]], bad_season_amount = eggs_amount_bad_season_kg_per_year[[x]])
  })
  colnames(eggs_collected_kg_per_year) <- paste0("eggs_collected_kg_per_year", "_", c(1:number_of_loops))
  eggs_collected_kg_per_year <- tibble::as_tibble(eggs_collected_kg_per_year)

  data <- add_column_after_specific_column(
    data = data,
    new_data = eggs_collected_kg_per_year,
    new_column_name = "eggs_collected_kg_per_year",
    old_column_name = "eggs_amount_bad_season_kg_per_year",
    loop_structure = T
  )

  return(data)
}

#' Eggs Proportions All
#'
#' A function for calculating the proportions of
#' eggs and consumed
#'
#' @param data The RHoMIS data including livestock loops and
#' livestock heads data
#'
#' @return
#' @export
#'
#' @examples
eggs_proportions_all <- function(data) {
  number_of_loops <- find_number_of_loops(data, name_column = "livestock_name")

  missing_columns <- check_columns_in_data(data,
    loop_columns = c(
      "eggs_consumed_amount",
      "livestock_name"
    ),
    warning_message = "Could not calculate amounts of eggs sold and consumed"
  )
  if (length(missing_columns) == 0) {
    egg_consumed_proportions_numeric <- sapply(c(1:number_of_loops), function(x) proportions_calculation(data, use = "use", use_column = "eggs_use", prop_column = "eggs_consumed_amount", loop_number = x))
    colnames(egg_consumed_proportions_numeric) <- paste0("eggs_consumed_prop_numeric", "_", c(1:number_of_loops))
    egg_consumed_proportions_numeric <- tibble::as_tibble(egg_consumed_proportions_numeric)
    data <- add_column_after_specific_column(
      data = data,
      new_data = egg_consumed_proportions_numeric,
      new_column_name = "eggs_consumed_prop_numeric",
      old_column_name = "eggs_consumed_amount",
      loop_structure = T
    )
  }

  missing_columns <- check_columns_in_data(data,
    loop_columns = c(
      "eggs_sell_amount",
      "livestock_name"
    ),
    warning_message = "Could not calculate amounts of eggs sold and consumed"
  )
  if (length(missing_columns) == 0) {
    egg_sold_proportions_numeric <- sapply(c(1:number_of_loops), function(x) proportions_calculation(data, use = "sell", use_column = "eggs_use", prop_column = "eggs_sell_amount", loop_number = x))
    colnames(egg_sold_proportions_numeric) <- paste0("eggs_sold_prop_numeric", "_", c(1:number_of_loops))
    egg_sold_proportions_numeric <- tibble::as_tibble(egg_sold_proportions_numeric)
    data <- add_column_after_specific_column(
      data = data,
      new_data = egg_sold_proportions_numeric,
      new_column_name = "eggs_sold_prop_numeric",
      old_column_name = "eggs_sell_amount",
      loop_structure = T
    )
  }

  return(data)
}



#' Eggs Sold and Consumed Calculations
#'
#' Function for calculating the amounts of eggs sold and consumed
#'
#' @param data Data containing livestock loops to calculate the amounts of
#' egg sold and consumed
#'
#' @return
#' @export
#'
#' @examples
eggs_sold_and_consumed_calculations <- function(data) {
  data <- eggs_proportions_all(data)
  number_of_loops <- find_number_of_loops(data, name_column = "eggs_sell_amount")
  amount_columns <- paste0("eggs_collected_kg_per_year", "_", c(1:number_of_loops))
  sold_columns <- paste0("eggs_sold_prop_numeric", "_", c(1:number_of_loops))

  if (all(amount_columns %in% colnames(data)) == F | all(sold_columns %in% colnames(data)) == F) {
    warning("Have not calculated the amount of eggs collected in kg or amounts sold. Calculate amounts collected before calculating amounts sold")
  }
  if (all(amount_columns %in% colnames(data)) == T & all(sold_columns %in% colnames(data)) == T) {
    eggs_amount_data <- data[amount_columns]
    sold_prop_data <- data[sold_columns]

    amount_sold_kg <- tibble::as_tibble(eggs_amount_data * sold_prop_data)
    colnames(amount_sold_kg) <- paste0("eggs_sold_kg_per_year", "_", c(1:number_of_loops))

    data <- add_column_after_specific_column(
      data = data,
      new_data = amount_sold_kg,
      new_column_name = "eggs_sold_kg_per_year",
      old_column_name = "eggs_sold_prop_numeric",
      loop_structure = T
    )
  }
  number_of_loops <- find_number_of_loops(data, name_column = "eggs_consumed_amount")
  amount_columns <- paste0("eggs_collected_kg_per_year", "_", c(1:number_of_loops))
  consumed_columns <- paste0("eggs_consumed_prop_numeric", "_", c(1:number_of_loops))

  if (all(amount_columns %in% colnames(data)) == F | all(consumed_columns %in% colnames(data)) == F) {
    warning("Have not calculated the amount of eggs collected in kg or amounts sold. Calculate amounts collected before calculating amounts sold")
  }
  if (all(amount_columns %in% colnames(data)) == T & all(consumed_columns %in% colnames(data)) == T) {
    eggs_amount_data <- data[amount_columns]
    consumed_prop_data <- data[consumed_columns]

    amount_consumed_kg <- tibble::as_tibble(eggs_amount_data * consumed_prop_data)
    colnames(amount_consumed_kg) <- paste0("eggs_consumed_kg_per_year", "_", c(1:number_of_loops))

    data <- add_column_after_specific_column(
      data = data,
      new_data = amount_consumed_kg,
      new_column_name = "eggs_consumed_kg_per_year",
      old_column_name = "eggs_consumed_prop_numeric",
      loop_structure = T
    )
  }
  return(data)
}


#' Eggs Income Calculations
#'
#' Function to calculate egg income from livestock loops
#'
#' @param data RHoMIS data with livestock loops included.
#' @param unit_conv_tibble A tibble containing common su
#'
#' @return
#' @export
#'
#' @examples
egg_income_calculations <- function(data,
                                    unit_conv_tibble = NULL) {
  if ("id_rhomis_dataset" %in% colnames(data) == F) {
    stop("Missing the id_rhomis_dataset column in RHoMIS data")
  }

  if (is.null(unit_conv_tibble)) {
    unit_conv_tibble <- make_per_project_conversion_tibble(
      proj_id_vector = data[["id_rhomis_dataset"]],
      unit_conv_tibble = eggs_price_time_units
    )
  }


  number_of_loops <- find_number_of_loops(data, "eggs_sold_income")

  income_columns <- paste0("eggs_sold_income", "_", paste0(1:number_of_loops))
  amount_sold_columns <- paste0("eggs_sold_kg_per_year", "_", paste0(1:number_of_loops))
  income_units_columns <- paste0("eggs_sold_price_timeunits", "_", paste0(1:number_of_loops))

  income_data <- data[income_columns]
  income_data <- income_data %>% dplyr::mutate_all(as.numeric)

  if (all(amount_sold_columns %in% colnames(data))) {
    amount_sold_data <- data[amount_sold_columns]
    amount_sold_data <- amount_sold_data %>% dplyr::mutate_all(as.numeric)
  }
  income_units_data <- data[income_units_columns]

  units_converted <- switch_units(income_units_data, unit_tibble = unit_conv_tibble, id_vector = data[["id_rhomis_dataset"]])
  if (all(amount_sold_columns %in% colnames(data))) {
    units_converted <- sapply(c(1:number_of_loops), function(x) {
      eggs_price_per_egg_to_numeric(units_column = units_converted[[x]], amount_sold_column = amount_sold_data[[x]])
    }) %>%
      magrittr::set_colnames(paste0("eggs_price_units_numeric", "_", c(1:number_of_loops))) %>%
      tibble::as_tibble()
  }

  units_converted <- units_converted %>% dplyr::mutate_all(as.numeric)
  total_income <- units_converted * income_data %>%
    tibble::as_tibble()
  colnames(total_income) <- paste0("eggs_income_per_year", "_", c(1:number_of_loops))

  data <- add_column_after_specific_column(
    data = data,
    new_data = total_income,
    new_column_name = "eggs_income_per_year",
    old_column_name = "eggs_sold_price_timeunits",
    loop_structure = T
  )

  if (all(amount_sold_columns %in% colnames(data)) == F) {
    warning("Could not calculate egg prices, missing information on the amount of eggs sold")
  }


  if (all(amount_sold_columns %in% colnames(data))) {
    price_data <- total_income / amount_sold_data %>% tibble::as_tibble()
    colnames(price_data) <- paste0("eggs_price_per_kg", "_", c(1:number_of_loops))
    data <- add_column_after_specific_column(
      data = data,
      new_data = price_data,
      new_column_name = "eggs_price_per_kg",
      old_column_name = "eggs_income_per_year",
      loop_structure = T
    )
  }

  return(data)
}

#' Eggs Price per Egg to Numeric
#'
#' A function to make sure that price per egg can be converted
#' for the correct calculations to be made
#'
#' @param units_column A vector of crop units to convert
#' @param amount_sold_column A vector of amounts of eggs sold (in kg)
#'
#' @return
#' @export
#'
#' @examples
eggs_price_per_egg_to_numeric <- function(units_column, amount_sold_column) {
  egg_weight_kg <- 0.0496


  per_egg_units <- units_column == "per_egg"
  per_egg_units[is.na(per_egg_units)] <- FALSE

  # egg_income <- eggs_kg*price_per_kg
  # price_per_kg <- price_per_egg/weight_of_egg
  # egg_income <- eggs_kg*price_per_egg/weight_per_egg

  units_column[per_egg_units] <- amount_sold_column[per_egg_units] / egg_weight_kg

  return(units_column)
}



#' Eggs Swap per Animal Units
#'
#' @param units_column A vector containing the units to be converted
#' @param livestock_name_column A vector of livestock names
#' @param livestock_heads_df A tibble of number of livestock heads
#' with column names "livestock_heads_cattle","livestock_heads_chicken" etc...
#'
#' @return
#' @export
#'
#' @examples
eggs_swap_per_animal_units <- function(units_column, livestock_name_column, livestock_heads_df) {
  number_of_heads <- identify_number_of_heads_for_livestock_loops(livestock_name_column, livestock_heads_df)
  number_of_heads <- as.numeric(number_of_heads)

  subset_using_per_animal_per_day <- units_column == "pieces/animal/day"
  subset_using_per_animal_per_day[is.na(subset_using_per_animal_per_day)] <- FALSE
  units_column[subset_using_per_animal_per_day] <- 365 * number_of_heads[subset_using_per_animal_per_day]

  return(units_column)
}


#' Identify Number of Livestock Heads in Loops
#'
#' Livestock heads information is collected seperately
#' the livestock loops. Howver in some cases, for example in
#' the eggs_amount calculations, it is useful to know the livestock heads
#' during the loops. This function allows us to do so
#'
#' @param livestock_name_column A vector of livestock names
#' @param livestock_heads_df A tibble of livestock head nunbers
#'
#' @return
#' @export
#'
#' @examples
identify_number_of_heads_for_livestock_loops <- function(livestock_name_column, livestock_heads_df) {
  colnames(livestock_heads_df) <- gsub("livestock_heads_", "", colnames(livestock_heads_df))

  number_of_animals <- sapply(c(1:length(livestock_name_column)), function(x) {
    if (livestock_name_column[x] %in% colnames(livestock_heads_df)) {
      return(livestock_heads_df[x, livestock_name_column[x]])
    } else {
      return(NA)
    }
  }) %>%
    unlist() %>%
    unname()

  return(number_of_animals)
}


#' Milk Swap Per Animal Units
#'
#' Some of RHoMIS milk yields are based on yield per animal.
#' This function converts yield per animal into a total milk yield.
#'
#'
#' @param units_column The original column containing milk yield units
#' @param number_of_animals_milked_column A column containing the number of animals
#' milked.
#'
#' @return
#' @export
#'
#' @examples
milk_swap_per_animal_units <- function(units_column, number_of_animals_milked_column) {
  number_of_animals_milked_column <- as.numeric(number_of_animals_milked_column)
  units_column[which(units_column == "l/animal/day")] <- round(365 * number_of_animals_milked_column[which(units_column == "l/animal/day")], 2)
  units_column[which(units_column == "per animal per week")] <- round((365 / 7) * number_of_animals_milked_column[which(units_column == "per animal per week")], 2)
  units_column[which(units_column == "0.3l/animal/day")] <- round((0.3 * 365) * number_of_animals_milked_column[which(units_column == "0.3l/animal/day")], 2)

  return(units_column)
}

#' Milk Price Time Units Conversion
#'
#' RHoMIS milk price units can come in both times and per
#' litre units. This function goes
#'
#' @param id_rhomis_dataset An id vector containing information on which rhomis
#' @param units_column The columns containing the original units
#' @param sold_amount_column The amounts of milk which was sold
#' @param unit_conv_tibble A table with all of the information to convert price time units
#'
#' @return
#' @export
#'
#' @examples
milk_price_time_units_conversion <- function(id_rhomis_dataset, units_column, sold_amount_column, unit_conv_tibble = NULL) {
  if (is.null(unit_conv_tibble)) {
    unit_conv_tibble <- make_per_project_conversion_tibble(
      proj_id_vector = id_rhomis_dataset,
      unit_conv_tibble = milk_price_time_units
    )
  }

  units_column <- switch_units(data_to_convert = units_column, unit_tibble = unit_conv_tibble, id_vector = id_rhomis_dataset)

  numeric_values <- suppressWarnings(!is.na(as.numeric(units_column)))

  converion_values <- units_column
  converion_values[numeric_values] <- (1 / as.numeric(units_column[numeric_values])) * sold_amount_column[numeric_values]

  converion_values[which(converion_values == "day")] <- 365
  converion_values[which(converion_values == "week")] <- 365 / 7
  converion_values[which(converion_values == "month")] <- 365 / 28
  converion_values[which(converion_values == "year")] <- 1

  converion_values <- suppressWarnings(as.numeric(converion_values))

  return(converion_values)
}


#' Calculate Average Milk Harvested Values
#'
#' Averaging yield for the good season and bad season
#' Where one season is NA, we only use information for the season available.
#' Where both seasons are NA, the final is calculated as NA
#'
#' @param good_season_amount Vector of good season yields
#' @param bad_season_amount Vector of bad season yields
#'
#' @return
#' @export
#'
#' @examples
average_good_and_bad_season <- function(good_season_amount, bad_season_amount) {
  sum_data <- tibble::as_tibble(list(
    good_season_amount = good_season_amount,
    bad_season_amount = bad_season_amount
  ))
  average_values <- rowMeans(sum_data, na.rm = T)
  average_values[is.na(average_values)] <- NA

  return(average_values)
}


#' Honey Amount Calculation
#'
#' Calculating the amount of honey produced from RHoMIS data
#'
#' @param data The data containing livestock loops
#' @param unit_conv_tibble A tibble of honey amount conversions
#'
#' @return
#' @export
#'
#' @examples
honey_amount_calculation <- function(data, unit_conv_tibble = NULL) {
  if ("id_rhomis_dataset" %in% colnames(data) == F) {
    stop("Missing the id_rhomis_dataset column in RHoMIS data")
  }

  if (is.null(unit_conv_tibble)) {
    unit_conv_tibble <- make_per_project_conversion_tibble(
      proj_id_vector = data[["id_rhomis_dataset"]],
      unit_conv_tibble = honey_amount_units
    )
  }

  number_of_loops <- find_number_of_loops(data, "bees_honey_production")

  honey_amount_columns <- paste0("bees_honey_production", "_", c(1:number_of_loops))
  honey_units_columns <- paste0("bees_honey_production_units", "_", c(1:number_of_loops))


  honey_amount_data <- data[honey_amount_columns]
  honey_amount_data <- honey_amount_data %>% dplyr::mutate_all(as.numeric)
  honey_units_data <- data[honey_units_columns]

  honey_units_converted <- switch_units(honey_units_data, unit_tibble = unit_conv_tibble, id_vector = data[["id_rhomis_dataset"]])

  bees_honey_kg_per_year <- honey_amount_data * honey_units_converted
  bees_honey_kg_per_year <- tibble::as_tibble(bees_honey_kg_per_year)
  colnames(bees_honey_kg_per_year) <- paste0("bees_honey_kg_per_year", "_", c(1:number_of_loops))

  data <- add_column_after_specific_column(
    data = data,
    new_data = bees_honey_kg_per_year,
    new_column_name = "bees_honey_kg_per_year",
    old_column_name = "bees_honey_production_units",
    loop_structure = T
  )

  return(data)
}


#' Honey Proportions All
#'
#' Can correctly calculate the numeric proportions of honey
#' and its uses
#'
#' @param data The data containing livestock loops
#'
#' @return
#' @export
#'
#' @examples
honey_proportions_all <- function(data) {
  number_of_loops <- find_number_of_loops(data, "livestock_name")

  missing_columns <- check_columns_in_data(data,
    loop_columns = c(
      "bees_honey_sell_amount"
    ),
    warning_message = "Could not calculate honey amounts sold or consumed"
  )
  if (length(missing_columns) == 0) {
    bees_honey_sold_props_numeric <- sapply(c(1:number_of_loops), function(x) proportions_calculation(data, use = "sell", use_column = "bees_honey_use", prop_column = "bees_honey_sell_amount", loop_number = x))
    colnames(bees_honey_sold_props_numeric) <- paste0("bees_honey_sold_props_numeric", "_", c(1:number_of_loops))
    bees_honey_sold_props_numeric <- tibble::as_tibble(bees_honey_sold_props_numeric)

    data <- add_column_after_specific_column(
      data = data,
      new_data = bees_honey_sold_props_numeric,
      new_column_name = "bees_honey_sold_props_numeric",
      old_column_name = "bees_honey_sell_amount",
      loop_structure = T
    )
  }

  missing_columns <- check_columns_in_data(data,
    loop_columns = c(
      "bees_honey_consumed_amount"
    ),
    warning_message = "Could not calculate honey amounts sold or consumed"
  )
  if (length(missing_columns) == 0) {
    bees_honey_consumed_props_numeric <- sapply(c(1:number_of_loops), function(x) proportions_calculation(data, use = "eat", use_column = "bees_honey_use", prop_column = "bees_honey_consumed_amount", loop_number = x))
    colnames(bees_honey_consumed_props_numeric) <- paste0("bees_honey_consumed_props_numeric", "_", c(1:number_of_loops))
    bees_honey_consumed_props_numeric <- tibble::as_tibble(bees_honey_consumed_props_numeric)
    data <- add_column_after_specific_column(
      data = data,
      new_data = bees_honey_consumed_props_numeric,
      new_column_name = "bees_honey_consumed_props_numeric",
      old_column_name = "bees_honey_consumed_amount",
      loop_structure = T
    )
  }
  return(data)
}

#' Honey Sold and Consumed Calculations
#'
#' Calculating the amounts of honey sold and consumed
#'
#' @param data Data containing RHoMIS
#' livestock loops
#'
#' @return
#' @export
#'
#' @examples
honey_amount_sold_and_consumed_calculations <- function(data) {
  data <- honey_proportions_all(data)
  # Beginning with honey sold

  number_of_loops <- find_number_of_loops(data, name_column = "bees_honey_production")
  amount_columns <- paste0("bees_honey_kg_per_year", "_", c(1:number_of_loops))
  sold_columns <- paste0("bees_honey_sold_props_numeric", "_", c(1:number_of_loops))

  if (all(amount_columns %in% colnames(data)) == F | all(sold_columns %in% colnames(data)) == F) {
    warning("Have not calculated the amounts collected in kg or amounts sold. Calculate amounts collected before calculating amounts sold")
  }
  if (all(amount_columns %in% colnames(data)) == T & all(sold_columns %in% colnames(data)) == T) {
    amounts_data <- data[amount_columns]
    sold_prop_data <- data[sold_columns]

    amount_sold_kg <- tibble::as_tibble(amounts_data * sold_prop_data)
    colnames(amount_sold_kg) <- paste0("bees_honey_sold_kg_per_year", "_", c(1:number_of_loops))

    data <- add_column_after_specific_column(
      data = data,
      new_data = amount_sold_kg,
      new_column_name = "bees_honey_sold_kg_per_year",
      old_column_name = "bees_honey_sold_props_numeric",
      loop_structure = T
    )
  }
  # Moving on to crops consumed
  number_of_loops <- find_number_of_loops(data, name_column = "bees_honey_production")
  amount_columns <- paste0("bees_honey_kg_per_year", "_", c(1:number_of_loops))
  consumed_columns <- paste0("bees_honey_consumed_props_numeric", "_", c(1:number_of_loops))

  if (all(amount_columns %in% colnames(data)) == F | all(consumed_columns %in% colnames(data)) == F) {
    warning("Have not calculated the amounts collected in kg or amounts sold. Calculate amounts collected before calculating amounts sold")
  }
  if (all(amount_columns %in% colnames(data)) == T & all(consumed_columns %in% colnames(data)) == T) {
    amounts_data <- data[amount_columns]
    consumed_prop_data <- data[consumed_columns]

    amount_consumed_kg <- tibble::as_tibble(amounts_data * consumed_prop_data)
    colnames(amount_consumed_kg) <- paste0("bees_honey_consumed_kg_per_year", "_", c(1:number_of_loops))

    data <- add_column_after_specific_column(
      data = data,
      new_data = amount_consumed_kg,
      new_column_name = "bees_honey_consumed_kg_per_year",
      old_column_name = "bees_honey_consumed_props_numeric",
      loop_structure = T
    )
  }

  return(data)
}

#' Gender Split of Livestock Information
#'
#' Whole livestock, and products produced from livestock
#' are divided among male and female farmers. This function
#' determines how these values are split
#'
#' @param data RHoMIS data including information on number
#' of livestock sold and who sells this livestock
#' @param gender_categories The categories you are interested in examining
#'
#' @return
#' @export
#'
#' @examples
gender_split_livestock <- function(data,
                                   gender_categories = pkg.env$gender_categories) {
  # Gender split whole livestock

  missing_columns <- check_columns_in_data(data,
    loop_columns = c(
      "livestock_sale_income",
      "livestock_who_sells"
    ),
    warning_message = "Could not gender_split for livestock sale incomes"
  )
  if (length(missing_columns) == 0) {
    data <- insert_gender_columns_in_core_data(
      data = data,
      original_column = "livestock_sale_income",
      control_column = "livestock_who_sells",
      loop_structure = T, gender_control_categories = gender_categories
    )
  }

  # Gender split meat
  missing_columns <- check_columns_in_data(data,
    loop_columns = c(
      "meat_sold_income",
      "livestock_meat_who_sells"
    ),
    warning_message = "Could not gender_split for meat sale incomes"
  )
  if (length(missing_columns) == 0) {
    data <- insert_gender_columns_in_core_data(
      data = data,
      original_column = "meat_sold_income",
      control_column = "livestock_meat_who_sells",
      loop_structure = T, gender_control_categories = gender_categories
    )
  }

  missing_columns <- check_columns_in_data(data,
    loop_columns = c(
      "meat_sold_kg_per_year",
      "livestock_meat_who_sells"
    ),
    warning_message = "Could not gender_split for meat sold amount"
  )
  if (length(missing_columns) == 0) {
    data <- insert_gender_columns_in_core_data(
      data = data,
      original_column = "meat_sold_kg_per_year",
      control_column = "livestock_meat_who_sells",
      loop_structure = T, gender_control_categories = gender_categories
    )
  }


  missing_columns <- check_columns_in_data(data,
    loop_columns = c(
      "meat_consumed_kg_per_year",
      "livestock_meat_who_control_eating"
    ),
    warning_message = "Could not gender_split for meat consumed amount"
  )
  if (length(missing_columns) == 0) {
    data <- insert_gender_columns_in_core_data(
      data = data,
      original_column = "meat_consumed_kg_per_year",
      control_column = "livestock_meat_who_control_eating",
      loop_structure = T, gender_control_categories = gender_categories
    )
  }

  # Gender split milk


  missing_columns <- check_columns_in_data(data,
    loop_columns = c(
      "milk_sold_litres_per_year",
      "milk_who_sells"
    ),
    warning_message = "Could not gender_split for milk sold amount"
  )
  if (length(missing_columns) == 0) {
    data <- insert_gender_columns_in_core_data(
      data = data,
      original_column = "milk_sold_litres_per_year",
      control_column = "milk_who_sells",
      loop_structure = T, gender_control_categories = gender_categories
    )
  }


  missing_columns <- check_columns_in_data(data,
    loop_columns = c(
      "milk_sold_income_per_year",
      "milk_who_sells"
    ),
    warning_message = "Could not gender_split for milk sold income"
  )
  if (length(missing_columns) == 0) {
    data <- insert_gender_columns_in_core_data(
      data = data,
      original_column = "milk_sold_income_per_year",
      control_column = "milk_who_sells",
      loop_structure = T, gender_control_categories = gender_categories
    )
  }

  missing_columns <- check_columns_in_data(data,
    loop_columns = c(
      "milk_consumed_litres_per_year",
      "milk_who_control_eating"
    ),
    warning_message = "Could not gender_split for milk consumed amount"
  )
  if (length(missing_columns) == 0) {
    data <- insert_gender_columns_in_core_data(
      data = data,
      original_column = "milk_consumed_litres_per_year",
      control_column = "milk_who_control_eating",
      loop_structure = T, gender_control_categories = gender_categories
    )
  }

  # Eggs gender split
  missing_columns <- check_columns_in_data(data,
    loop_columns = c(
      "eggs_sold_kg_per_year",
      "eggs_who_sells"
    ),
    warning_message = "Could not gender_split for eggs sold amount"
  )
  if (length(missing_columns) == 0) {
    data <- insert_gender_columns_in_core_data(
      data = data,
      original_column = "eggs_sold_kg_per_year",
      control_column = "eggs_who_sells",
      loop_structure = T, gender_control_categories = gender_categories
    )
  }


  missing_columns <- check_columns_in_data(data,
    loop_columns = c(
      "eggs_income_per_year",
      "eggs_who_sells"
    ),
    warning_message = "Could not gender_split for eggs sold income"
  )
  if (length(missing_columns) == 0) {
    data <- insert_gender_columns_in_core_data(
      data = data,
      original_column = "eggs_income_per_year",
      control_column = "eggs_who_sells",
      loop_structure = T, gender_control_categories = gender_categories
    )
  }


  missing_columns <- check_columns_in_data(data,
    loop_columns = c(
      "eggs_consumed_kg_per_year",
      "eggs_who_control_eating"
    ),
    warning_message = "Could not gender_split for eggs consumed per year"
  )
  if (length(missing_columns) == 0) {
    data <- insert_gender_columns_in_core_data(
      data = data,
      original_column = "eggs_consumed_kg_per_year",
      control_column = "eggs_who_control_eating",
      loop_structure = T, gender_control_categories = gender_categories
    )
  }

  # Honey Gender split

  missing_columns <- check_columns_in_data(data,
    loop_columns = c(
      "bees_honey_sold_kg_per_year",
      "bees_who_sells"
    ),
    warning_message = "Could not gender_split for honey sold amount"
  )
  if (length(missing_columns) == 0) {
    data <- insert_gender_columns_in_core_data(
      data = data,
      original_column = "bees_honey_sold_kg_per_year",
      control_column = "bees_who_sells",
      loop_structure = T, gender_control_categories = gender_categories
    )
  }

  missing_columns <- check_columns_in_data(data,
    loop_columns = c(
      "bees_honey_sold_income",
      "bees_who_sells"
    ),
    warning_message = "Could not gender_split for honey sold income"
  )
  if (length(missing_columns) == 0) {
    data <- insert_gender_columns_in_core_data(
      data = data,
      original_column = "bees_honey_sold_income",
      control_column = "bees_who_sells",
      loop_structure = T, gender_control_categories = gender_categories
    )
  }

  missing_columns <- check_columns_in_data(data,
    loop_columns = c(
      "bees_honey_consumed_kg_per_year",
      "bees_who_control_eating"
    ),
    warning_message = "Could not gender_split for honey consumed amount"
  )
  if (length(missing_columns) == 0) {
    data <- insert_gender_columns_in_core_data(
      data = data,
      original_column = "bees_honey_consumed_kg_per_year",
      control_column = "bees_who_control_eating",
      loop_structure = T, gender_control_categories = gender_categories
    )
  }


  return(data)
}


#' Livestock calculations all
#'
#' Carrying out all calculations on RHoMIS
#' livestock loops.
#'
#' @param livestock_weights_conv_tibble Conversion tibble of livestock weights
#' @param eggs_amount_unit_conv_tibble Conversion tibble of eggs amount
#' @param eggs_price_time_units_conv_tibble Conversion tibble of egg price time units
#' @param honey_amount_unit_conv_tibble Conversion tibble of honey amount
#' @param milk_amount_unit_conv_tibble Conversion tibble of milk amounts
#' @param milk_price_time_unit_conv_tibble Conversion tibble of milk prices
#' @param data RHoMIS data, containing livestock loops
#' @param gender_categories The gender control catgeories included in the survey
#' @return
#' @export
#'
#' @examples
livestock_calculations_all <- function(data,
                                       livestock_weights_conv_tibble = NULL,
                                       eggs_amount_unit_conv_tibble = NULL,
                                       eggs_price_time_units_conv_tibble = NULL,
                                       honey_amount_unit_conv_tibble = NULL,
                                       milk_amount_unit_conv_tibble = NULL,
                                       milk_price_time_unit_conv_tibble = NULL,
                                       gender_categories = pkg.env$gender_categories) {
  if ("id_rhomis_dataset" %in% colnames(data) == F) {
    stop("Missing the id_rhomis_dataset column in RHoMIS data")
  }

  if (is.null(livestock_weights_conv_tibble)) {
    livestock_weights_conv_tibble <- make_per_project_conversion_tibble(
      proj_id_vector = data[["id_rhomis_dataset"]],
      unit_conv_tibble = livestock_weights
    )
  }

  if (is.null(eggs_amount_unit_conv_tibble)) {
    eggs_amount_unit_conv_tibble <- make_per_project_conversion_tibble(
      proj_id_vector = data[["id_rhomis_dataset"]],
      unit_conv_tibble = eggs_amount_units
    )
  }

  if (is.null(eggs_price_time_units_conv_tibble)) {
    eggs_price_time_units_conv_tibble <- make_per_project_conversion_tibble(
      proj_id_vector = data[["id_rhomis_dataset"]],
      unit_conv_tibble = eggs_price_time_units
    )
  }

  if (is.null(honey_amount_unit_conv_tibble)) {
    honey_amount_unit_conv_tibble <- make_per_project_conversion_tibble(
      proj_id_vector = data[["id_rhomis_dataset"]],
      unit_conv_tibble = honey_amount_units
    )
  }

  if (is.null(milk_amount_unit_conv_tibble)) {
    milk_amount_unit_conv_tibble <- make_per_project_conversion_tibble(
      proj_id_vector = data[["id_rhomis_dataset"]],
      unit_conv_tibble = milk_amount_units
    )
  }

  if (is.null(milk_price_time_unit_conv_tibble)) {
    milk_price_time_unit_conv_tibble <- make_per_project_conversion_tibble(
      proj_id_vector = data[["id_rhomis_dataset"]],
      unit_conv_tibble = milk_price_time_units
    )
  }



  # Adding livestock prices to the data set
  missing_columns <- check_columns_in_data(data,
    loop_columns = c("livestock_sold", "livestock_sale_income"),
    warning_message = "Could not calculate livestock prices"
  )
  if (length(missing_columns) == 0) {
    data <- price_per_livestock(data)
  }



  # Calculating the amount of meat collected and adding it to
  # the data-set
  missing_columns <- check_columns_in_data(data,
    loop_columns = c("livestock_name", "killed_for_meat"),
    warning_message = "Could not calculate amounts of meat harvested"
  )
  if (length(missing_columns) == 0) {
    data <- meat_amount_calculation(data, unit_conv_tibble = livestock_weights_conv_tibble)
  }




  # Meat sold and consumed amounts

  data <- meat_sold_and_consumed_calculation(data)




  # Meat Prices
  missing_columns <- check_columns_in_data(data,
    loop_columns = c(
      "meat_sold_income",
      "meat_sold_kg_per_year",
      "livestock_name"
    ),
    warning_message = "Could not calculate income from meat sold"
  )
  if (length(missing_columns) == 0) {
    data <- meat_prices(data)
  }







  # Milk amounts
  missing_columns <- check_columns_in_data(data,
    loop_columns = c(
      "milk_amount_good_season",
      "milk_amount_bad_season",
      "milk_units",
      "livestock_name"
    ),
    warning_message = "Cannot calculate the amounts of milk collected"
  )
  if (length(missing_columns) == 0) {
    data <- milk_amount_calculations(data, unit_conv_tibble = milk_amount_unit_conv_tibble)
  }




  # Milk sold and consumed

  data <- milk_sold_and_consumed_calculations(data)




  # Milk income
  missing_columns <- check_columns_in_data(data,
    loop_columns = c(
      "milk_sold_price_timeunits",
      "milk_sold_income",
      "milk_sold_litres_per_year"
    ),
    warning_message = "Cannot calculate milk incomes"
  )
  if (length(missing_columns) == 0) {
    data <- milk_income_calculations(data, unit_conv_tibble = milk_price_time_unit_conv_tibble)
  }



  # Eggs amounts
  missing_columns <- check_columns_in_data(data,
    loop_columns = c(
      "eggs_amount_good",
      "eggs_amount_bad",
      "eggs_units",
      "livestock_name"
    ),
    warning_message = "Could not calculate amounts of eggs collected"
  )
  if (length(missing_columns) == 0) {
    data <- eggs_amount_calculations(data, unit_conv_tibble = eggs_amount_unit_conv_tibble)
  }


  # Eggs sold and consumed

  data <- eggs_sold_and_consumed_calculations(data)



  # Eggs income
  missing_columns <- check_columns_in_data(data,
    loop_columns = c(
      "eggs_sold_income",
      "eggs_sold_price_timeunits",
      "livestock_name"
    ),
    warning_message = "Could not calculate egg incomes"
  )
  if (length(missing_columns) == 0) {
    data <- egg_income_calculations(data, unit_conv_tibble = eggs_price_time_units_conv_tibble)
  }




  # Honey amount

  missing_columns <- check_columns_in_data(data,
    loop_columns = c(
      "bees_honey_production",
      "bees_honey_production_units"
    ),
    warning_message = "Could not calculate honey amounts"
  )
  if (length(missing_columns) == 0) {
    data <- honey_amount_calculation(data, unit_conv_tibble = honey_amount_unit_conv_tibble)
  }


  # Honey sold and consumed
  data <- honey_amount_sold_and_consumed_calculations(data)
  data <- honey_income_calculations(data)


  data <- gender_split_livestock(data,
    gender_categories = gender_categories
  )

  return(data)
}


#' Calculate the prices for honey
#'
#' @param data RHoMIS dataset.
#'
#' @return
#' @export
#'
#' @examples
honey_income_calculations <- function(data) {
  missing_columns <- check_columns_in_data(data,
    loop_columns = c(
      "bees_honey_sold_income",
      "bees_honey_sold_kg_per_year",
      "livestock_name"
    ),
    warning_message = "Could not calculate honey prices"
  )

  if (length(missing_columns) == 0) {
    number_of_loops <- find_number_of_loops(data, name_column = "bees_honey_sold_kg_per_year")
    sold_columns <- paste0("bees_honey_sold_kg_per_year", "_", c(1:number_of_loops))
    income_columns <- paste0("bees_honey_sold_income", "_", c(1:number_of_loops))
    price_columns <- paste0("bees_honey_price_per_kg", "_", c(1:number_of_loops))

    sold_data <- data[sold_columns]
    sold_data <- sold_data %>% dplyr::mutate_all(as.numeric)
    income_data <- data[income_columns]
    income_data <- income_data %>% dplyr::mutate_all(as.numeric)


    bees_honey_prices <- income_data / sold_data
    colnames(bees_honey_prices) <- price_columns

    data <- add_column_after_specific_column(data,
      new_data = bees_honey_prices,
      new_column_name = "bees_honey_price_per_kg",
      old_column_name = "bees_honey_sold_income",
      loop_structure = T
    )
  }

  return(data)
}