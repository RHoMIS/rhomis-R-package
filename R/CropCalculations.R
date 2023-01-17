#' Convert Crop Yield Units
#'
#' RHoMIS crop yields come in a character format. This function
#' converts these to numeric and adds the new numeric units in an
#' appropriate place for the dataset
#'
#' Rpackage file: CropCalculations.R
#'
#' @param data The data containing the units to convert
#' @param unit_conv_tibble Unit tibble for crop yield,
#' including individual idsfor projects. Crops to be coverted
#' to kg amount
#'
#' @return
#' @export
#'
#' @examples
convert_crop_yield_units <- function(data,
                                     unit_conv_tibble = NULL) {
  if ("id_rhomis_dataset" %in% colnames(data) == F) {
    stop("Missing the id_rhomis_dataset column in RHoMIS data")
  }

  if (is.null(unit_conv_tibble)) {
    unit_conv_tibble <- make_per_project_conversion_tibble(
      proj_id_vector = data[["id_rhomis_dataset"]],
      unit_conv_tibble = crop_amount_to_kg
    )
  }

  missing_columns <- check_columns_in_data(data,
    loop_columns = c(
      "crop_name",
      "crop_yield_units"
    ),
    warning_message = "Tried to convert crop yield units, will not
    be able to calculate crop yields. Other indicators will also be
    affected"
  )

  if (length(missing_columns) == 0) {
    number_of_loops <- find_number_of_loops(data, name_column = "crop_name")
    columns_to_convert <- paste0(
      "crop_yield_units",
      "_",
      c(1:number_of_loops)
    )
    new_column_names <- paste0(
      "crop_yield_units_numeric",
      "_",
      c(1:number_of_loops)
    )
    numeric_crop_units <- switch_units(
      data_to_convert = data[columns_to_convert],
      unit_tibble = unit_conv_tibble,
      id_vector = data[["id_rhomis_dataset"]]
    )
    colnames(numeric_crop_units) <- new_column_names

    # indicator_search_crop_yield_units_numeric
    data <- add_column_after_specific_column(
      data = data,
      new_data = numeric_crop_units,
      new_column_name = "crop_yield_units_numeric",
      old_column_name = "crop_yield_units",
      loop_structure = T
    )
  }
  return(data)
}

#' Crop yield single loop
#'
#' Calculate crop yield based off of a single loop
#'
#' Rpackage file: CropCalculations.R
#'
#' @param data THe data containind crop yields, and numeric
#' crop yield units
#' @param loop_number which loop you are calculating crop yield for.
#'
#' @return
#' @export
#'
#' @examples
crop_harvest_single_loop <- function(data, loop_number) {
  missing_columns <- check_columns_in_data(data,
    individual_columns = c(
      paste0(
        "crop_yield",
        "_",
        loop_number
      ),
      paste0("crop_yield_units_numeric", "_", loop_number)
    ),
    warning_message = "Tried to calculate crop yield, but some information was missing."
  )

  if (length(missing_columns) == 0) {
    crop_yield <- as.numeric(
      data[[paste0(
        "crop_yield",
        "_",
        loop_number
      )]]
    ) * as.numeric(data[[paste0(
      "crop_yield_units_numeric",
      "_",
      loop_number
    )]])
  }
  return(crop_yield)
}

#' Crop harvest calculations
#'
#' Calculating the amount of each crop harvested per year
#'
#' Rpackage file: CropCalculations.R
#'
#' @param data RHoMIS data containing cropping information
#' @param unit_conv_tibble Units and conversions to convert crop yields to kg
#'
#' @return
#' @export
#'
#' @examples
crop_harvest_calculations <- function(data, unit_conv_tibble = NULL) {

  # indicator_search_crop_harvest_kg_per_year
  missing_columns <- check_columns_in_data(data,
    loop_columns = c("crop_name"),
    warning_message = "Tried to calculate crop yield, but some information was missing."
  )

  if (length(missing_columns) == 0) {
    number_of_loops <- find_number_of_loops(data, name_column = "crop_name")
    data <- convert_crop_yield_units(data, unit_conv_tibble = unit_conv_tibble)

    new_column_names <- paste0("crop_harvest_kg_per_year", "_", 1:number_of_loops)
    crop_harvest_per_year <- sapply(c(1:number_of_loops), function(x) crop_harvest_single_loop(data, x))
    colnames(crop_harvest_per_year) <- new_column_names
    crop_harvest_per_year <- tibble::as_tibble(crop_harvest_per_year)

    data <- add_column_after_specific_column(
      data = data,
      new_data = crop_harvest_per_year,
      new_column_name = "crop_harvest_kg_per_year",
      old_column_name = "crop_yield_units_numeric",
      loop_structure = T
    )
  }

  return(data)
}


#' Crop Proportions for all
#'
#' A function for calculating the numeric proportions of crops which are sold or
#' consumed
#'
#' Rpackage file: CropCalculations.R
#'
#' @param data A standard RHoMIS data set
#'
#' @return
#' @export
#'
#' @examples
crop_proportions_all <- function(data) {
  number_of_loops <- find_number_of_loops(data, name_column = "crop_name")

  crop_consumed_columns_in_data <- check_columns_in_data(data,
    loop_columns = c("crop_consumed_prop"),
    warning_message = "Crop consumed calculation not possible"
  )
  if (length(crop_consumed_columns_in_data) == 0) {
    crop_consumed_proportions_numeric <- sapply(c(1:number_of_loops), function(x) proportions_calculation(data, use = "eat", use_column = "crop_use", prop_column = "crop_consumed_prop", loop_number = x))
    colnames(crop_consumed_proportions_numeric) <- paste0("crop_consumed_prop_numeric", "_", c(1:number_of_loops))
    crop_consumed_proportions_numeric <- tibble::as_tibble(crop_consumed_proportions_numeric)
    data <- add_column_after_specific_column(
      data = data,
      new_data = crop_consumed_proportions_numeric,
      new_column_name = "crop_consumed_prop_numeric",
      old_column_name = "crop_consumed_prop",
      loop_structure = T
    )
  }

  crop_sold_columns_in_data <- check_columns_in_data(data,
    loop_columns = c("crop_sold_prop"),
    warning_message = "Crop sold calculation not possible"
  )
  if (length(crop_sold_columns_in_data) == 0) {
    crop_sold_proportions_numeric <- sapply(c(1:number_of_loops), function(x) proportions_calculation(data, use = "sell", use_column = "crop_use", prop_column = "crop_sold_prop", loop_number = x))
    colnames(crop_sold_proportions_numeric) <- paste0("crop_sold_prop_numeric", "_", c(1:number_of_loops))
    crop_sold_proportions_numeric <- tibble::as_tibble(crop_sold_proportions_numeric)
    data <- add_column_after_specific_column(
      data = data,
      new_data = crop_sold_proportions_numeric,
      new_column_name = "crop_sold_prop_numeric",
      old_column_name = "crop_sold_prop",
      loop_structure = T
    )
  }

  return(data)
}



#' Convert Crop Sold Units
#'
#' Convert crop income units into a numeric conversion factor
#'
#' Rpackage file: CropCalculations.R
#'
#' @param data RHoMIS data containing crop-looping information
#' @param unit_conv_tibble A tibble of unit conversions.
#'
#' @return
#' @export
#'
#' @examples
convert_crop_sold_units <- function(data,
                                    unit_conv_tibble = NULL) {

    #indicator_search_crop_sold_units_numeric
  if ("id_rhomis_dataset" %in% colnames(data) == F) {
    stop("Missing the id_rhomis_dataset column in RHoMIS data")
  }

  if (is.null(unit_conv_tibble)) {
    unit_conv_tibble <- make_per_project_conversion_tibble(
      proj_id_vector = data[["id_rhomis_dataset"]],
      unit_conv_tibble = crop_price_to_lcu_per_kg
    )
  }


  number_of_loops <- find_number_of_loops(data, name_column = "crop_name")
  crop_sold_units_column <- paste0("crop_sold_price_quantityunits", "_", c(1:number_of_loops))

  crop_sold_units <- data[crop_sold_units_column]
  colnames(crop_sold_units) <- paste0("crop_sold_units_numeric", "_", c(1:number_of_loops))
  units_converted <- switch_units(crop_sold_units,
    unit_tibble = unit_conv_tibble,
    id_vector = data[["id_rhomis_dataset"]]
  )

  data <- add_column_after_specific_column(
    data = data,
    new_data = units_converted,
    new_column_name = "crop_sold_units_numeric",
    old_column_name = "crop_sold_price_quantityunits",
    loop_structure = T
  )

  return(data)
}

#' Crop sold and consumed.
#'
#' Note that for this function to work,
#' previous functions have to have been run. The crops harvested has to have been
#' calculated, as well as the proportions (numeric) which have been sold
#' and consumed
#'
#' Rpackage file: CropCalculations.R
#'
#' @param data RHoMIS data which has been processed to include
#' crops harvested (see above) and crop proportions sold/consumed.
#'
#' @return
#' @export
#'
#' @examples
crop_sold_and_consumed_calculation <- function(data) {
  data <- crop_proportions_all(data)
  # Beginning with crops sold
  # indicator_search_crop_sold_kg_per_year
  number_of_loops <- find_number_of_loops(data, name_column = "crop_name")
  harvested_columns <- paste0("crop_harvest_kg_per_year", "_", c(1:number_of_loops))
  sold_columns <- paste0("crop_sold_prop_numeric", "_", c(1:number_of_loops))

  if (all(harvested_columns %in% colnames(data)) == F) {
    stop("Have not calculated the amounts harvested in kg. Calculate amounts harvested before calculating amounts sold")
  }
  if (all(sold_columns %in% colnames(data)) == F) {
    stop("Have not calculated the numeric proportions of amount of crops sold. Calculate proportions sold before calculating amounts sold")
  }

  harvest_data <- data[harvested_columns]
  sold_prop_data <- data[sold_columns]

  amount_sold_kg <- tibble::as_tibble(harvest_data * sold_prop_data)
  colnames(amount_sold_kg) <- paste0("crop_sold_kg_per_year", "_", c(1:number_of_loops))

  data <- add_column_after_specific_column(
    data = data,
    new_data = amount_sold_kg,
    new_column_name = "crop_sold_kg_per_year",
    old_column_name = "crop_sold_prop_numeric",
    loop_structure = T
  )

  # Moving on to crops consumed
  # indicator_search_crop_consumed_kg_per_year
  number_of_loops <- find_number_of_loops(data, name_column = "crop_name")
  harvested_columns <- paste0("crop_harvest_kg_per_year", "_", c(1:number_of_loops))
  consumed_columns <- paste0("crop_consumed_prop_numeric", "_", c(1:number_of_loops))

  if (all(harvested_columns %in% colnames(data)) == F | all(consumed_columns %in% colnames(data)) == F) {
    warning("Have not calculated the amounts harvested in kg or amounts sold. Calculate amounts harvested before calculating amounts consumed")
  }
  if (all(harvested_columns %in% colnames(data)) == T & all(consumed_columns %in% colnames(data)) == T) {
    harvest_data <- data[harvested_columns]
    consumed_prop_data <- data[consumed_columns]

    amount_consumed_kg <- tibble::as_tibble(harvest_data * consumed_prop_data)
    colnames(amount_consumed_kg) <- paste0("crop_consumed_kg_per_year", "_", c(1:number_of_loops))

    data <- add_column_after_specific_column(
      data = data,
      new_data = amount_consumed_kg,
      new_column_name = "crop_consumed_kg_per_year",
      old_column_name = "crop_consumed_prop_numeric",
      loop_structure = T
    )
  }

  return(data)
}

#' Crop Income Calculations
#'
#' A function for calculating crop incomes. Please note
#' for this calculation to work. The amount of a crop harvested, and the amount
#' of a crop sold also needs to have been calculated
#'
#' Rpackage file: CropCalculations.R
#'
#' @param data A RHoMIS dataset, including information on crop harvested,
#' and crop sold
#' @param unit_conv_tibble Units for crop income calculations which
#' need to be converted
#'
#' @return
#' @export
#'
#' @examples
crop_income_calculations <- function(data, unit_conv_tibble = NULL) {

  #indicator_search_crop_income_per_year
  data <- convert_crop_sold_units(data, unit_conv_tibble = unit_conv_tibble)

  number_of_loops <- find_number_of_loops(data, name_column = "crop_name")

  crop_sold_columns <- paste0("crop_sold_kg_per_year", "_", c(1:number_of_loops))
  crop_sold_unit_columns <- paste0("crop_sold_units_numeric", "_", c(1:number_of_loops))
  crop_sold_income_columns <- paste0("crop_sold_income", "_", c(1:number_of_loops))

  if (all(crop_sold_columns %in% colnames(data)) == F) {
    stop("Have not calculated the amounts sold in kg. Calculate amounts sold before calculating income")
  }
  if (all(crop_sold_unit_columns %in% colnames(data)) == F) {
    stop("Have not converted the crop price quantity units yet. Convert these units before calculating incomes sold")
  }

  crop_sold_amount <- data[crop_sold_columns]
  crop_sold_units <- data[crop_sold_unit_columns]
  crop_sold_income <- data[crop_sold_income_columns]
  crop_sold_income <- crop_sold_income %>% dplyr::mutate_all(as.numeric)

  crop_sold_units_numeric <- crop_sold_units
  crop_sold_units_numeric[crop_sold_units == "total_income_per_year"] <- NA
  crop_sold_units_numeric <- crop_sold_units_numeric %>% dplyr::mutate_all(as.numeric)


  crop_sold_income_per_year <- crop_sold_income
  # Multiplying values which do not have "total_income_per_year_unit
  crop_sold_income_per_year <- crop_sold_income_per_year %>% dplyr::mutate_all(as.numeric)
  crop_sold_amount <- crop_sold_amount %>% dplyr::mutate_all(as.numeric)

  crop_sold_income_per_year <- crop_sold_income_per_year * crop_sold_units_numeric * crop_sold_amount

  crop_sold_income_per_year[crop_sold_amount==0] <- 0

  subscript_frame <- crop_sold_units == "total_income_per_year"
  subscript_frame[is.na(subscript_frame)] <- FALSE
  crop_sold_income_per_year[subscript_frame] <- as.numeric(crop_sold_income[subscript_frame])
  crop_sold_income_per_year <- tibble::as_tibble(crop_sold_income_per_year)
  crop_sold_income_per_year <- crop_sold_income_per_year %>% dplyr::mutate_all(as.numeric)


  colnames(crop_sold_income_per_year) <- paste0("crop_income_per_year", "_", c(1:number_of_loops))
  data <- add_column_after_specific_column(
    data = data,
    new_data = crop_sold_income_per_year,
    new_column_name = "crop_income_per_year",
    old_column_name = "crop_sold_units_numeric",
    loop_structure = T
  )

  crop_price <- crop_sold_income_per_year / crop_sold_amount
  colnames(crop_price) <- paste0("crop_price", "_", c(1:number_of_loops))

  # indicator_search_crop_price
  data <- add_column_after_specific_column(
    data = data,
    new_data = crop_price,
    new_column_name = "crop_price",
    old_column_name = "crop_income_per_year",
    loop_structure = T
  )
}

#' Crop Gender Calculations
#'
#' Adding the gendered information for crop consumed
#' crop sold, and crop income. These have to already have been
#' calculated in the dataset being used
#'
#' Rpackage file: CropCalculations.R
#'
#' @param data RHoMIS data set conatining processed values for
#' crop consumed, crop sold, and crop income
#' @param gender_categories The categories you are interested in examining
#'
#' @return
#' @export
#'
#' @examples
crop_gender_calculations <- function(data, gender_categories = pkg.env$gender_categories) {


  crop_columns_in_data <- check_columns_in_data(data,
    loop_columns = c(
      "crop_consumed_kg_per_year",
      "crop_consume_control",
      "crop_sold_kg_per_year",
      "crop_who_control_revenue",
      "crop_income_per_year"
    )
  )


  crop_gender_columns_in_data <- check_columns_in_data(data,
    loop_columns = c(
      "crop_consumed_kg_per_year",
      "crop_consume_control"
    )
  )

  # indicator_search_male_youth_crop_consumed_kg_per_year
  # indicator_search_male_adult_crop_consumed_kg_per_year
  # indicator_search_female_youth_crop_consumed_kg_per_year
  # indicator_search_female_adult_crop_consumed_kg_per_year


  if (length(crop_gender_columns_in_data) == 0) {
    # crop consumed calculations
    data <- insert_gender_columns_in_core_data(
      data = data,
      original_column = "crop_consumed_kg_per_year",
      control_column = "crop_consume_control",
      loop_structure = T,
      gender_control_categories = gender_categories
    )
  }

  # crop sold calculations
  crop_gender_columns_in_data <- check_columns_in_data(data,
    loop_columns = c(
      "crop_sold_kg_per_year",
      "crop_who_control_revenue"
    )
  )

  # indicator_search_male_youth_crop_sold_kg_per_year
  # indicator_search_male_adult_crop_sold_kg_per_year
  # indicator_search_female_youth_crop_sold_kg_per_year
  # indicator_search_female_adult_crop_sold_kg_per_year

  if (length(crop_gender_columns_in_data) == 0) {
    data <- insert_gender_columns_in_core_data(data,
      original_column = "crop_sold_kg_per_year",
      control_column = "crop_who_control_revenue",
      loop_structure = T,
      gender_control_categories = gender_categories
    )
  }

  # crop income calculations
  crop_gender_columns_in_data <- check_columns_in_data(data,
    loop_columns = c(
      "crop_income_per_year",
      "crop_who_control_revenue"
    )
  )


  # indicator_search_male_youth_crop_income_per_year
  # indicator_search_male_adult_crop_income_per_year
  # indicator_search_female_youth_crop_income_per_year
  # indicator_search_female_adult_crop_income_per_year
  if (length(crop_gender_columns_in_data) == 0) {
    data <- insert_gender_columns_in_core_data(data,
      original_column = "crop_income_per_year",
      control_column = "crop_who_control_revenue",
      loop_structure = T,
      gender_control_categories = gender_categories
    )
  }

  return(data)
}

#' Crop Calculations All
#'
#' A single function for conducting all of the crop calculations
#'
#' Rpackage file: CropCalculations.R
#'
#' @param data RHoMIS crop loop data
#' @param crop_yield_units_conv_tibble Conversion tibble of crop yield units
#' @param crop_income_units_conv_tibble Conversion tibble of crop income units
#' @param gender_categories The categories you are interested in examining
#'
#' @return
#' @export
#'
#' @examples
crop_calculations_all <- function(data,
                                  crop_yield_units_conv_tibble = crop_yield_units,
                                  crop_income_units_conv_tibble = crop_price_units,
                                  gender_categories = pkg.env$gender_categories) {

  # Calculating the amount of crops harvested in kg
  crop_columns_in_data <- check_columns_in_data(data,
    loop_columns = c(
      "crop_name",
      "crop_yield",
      "crop_yield_units"
    ),
    warning_message = "Cannot calculate amounts of crops harvested"
  )
  if (length(crop_columns_in_data) == 0) {
    data <- crop_harvest_calculations(data, unit_conv_tibble = crop_yield_units_conv_tibble)
  }



  # Calculating amounts sold and consumed in kg
  crop_columns_in_data <- check_columns_in_data(data,
    loop_columns = c(
      "crop_harvest_kg_per_year",
      "crop_sold_prop",
      "crop_consumed_prop"
    ),
    warning_message = "Problems calculating crop sold and crop consumed"
  )
  if (length(crop_columns_in_data)==0){
  data <- crop_sold_and_consumed_calculation(data)
  }


  # Crop income calculations
  crop_columns_in_data <- check_columns_in_data(data,
    loop_columns = c(
      "crop_sold_kg_per_year",
      "crop_sold_price_quantityunits",
      "crop_sold_income"
    ),
    warning_message = "Cannot calculate amounts of crop incomes"
  )
  if (length(crop_columns_in_data) == 0) {
    data <- crop_income_calculations(data, unit_conv_tibble = crop_income_units_conv_tibble)
  }



  data <- crop_gender_calculations(data, gender_categories = gender_categories)

  return(data)
}
