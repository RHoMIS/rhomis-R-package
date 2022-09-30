

extract_values_central <- function(central_email,
                                   central_password,
                                   project_name,
                                   form_name,
                                   form_version,
                                   database,
                                   isDraft,
                                   country_column = "country",
                                   unique_id_col = "_uuid",
                                   hh_id_col = NULL) {


  # Getting project and formID
  projectID <- get_project_id_from_name(
    project_name,
    central_url,
    central_email,
    central_password
  )

  # Finding form information from the API
  formID <- get_xml_form_id_from_name(
    form_name,
    projectID,
    central_url,
    central_email,
    central_password
  )

  # Getting the submission data from ODK central
  rhomis_data <- get_submission_data(
    central_url,
    central_email,
    central_password,
    projectID,
    formID,
    isDraft
  )



  colnames(rhomis_data) <- clean_column_names(colnames(rhomis_data))
  rhomis_data <- rhomis_data %>%
    remove_extra_central_columns()



  rhomis_data <- convert_all_columns_to_lower_case(rhomis_data)

  rhomis_data <- sapply(rhomis_data, function(x) {
    x[as.numeric(x) == -999] <- NA
    x
  }, simplify = F) %>% tibble::as_tibble()


  indicator_data <- tibble::as_tibble(list(
    projectName = rep(project_name, nrow(rhomis_data)),
    formName = rep(form_name, nrow(rhomis_data)),
    formVersion = rep(form_version, nrow(rhomis_data))
  ))



  # Loading a rhomis dataset, and adding
  # id values (using a hashing function, digest)
  # to make sure that units, crop name conversions...
  # are all linked to a specific project.
  rhomis_data <- load_rhomis_csv(
    file_path = file_path,
    country_column = country_column,
    unique_id_col = unique_id_col,
    hh_id_col = hh_id_col,
    id_type = id_type,
    proj_id = proj_id,
    form_id = form_id
  )

  extract_project_values(rhomis_data)
  new_values <- extract_values_by_project(rhomis_data)
  new_values <- check_existing_conversions(list_of_df = new_values)

  units_folder_dest <- paste0(base_path, "/original_units")
  write_units_to_folder(
    list_of_df = new_values,
    folder = units_folder_dest
  )

  new_units_dest <- paste0(base_path, "/converted_units")

  if (dir.exists(new_units_dest) == F) {
    write_units_to_folder(
      list_of_df = new_values,
      folder = new_units_dest
    )
  }
  if (overwrite == T) {
    write_units_to_folder(
      list_of_df = new_values,
      folder = new_units_dest
    )
  }
}



#' Extract values
#'
#' Rpackage file: redirectModules.R
#'
#' @param base_folder The folder where all outputs will be written to
#' @param file_path The path to the raw-data rhomis file
#' @param overwrite Whether or not you would like to overwrite your
#' converted units
#' @param country_column The column name for the variable "country".
#' @param unique_id_col The column name of the odk uuid, usually "_uuid"
#' @param hh_id_col The column name containing household IDs.
#' @param id_type The type of ID you would like to enter for projects and forms. If you select "string", then fill in the proj_id and form_id arguments, with the project id and form id you would like to use. If selecting "column", enter the name of the column (proj_id_col) containing the project ID you would like to use, and the name of the column (form_id_col) containing the form ids you would like to use.
#' @param id_type Indicator of whether you are providing a single ID
#' @param proj_id Either a single string to be used as the project ID for all households, or the name of the column containing the project IDs (depending on id_type)
#' @param form_id Either a single string to be used as the form ID for all households, or the name of the column containing the form IDs (depending on id_type)
#' @param overwrite True if you would like to overwrite previous ID column, false if would not like to overwrite existing IDs
#' @param repeat_column_names The prefixes for repeat columns (e.g. 'section/crop_repeat[1]/crop_name', the prefix would be crop_repeat)
#'
#' @return
#' @export
#'
#' @examples
extract_values_local <- function(base_folder = "./",
                                 file_path = "./raw-data/raw-data.csv",
                                 country_column = pkg.env$identification_column_list$country,
                                 unique_id_col = pkg.env$identification_column_list$uuid_local,
                                 hh_id_col = NULL,
                                 id_type = c("string", "column"),
                                 proj_id,
                                 form_id,
                                 overwrite = FALSE,
                                 repeat_column_names = pkg.env$repeat_columns) {
  id_type <- match.arg(id_type)

  # Loading a rhomis dataset, and adding
  # id values (using a hashing function, digest)
  # to make sure that units, crop name conversions...
  # are all linked to a specific project.
  rhomis_data <- load_rhomis_csv(
    file_path = file_path,
    country_column = country_column,
    unique_id_col = unique_id_col,
    hh_id_col = hh_id_col,
    id_type = id_type,
    proj_id = proj_id,
    form_id = form_id,
  )

  extract_project_values(rhomis_data)
  new_values <- extract_values_by_project(rhomis_data)
  new_values <- check_existing_conversions(list_of_df = new_values)

  units_folder_dest <- paste0(base_path, "/", pkg.env$local_processing_paths$original_units)
  write_units_to_folder(
    list_of_df = new_values,
    folder = units_folder_dest
  )

  new_units_dest <- paste0(base_path, "/", pkg.env$local_processing_paths$converted_units)

  if (dir.exists(new_units_dest) == F) {
    write_units_to_folder(
      list_of_df = new_values,
      folder = new_units_dest
    )
  }
  if (overwrite == T) {
    write_units_to_folder(
      list_of_df = new_values,
      folder = new_units_dest
    )
  }
}


#' Extract Project values
#'
#' Extract all of the values from an individual data set
#'
#' Rpackage file: redirectModules.R
#'
#' @param rhomis_data A rhomis data set
#'
#' @return
#' @export
#'
#' @examples
extract_project_values <- function(rhomis_data) {
  new_values <- extract_values_by_project(rhomis_data)
  new_values <- check_existing_conversions(list_of_df = new_values)

  return(new_values)
}


#' Make new dataset
#'
#' Rpackage file: redirectModules.R
#'
#' @param rhomis_data A rhomis_dataset including IDs
#'
#' @return
#' @export
#'
#' @examples
make_new_dataset <- function(rhomis_data) {
  if (
    "id_unique" %in% colnames(rhomis_data) |
      "id_hh" %in% colnames(rhomis_data) |
      "id_rhomis_dataset" %in% colnames(rhomis_data) |
      "id_form" %in% colnames(rhomis_data) |
      "id_proj" %in% colnames(rhomis_data)
  ) {
    return(
      rhomis_data[c("id_unique", "id_hh", "id_rhomis_dataset", "id_form", "id_proj")]
    )
  }
}

#' Calculate prices and indicator Local
#'
#' Calculate prices and initial indicators locally
#'
#' Rpackage file: redirectModules.R
#'
#' @param data The dataset which is to be processed
#' @param base_path Path to the project folder
#' @param units_path Path to the folder of converted units
#' @param gender_categories The gender categories used in this survey
#' @param units A nested list containing all of the unit conversion tibbles

#' @return
#' @export
#'
#' @examples
calculate_prices_and_indicator_local <- function(data,
                                                 base_path = "./", units_path = "converted_units/",
                                                 gender_categories = pkg.env$gender_categories,
                                                 units) {
  load_local_units(paste0(base_path, units_path), id_rhomis_dataset = data[["id_rhomis_dataset"]])

  # load_calorie_conversions
  results <- run_preliminary_calculations(data, gender_categories = gender_categories)


  lapply(names(results), function(x) {
    new_folder <- paste0(base_path, x)
    dir.create(new_folder, showWarnings = F)
    data_to_write <- results[[x]]

    if (x == "processed_data" | x == "indicator_data") {
      path <- paste0(new_folder, "/", x, ".csv")
      readr::write_csv(data_to_write, path)
      return()
    }

    write_list_of_df_to_folder(list_of_df = data_to_write, folder = new_folder)
  })


  if ("processed_data" %in% names(results)) {
    calorie_conversions_dfs <- check_existing_calorie_conversions(results$processed_data)

    calorie_conversions_dfs$staple_crop <- make_per_project_conversion_tibble(proj_id_vector = data[["id_rhomis_dataset"]], unit_conv_tibble = list(
      "staple_crop" = c("maize")
    ))

    original_calorie_values_folder <- paste0(base_path, "original_calorie_conversions")
    write_list_of_df_to_folder(list_of_df = calorie_conversions_dfs, folder = original_calorie_values_folder)

    converted_calorie_conversions_folder <- paste0(base_path, "completed_calorie_conversions")
    if (!dir.exists(converted_calorie_conversions_folder)) {
      write_list_of_df_to_folder(list_of_df = calorie_conversions_dfs, folder = converted_calorie_conversions_folder)
    }
  }

  converted_prices_folder <- paste0(base_path, "converted_prices")
  if (!dir.exists(converted_prices_folder)) {
    dir.create(converted_prices_folder, showWarnings = F)
    data_to_write <- results[["original_prices"]]
    write_list_of_df_to_folder(list_of_df = data_to_write, folder = converted_prices_folder)
  }




  return(results)
}

#' Calculate Values, Gender and Food Availability
#'
#' Rpackage file: redirectModules.R
#'
#' @param base_path Path to the project folder
#' @param units_path Path to the folder of converted units
#' @param processed_data_path Path to processed data folder
#' @param indicator_path Path to indicator folder
#' @param calories_path Path to calories folder
#' @param prices_path Path to prices folder
#' @param staple_crop The main staple crop consumed in the area
#' @param gender_categories The gender categories to include in
#' the analysis
#' @param units A nested list containing all of the unit conversion tibbles
#'
#' @return
#' @export
#'
#' @examples
calculate_values_gender_and_fa_local <- function(base_path = "./",
                                                 processed_data_path = "processed_data/",
                                                 indicator_path = "indicator_data/",
                                                 units_path = "converted_units/",
                                                 calories_path = "completed_calorie_conversions/",
                                                 prices_path = "converted_prices/",
                                                 staple_crop = "maize",
                                                 gender_categories = pkg.env$gender_categories,
                                                 units) {

  # Load unit conversions into the global environment

  processed_data <- read_folder_of_csvs(folder = paste0(base_path, processed_data_path))[[1]]
  indicator_data <- read_folder_of_csvs(folder = paste0(base_path, indicator_path))[[1]]
  load_local_units(paste0(base_path, units_path), id_rhomis_dataset = processed_data[["id_rhomis_dataset"]])

  prices <- read_folder_of_csvs(folder = paste0(base_path, prices_path))
  calorie_conversions <- read_folder_of_csvs(folder = paste0(base_path, calories_path))

  results <- value_gender_fa_calculations(
    processed_data = processed_data,
    indicator_data = indicator_data,
    calorie_conversions = calorie_conversions,
    prices = prices,
    gender_categories = gender_categories
  )



  lapply(names(results), function(x) {
    data_to_write <- results[[x]]

    if (x == "processed_data" | x == "indicator_data") {
      new_folder <- paste0(base_path, x)
      dir.create(new_folder, showWarnings = F)

      path <- paste0(new_folder, "/", x, ".csv")
      readr::write_csv(data_to_write, path)
      return()
    }

    if (x == "extra_outputs") {
      write_list_of_df_to_folder(list_of_df = data_to_write, folder = base_path)
    }
  })


  return(results)
}



#' Value, Gender, and Food Availability Calculations
#'
#' Rpackage file: redirectModules.R
#'
#' @param processed_data RHoMIS Processed Dataset
#' @param indicator_data RHoMIS Indicator Data set
#' @param calorie_conversions Calorie conversions list
#' @param gender_categories Gender categories to convert
#' @param prices A list of tibbles on mean prices
#' @param units A list of units and conversion factors
#'
#' @return
#' @export
#'
#' @examples
value_gender_fa_calculations <- function(processed_data,
                                         indicator_data,
                                         calorie_conversions,
                                         prices,
                                         gender_categories,
                                         units) {
  extra_outputs <- list()
  value_calc_results <- value_calculations(
    processed_data,
    indicator_data,
    prices,
    gender_categories
  )

  processed_data <- value_calc_results$processed_data
  indicator_data <- value_calc_results$indicator_data
  extra_outputs$consumption_lcu_values <- value_calc_results$consumption_lcu_calues




  energy_calc_results <- calorie_calculations(
    processed_data,
    indicator_data,
    calorie_conversions
  )

  processed_data <- energy_calc_results$processed_data
  indicator_data <- energy_calc_results$indicator_data
  extra_outputs$consumption_calorie_values <- energy_calc_results$consumption_kcal_calues

  if ("mean_crop_price_lcu_per_kg" %in% names(prices) & "staple_crop" %in% names(calorie_conversions) & "crop_calories" %in% names(calorie_conversions)) {
    indicator_data <- indicator_data %>% dplyr::left_join(calorie_conversions[["staple_crop"]], by = "id_rhomis_dataset")

    staple_crop_price <- switch_units(indicator_data$staple_crop, unit_tibble = prices[["mean_crop_price_lcu_per_kg"]], id_vector = indicator_data[["id_rhomis_dataset"]])
    staple_crop_energy <- switch_units(indicator_data$staple_crop, unit_tibble = calorie_conversions[["crop_calories"]], id_vector = indicator_data[["id_rhomis_dataset"]])

    staple_crop_kcal_per_lcu <- staple_crop_energy / staple_crop_price
    indicator_data$staple_crop_kcal_per_lcu <- staple_crop_kcal_per_lcu
  }

  gender_calc_results <- gender_control_summary(
    processed_data = processed_data,
    indicator_data = indicator_data,
    gender_categories = gender_categories
  )
  processed_data <- gender_calc_results$processed_data
  indicator_data <- gender_calc_results$indicator_data
  extra_outputs$gender_control <- gender_calc_results$gender_outputs

  results <- list()
  results$processed_data <- processed_data
  results$indicator_data <- indicator_data
  results$extra_outputs <- extra_outputs

  return(results)
}

#' Run preliminary calculations
#'
#' Rpackage file: redirectModules.R
#'
#' @param rhomis_data A tibble of rhomis_data
#' @param gender_categories The gender categories present in the data which is to be processed
#' @param units A nested list containing all of the unit conversion tibbles
#' @return
#' @export
#'
#' @examples
run_preliminary_calculations <- function(rhomis_data,
                                         gender_categories = pkg.env$gender_categories,
                                         units
                                         ) {


  rhomis_data <- replace_crop_and_livestock_other(rhomis_data)
  indicator_data <- make_new_dataset(rhomis_data)

  results <- list()
  prices <- list()
  crop_outputs <- list()
  livestock_outputs <- list()

  number_crop_loops <- find_number_of_loops(rhomis_data, "crop_name")
  crop_loops <- paste0("crop_name_", 1:number_crop_loops)

  # Checks if columns are missing, if columns do not exist then they are returned
  crop_name_in_data <- check_columns_in_data(rhomis_data, loop_columns = "crop_name")

  if (length(crop_name_in_data) == 0) {
    rhomis_data[crop_loops] <- switch_units(rhomis_data[crop_loops],
      unit_tibble = units$crop_name_to_std,
      rhomis_data[["id_rhomis_dataset"]]
    )
  }

  number_livestock_loops <- find_number_of_loops(rhomis_data, "livestock_name")
  livestock_loops <- paste0("livestock_name_", 1:number_livestock_loops)

  # Checks if columns are missing, if columns do not exist then they are returned
  livestock_name_in_data <- check_columns_in_data(rhomis_data, loop_columns = "livestock_name")

  if (length(livestock_name_in_data) == 0) {
    rhomis_data[livestock_loops] <- switch_units(rhomis_data[livestock_loops],
      unit_tibble = units$livestock_name_to_std,
      id_vector = rhomis_data[["id_rhomis_dataset"]]
    )
  }

  # Make sure "other" units are considered
  rhomis_data <- replace_units_with_other_all(rhomis_data)

  # Converting the country column
  # into a two letter iso country code
  # using a country conversion table
  if ("country_to_iso2" %in% names(units)) {
    # indicator_search_id_rhomis_dataset
    indicator_data$iso_country_code <- toupper(switch_units(
      data_to_convert = rhomis_data$country,
      unit_tibble = units$country_to_iso2,
      id_vector = rhomis_data[["id_rhomis_dataset"]]
    ))
    # Provide this warning if the user has not converted their country names
    if (all(is.na(units$country_to_iso2)) | all(is.na(indicator_data$iso_country_code))) {
      warning(paste0(
        "\nHave not provided the ISO country codes for the survey. \nCheck the country names, and check that they are converted",
        "\n---------------------------------------------"
      ))
    }


    if ("start_time_user" %in% colnames(rhomis_data)) {
      if ("year" %in% colnames(rhomis_data)) {
        # indicator_search_year
        indicator_data$year <- rhomis_data$year
      } else {
        indicator_data$year <- substr(rhomis_data$start_time_user, start = 1, stop = 4)
      }

      if (any(!is.na(indicator_data$iso_country_code)) & any(!is.na(indicator_data$year))) {
        # indicator_search_currency_conversion_lcu_to_ppp
        indicator_data <- convert_all_currencies(indicator_data, country_column = "iso_country_code", year_column = "year")
        indicator_data <- dplyr::rename(indicator_data, currency_conversion_lcu_to_ppp = conversion_factor)
        indicator_data <- dplyr::rename(indicator_data, currency_conversion_factor_year = conversion_year)
      }
    }

    missing_columns <- check_columns_in_data(rhomis_data,
      individual_columns = c("start_time_user", "end_time_user"),
      warning_message = "Could not calculate length of survey"
    )
    if (length(missing_columns) == 0) {
      # indicator_search_survey_length_minutes
      indicator_data$survey_length_minutes <- as.POSIXct(rhomis_data[["end_time_user"]], optional = T) - as.POSIXct(rhomis_data[["start_time_user"]], optional = T)
      indicator_data$survey_length_minutes <- as.character(indicator_data$survey_length_minutes)
    }
  }


  #---------------------------------------------------------------
  # Conduct Calculations
  #---------------------------------------------------------------

  ###############
  # Crop calculations
  ###############

  rhomis_data <- crop_calculations_all(rhomis_data,
    crop_yield_units_conv_tibble = units$crop_amount_to_kg,
    crop_income_units_conv_tibble = units$crop_price_to_lcu_per_kg,
    gender_categories = gender_categories
  )


  crop_columns <- c(
    "crop_harvest_kg_per_year",
    "crop_consumed_kg_per_year",
    "crop_sold_kg_per_year",
    "crop_income_per_year",
    "crop_price"
  )


  missing_crop_columns <- check_columns_in_data(rhomis_data,
    loop_columns = crop_columns
  )
  if (length(missing_crop_columns) >= 0 & length(missing_crop_columns) < length(crop_columns)) {
    columns_to_widen <- crop_columns[crop_columns %in% missing_crop_columns == F]
    crop_data <- map_to_wide_format(
      data = rhomis_data,
      name_column = "crop_name",
      column_prefixes = columns_to_widen,
      types = rep("num", length(columns_to_widen))
    )

    if ("crop_price" %in% names(crop_data)) {
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



  ###############
  # Livestock calculations
  ###############

  livestock_weight_kg <- make_per_project_conversion_tibble(proj_id_vector = rhomis_data[["id_rhomis_dataset"]], unit_conv_tibble = units$livestock_weight_kg)

  rhomis_data <- livestock_calculations_all(rhomis_data,
    livestock_weights_conv_tibble = units$livestock_weight_kg,
    eggs_amount_unit_conv_tibble = units$eggs_amount_to_pieces_per_year,
    #Why is egg price not included
    honey_amount_unit_conv_tibble = units$honey_amount_to_l,
    milk_amount_unit_conv_tibble = units$milk_amount_to_l,
    milk_price_time_unit_conv_tibble = units$milk_price_to_lcu_per_l,
    gender_categories = gender_categories
    # Need to add livestock weights to the conversions sheets
  )


    livestock_loop_columns <- c(
    "livestock_sold",
    "livestock_sale_income",
    "livestock_price_per_animal",
    "meat_kg_per_year",
    "meat_consumed_kg_per_year",
    "meat_sold_kg_per_year",
    "meat_sold_income",
    "meat_price_per_kg",
    "milk_collected_litres_per_year",
    "milk_consumed_litres_per_year",
    "milk_sold_litres_per_year",
    "milk_sold_income_per_year",
    "milk_price_per_litre",
    "eggs_collected_kg_per_year",
    "eggs_consumed_kg_per_year",
    "eggs_sold_kg_per_year",
    "eggs_income_per_year",
    "eggs_price_per_kg",
    "bees_honey_kg_per_year",
    "bees_honey_consumed_kg_per_year",
    "bees_honey_sold_kg_per_year",
    "bees_honey_sold_income",
    "bees_honey_price_per_kg"
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

  if (length(missing_livestock_columns) >= 0 & length(missing_livestock_columns) < length(livestock_loop_columns)) {
    columns_to_widen <- livestock_loop_columns[livestock_loop_columns %in% missing_livestock_columns == F]
    livestock_data <- map_to_wide_format(
      data = rhomis_data,
      name_column = "livestock_name",
      column_prefixes = columns_to_widen,
      types = rep("num", length(columns_to_widen))
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


    data_to_bind <- make_new_dataset(rhomis_data)
    livestock_data <- lapply(livestock_data, function(x) {
      dplyr::bind_cols(data_to_bind, x)
    })
  }

  if (length(missing_livestock_columns) == length(livestock_loop_columns)) {
    livestock_data <- NULL
    warning("No extra outputs generated for livestock loops")
  }

  # Livestock TLU

  livestock_heads_columns <- grep("livestock_heads_", colnames(rhomis_data))

  if (length(livestock_heads_columns) == 0) {
    warning("Unable to calculate livestock TLU, no 'livestock_heads' columns")
  } else {
    #indicator_search_livestock_tlu
    livestock_tlu_conv <- make_per_project_conversion_tibble(rhomis_data[["id_rhomis_dataset"]],livestock_count_to_tlu)
    data <- clean_tlu_column_names(rhomis_data, units$livestock_name_to_std,livestock_tlu_conv)
    indicator_data$livestock_tlu <- livestock_tlu_calculations(rhomis_data, units$livestock_name_to_std, livestock_tlu_conv)
  }

  ###############
  # Demographics
  ###############
  household_size_conversions <- get_household_size_conversion()
  if (length(check_columns_in_data(rhomis_data, "hh_pop_rep_num")) == 0 |
    any(names(household_size_conversions) %in% colnames(rhomis_data))) {
    indicator_data$hh_size_members <- calculate_household_size_members(rhomis_data)
  }
  if (length(check_columns_in_data(rhomis_data, "hh_pop_rep_num")) == 0 |
    any(names(household_size_conversions) %in% colnames(rhomis_data))) {
    indicator_data$hh_size_mae <- calculate_MAE(rhomis_data)
  }



  if ("household_type" %in% colnames(rhomis_data) == T) {
    indicator_data$household_type <- rhomis_data[["household_type"]]
  }
  if ("household_type" %in% colnames(rhomis_data) == F) {
    warning('"household_type" does not exist in dataset')
  }

  if ("education_level" %in% colnames(rhomis_data) == T) {
    indicator_data$head_education_level <- rhomis_data[["education_level"]]
  }
  if ("education_level" %in% colnames(rhomis_data) == F) {
    warning('"education_level" does not exist in dataset')
  }


  ###############
  # Land use
  ###############


  indicator_data <- dplyr::bind_cols(indicator_data, land_size_calculation(rhomis_data, unit_conv_tibble = units$land_area_to_ha))
  indicator_data <- dplyr::rename(indicator_data, land_cultivated_ha = land_cultivated)
  indicator_data <- dplyr::rename(indicator_data, land_owned_ha = land_owned)



  ###############
  # Food security
  ###############
  if ("food_worst_month" %in% colnames(rhomis_data) == T) {
    indicator_data$worst_food_security_month <- rhomis_data[["food_worst_month"]]
  }
  if ("food_worst_month" %in% colnames(rhomis_data) == F) {
    warning('"food_worst_month" does not exist in dataset')
  }

  if ("food_best_month" %in% colnames(rhomis_data) == T) {
    indicator_data$best_food_security_month <- rhomis_data[["food_best_month"]]
  }
  if ("food_best_month" %in% colnames(rhomis_data) == F) {
    warning('"food_best_month" does not exist in dataset')
  }

  indicator_data <- dplyr::bind_cols(indicator_data, food_security_calculations(rhomis_data))

  ###############
  # Dietary diversity
  ###############

  hdds_data <- hdds_calc(rhomis_data)
  if (ncol(hdds_data)>0 & nrow(hdds_data)==nrow(indicator_data))
  {
  indicator_data <- dplyr::bind_cols(indicator_data, hdds_data)
  }
  #---------------------------------------------------------------
  # Totals
  #---------------------------------------------------------------

  missing_columns <- check_columns_in_data(rhomis_data,
    loop_columns = "crop_income_per_year",
    warning_message = "Could not calculate crop income"
  )
  if (length(missing_columns) == 0) {
    indicator_data$crop_income_lcu_per_year <- total_crop_income(rhomis_data)
  }

  indicator_data$livestock_income_lcu_per_year <- total_livestock_income(rhomis_data)


  if (!is.null(indicator_data$crop_income_lcu_per_year) & !is.null(indicator_data$livestock_income_lcu_per_year) &
    "offfarm_income_proportion" %in% colnames(rhomis_data) &
    "offfarm_incomes_any" %in% colnames(rhomis_data)) {
    total_and_off_farm_income <- total_and_off_farm_incomes(rhomis_data,
      total_crop_income = indicator_data$crop_income_lcu_per_year,
      total_livestock_income = indicator_data$livestock_income_lcu_per_year
    )
    indicator_data$total_income_lcu_per_year <- total_and_off_farm_income$total_income
    indicator_data$off_farm_income_lcu_per_year <- total_and_off_farm_income$off_farm_income

    rhomis_data <- gendered_off_farm_income_split(rhomis_data, gender_categories = gender_categories)
  }

  # Off farm incomes

  off_farm_columns <- c("offfarm_income_name", "offfarm_year_round", "offfarm_month", "offfarm_who_control_revenue")

  missing_off_farm_columns <- check_columns_in_data(rhomis_data,
    loop_columns = off_farm_columns
  )
  if (length(missing_off_farm_columns) >= 0 &
    length(missing_off_farm_columns) < length(off_farm_columns) &
    "offfarm_income_name" %in% missing_off_farm_columns == F) {
    columns_to_widen <- off_farm_columns[off_farm_columns %in% missing_off_farm_columns == F]
    off_farm_data <- map_to_wide_format(
      data = rhomis_data,
      name_column = "offfarm_income_name",
      column_prefixes = columns_to_widen,
      types = rep("chr", length(columns_to_widen))
    )

    data_to_bind <- make_new_dataset(rhomis_data)
    off_farm_data <- lapply(off_farm_data, function(x) {
      dplyr::bind_cols(data_to_bind, x)
    })
  }

  if (length(missing_off_farm_columns) == length(off_farm_columns) |
    "offfarm_income_name" %in% missing_off_farm_columns) {
    off_farm_data <- NULL
    warning("No extra outputs generated for off-farm loops")
  }

  results$indicator_data <- indicator_data
  results$processed_data <- rhomis_data
  results$crop_data <- crop_data
  results$livestock_data <- livestock_data
  results$off_farm_data <- off_farm_data
  results$original_prices <- prices

  return(results)
}


#' Read Folder of CSVs
#'
#' A function
#'
#' Rpackage file: redirectModules.R
#'
#' @param folder The folder containing the tables to load
#'
#' @return
#' @export
#'
#' @examples
read_folder_of_csvs <- function(folder = "./") {
  tables <- grep(".csv", list.files(folder), value = T)
  if (length(tables) == 0) {
    warning(paste0("No csvs in folder ", folder))
    return(list())
  }

  table_name <- gsub(".csv", "", tables, fixed = T)
  results <- sapply(table_name, function(x) {
    path <- paste0(folder, x, ".csv")
    readr::read_csv(path, show_col_types = FALSE)
  }, simplify = F)

  return(results)
}
