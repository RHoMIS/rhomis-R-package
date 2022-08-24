
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
        secondary_units,
        prices,
        calories,
        gender_categories
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
    indicator_data <- make_new_dataset(rhomis_data)



    # Check for the number of crop loops
    # and identify the "crop_name" columns
    number_crop_loops <- find_number_of_loops(rhomis_data, "crop_name")
    crop_loops <- paste0("crop_name_", 1:number_crop_loops)

    # Checks if columns are missing, if columns do not exist then they are returned
    crop_name_in_data <- check_columns_in_data(rhomis_data, loop_columns = "crop_name")

    # If there are no "crop_name" columns missing. Take the entries
    # in crop_name and replace them using crop_name conversions
    if (length(crop_name_in_data) == 0 & "crop_name_to_std" %in% names(units_and_conversions)) {
        rhomis_data[crop_loops] <- switch_units(rhomis_data[crop_loops],
                                                unit_tibble = units_and_conversions$crop_name_to_std,
                                                rhomis_data[["id_rhomis_dataset"]]
        )
    }

    # Also check through livestock loops and replace conversions
    number_livestock_loops <- find_number_of_loops(rhomis_data, "livestock_name")
    livestock_loops <- paste0("livestock_name_", 1:number_livestock_loops)

    # Checks if columns are missing, if columns do not exist then they are returned
    livestock_name_in_data <- check_columns_in_data(rhomis_data, loop_columns = "livestock_name")

    if (length(livestock_name_in_data) == 0 & "livestock_name_to_std" %in% names(units_and_conversions)) {

        rhomis_data[livestock_loops] <- switch_units(rhomis_data[livestock_loops],
                                                     unit_tibble = units_and_conversions$livestock_name_to_std,
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
        crop_yield_units_conv_tibble = units_and_conversions$crop_amount_to_kg,
        crop_income_units_conv_tibble = units_and_conversions$crop_price_to_lcu_per_kg,
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

        data_to_bind <- make_new_dataset(rhomis_data)
        crop_data <- lapply(crop_data, function(x) {
            dplyr::bind_cols(data_to_bind, x)
        })
    }

    if (length(missing_crop_columns) == length(crop_columns)) {
        crop_data <- NULL
        warning("No extra outputs generated for crop loops")
    }




    livestock_weights <- make_per_project_conversion_tibble(proj_id_vector = rhomis_data[["id_rhomis_dataset"]], unit_conv_tibble = units_and_conversions$livestock_weights)



    rhomis_data <- livestock_calculations_all(rhomis_data,
                                              livestock_weights_conv_tibble = units_and_conversions$livestock_weights_kg,
                                              eggs_amount_unit_conv_tibble = units_and_conversions$eggs_amount_to_pieces_per_year,
                                              eggs_price_time_units_conv_tibble = units_and_conversions$eggs_price_to_lcu_per_year,
                                              honey_amount_unit_conv_tibble = units_and_conversions$honey_amount_to_l,
                                              milk_amount_unit_conv_tibble = units_and_conversions$milk_amount_to_l,
                                              milk_price_time_unit_conv_tibble = units_and_conversions$milk_price_to_lcu_per_l,
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
        # indicator_search_livestock_tlu
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



    results <- value_gender_fa_calculations(
        processed_data = processed_data,
        indicator_data = indicator_data,
        calorie_conversions = calorie_conversions,
        prices = prices,
        gender_categories = gender_categories,
        units = units_and_conversions
    )

    return(results)
}

calculate_indicators_local <- function(
        base_path="./",
        file_path,
        id_type=c("string", "column"),
        proj_id,
        form_id,
        repeat_columns = pkg.env$repeat_columns,
        gender_categories = pkg.env$gender_categories

){


    # Read raw data

    # Read in units and conversions

    # Read in Secondary Units

    # Read in prices

    # Read in calories


    result <- calculate_indicators(
        rhomis_data,
        units_and_conversions,
        secondary_units,
        prices,
        calories,
        gender_categories
    )

    return(result)


}

calculate_indicators_server <- function(
        central_url,
        central_email,
        central_password,
        project_name,
        form_name,
        database,
        isDraft,
        central_test_case = FALSE,
        repeat_columns = pkg.env$repeat_columns,
        gender_categories = pkg.env$gender_categories
){

    # Read raw data

    # Read in units and conversions

    # Read in Secondary Units

    # Read in prices

    # Read in calories

    result <- calculate_indicators(
        rhomis_data,
        units_and_conversions,
        secondary_units,
        prices,
        calories,
        gender_categories,
        gender_categories

    )

    return(result)


}
