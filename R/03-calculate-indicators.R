
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
    results <- list()



    # Metadata ----------------------------------------------------------------
    indicator_data <- make_new_dataset(rhomis_data)

    if ("country_to_iso2" %in% names(units_and_conversions)) {
        # indicator_search_id_rhomis_dataset
        indicator_data$iso_country_code <- toupper(switch_units(
            data_to_convert = rhomis_data$country,
            unit_tibble = units_and_conversions$country_to_iso2,
            id_vector = rhomis_data[["id_rhomis_dataset"]]
        ))
        # Provide this warning if the user has not converted their country names
        if (all(is.na(units_and_conversions$country_to_iso2)) | all(is.na(indicator_data$iso_country_code))) {
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



    # Crops and Livestock ----------------------------------------------------------------

    rhomis_data <- crop_and_livestock_calcs_all(
        rhomis_data=rhomis_data,
        units_and_conversions=units_and_conversions,
        gender_categories = gender_categories)[["rhomis_data"]]

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



    # Livestock TLU -----------------------------------------------------------

    livestock_heads_columns <- grep("livestock_heads_", colnames(rhomis_data))

    if (length(livestock_heads_columns) == 0) {
        warning("Unable to calculate livestock TLU, no 'livestock_heads' columns")
    } else if ("livestock_count_to_tlu" %in% names(units_and_conversions)==F){
        warning("Unable to calculate livestock TLU, no 'TLU' conversions provided")
    }else {
        #indicator_search_livestock_tlu
        data <- clean_tlu_column_names(rhomis_data, units_and_conversions$livestock_name_to_std,units_and_conversions$livestock_count_to_tlu)
        indicator_data$livestock_tlu <- livestock_tlu_calculations(rhomis_data, units_and_conversions$livestock_name_to_std, units_and_conversions$livestock_count_to_tlu)
    }

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


    results <- value_gender_fa_calculations(
        processed_data = rhomis_data,
        indicator_data = indicator_data,
        calorie_conversions = calorie_conversions,
        prices = prices,
        gender_categories = gender_categories
    )

    results$indicator_data <- indicator_data
    results$processed_data <- rhomis_data
    results$crop_data <- crop_data
    results$livestock_data <- livestock_data
    results$off_farm_data <- off_farm_data

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

    # indicator_data <- read_folder_of_csvs(folder = paste0(base_path, "indicator_data/"))[[1]]
    # Read raw data
    rhomis_data <- load_rhomis_csv(
        file_path = file_path,
        id_type = id_type,
        proj_id = proj_id,
        form_id = form_id,
        repeat_columns=repeat_columns
    )

    units_and_conversions <- load_local_units(paste0( "./conversions_stage_1/"), id_rhomis_dataset = rhomis_data[["id_rhomis_dataset"]])

    secondary_units <- sapply(names(pkg.env$secondary_units), function(unit_name){
        file_name <- paste0(base_path,"conversions_stage_2/",unit_name,".csv")

        if (file.exists(file_name)){
            return(readr::read_csv(file_name))
        }
    }, simplify = F)

    units_and_conversions <- c(units_and_conversions, secondary_units)

    prices <- sapply(pkg.env$price_conversion_list, function(unit_name){
        file_name <- paste0(base_path,"conversions_stage_2/",unit_name,".csv")

        if (file.exists(file_name)){
            return(readr::read_csv(file_name))
        }
    }, simplify = F)

    calorie_conversions <- sapply(pkg.env$calorie_conversion_list, function(unit_name){
        file_name <- paste0(base_path,"conversions_stage_2/",unit_name,".csv")

        if (file.exists(file_name)){
            return(readr::read_csv(file_name))
        }
    }, simplify = F)







    # Read in units and conversions

    # Read in Secondary Units

    # Read in prices

    # Read in calories

    result <- calculate_indicators(
        rhomis_data,
        units_and_conversions,
        prices,
        calories,
        gender_categories)



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
    rhomis_data <- load_rhomis_central(
        central_url=central_url,
        central_email=central_email,
        central_password=central_password,
        project_name=project_name,
        form_name=form_name,
        database=database,
        isDraft=isDraft,
        central_test_case=central_test_case,
        repeat_columns=repeat_columns
    )

    unit_list <- find_db_units(
        projectID = project_name,
        formID = form_name,
        url = "mongodb://localhost",
        collection = "projectData",
        database = database
    )
    units_and_conversions <- load_all_db_units(unit_list,
                               projectID = project_name,
                               formID = form_name,
                               database = database,
                               id_rhomis_dataset = rhomis_data[["id_rhomis_dataset"]]
    )

    prices <- sapply(pkg.env$price_conversion_list, function(unit_name){
        extract_units_from_db(database,
                              url = "mongodb://localhost",
                              projectID = project_name,
                              formID = form_name,
                              conversion_type = unit_name,
                              collection = "units_and_conversions"
        )

    }, simplify = F)

    calorie_conversions <- sapply(pkg.env$calorie_conversion_list, function(unit_name){
        extract_units_from_db(database,
                              url = "mongodb://localhost",
                              projectID = project_name,
                              formID = form_name,
                              conversion_type = unit_name,
                              collection = "units_and_conversions"
        )

    }, simplify = F)











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