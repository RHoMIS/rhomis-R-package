library(readr)
library(dplyr)
library(tibble)


#' Make new dataset
#'
#' @param rhomis_data A rhomis_dataset including IDs
#'
#' @return
#' @export
#'
#' @examples
make_new_dataset <- function(rhomis_data){
    if (
        "id_unique" %in% colnames(rhomis_data) |
        "id_hh" %in% colnames(rhomis_data) |
        "id_rhomis_dataset" %in% colnames(rhomis_data) |
        "id_form" %in% colnames(rhomis_data) |
        "id_proj" %in% colnames(rhomis_data)
    )
    return(
        rhomis_data[c("id_unique","id_hh","id_rhomis_dataset","id_form","id_proj")]
    )
}

#' Calculate prices and indicator Local
#'
#' Calculate prices and initial indicators locally
#'
#' @param base_path Path to the project folder
#' @param units_path Path to the folder of converted units
#'
#' @return
#' @export
#'
#' @examples
calculate_prices_and_indicator_local <- function(data,base_path="./", units_path="./converted_units/"){

    load_units_csvs(units_path)

    results <- run_preliminary_calculations(data)

    lapply(names(results), function(x){
        new_folder <- paste0(base_path,x)
        dir.create(new_folder, showWarnings = F)

        data_to_write <- results[[x]]

        if ( any(class(data_to_write)=="tbl_df") | any(class(data_to_write)=="tbl") | any(class(data_to_write)=="data.frame")){
            file_path <- paste0(new_folder,"/", x,".csv")
            readr::write_csv(data_to_write,file = file_path)
        }
        if (class(data_to_write)=="list"){

        write_list_of_df_to_folder(list_of_df = data_to_write,folder = new_folder)
        }
    })

    converted_prices_folder <- paste0(base_path,"converted_prices")
    if (!dir.exists(converted_prices_folder))
    {
    dir.create(converted_prices_folder, showWarnings = F)
    data_to_write <- results[["prices"]]
    write_list_of_df_to_folder(list_of_df = data_to_write,folder = converted_prices_folder)
    }


    return(results)



}

#' Run preliminary calculations
#'
#' @param rhomis_data A tibble of rhomis_data
#'
#' @return
#' @export
#'
#' @examples
run_preliminary_calculations <- function(rhomis_data){

    results <- list()

    rhomis_data <- replace_crop_and_livestock_other(rhomis_data)
    indicator_data <- make_new_dataset(rhomis_data)

    prices <- list()
    crop_outputs <- list()
    livestock_outputs <- list()

    number_crop_loops <- find_number_of_loops(rhomis_data,"crop_name")
    crop_loops <- paste0("crop_name_",1:number_crop_loops)

    # Checks if columns are missing, if columns do not exist then they are returned
    crop_name_in_data <- check_columns_in_data(rhomis_data, loop_columns = "crop_name")

    if(length(crop_name_in_data)==0)
    {
        rhomis_data[crop_loops] <- switch_units(rhomis_data[crop_loops],unit_tibble = crop_name_conversions,
                                                rhomis_data[["id_rhomis_dataset"]])
    }

    number_livestock_loops <- find_number_of_loops(rhomis_data,"livestock_name")
    livestock_loops <- paste0("livestock_name_",1:number_livestock_loops)

    # Checks if columns are missing, if columns do not exist then they are returned
    livestock_name_in_data <- check_columns_in_data(rhomis_data, loop_columns = "livestock_name")

    if(length(livestock_name_in_data)==0)
    {
        rhomis_data[livestock_loops] <- switch_units(rhomis_data[livestock_loops],
                                                     unit_tibble=livestock_name_conversions,
                                                     id_vector = rhomis_data[["id_rhomis_dataset"]])
    }

    # Make sure "other" units are considered
    rhomis_data <- replace_units_with_other_all(rhomis_data)

    if (exists("country_conversions")){

        indicator_data$iso_country_code <- toupper(switch_units(data_to_convert = rhomis_data$country,
                                                                unit_tibble = country_conversions,
                                                                id_vector = rhomis_data[["id_rhomis_dataset"]]))
        if (all(is.na(country_conversions$conversion)) | all(is.na(indicator_data$iso_country_code))){
            warning(paste0("\nHave not provided the ISO country codes for the survey. \nCheck the country names, and check that they are converted",
                           "\n---------------------------------------------"))
        }


        if ("start_time_user" %in% colnames(rhomis_data)){
            indicator_data$year <- substr(rhomis_data$start_time_user, start = 1,stop = 4)

            if (any(!is.na(indicator_data$iso_country_code)) & any(!is.na(indicator_data$year)))
            {
                indicator_data<- convert_all_currencies(indicator_data,country_column="iso_country_code", year_column="year")
            }

        }

        missing_columns <- check_columns_in_data(rhomis_data,
                                                 individual_columns = c("start_time_user","end_time_user"),
                                                 warning_message = "Could not calculate length of survey")
        if (length(missing_columns)==0){
            # Survey

            indicator_data$survey_length_minutes <- as.POSIXct(rhomis_data[["end_time_user"]])-as.POSIXct(rhomis_data[["start_time_user"]])
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
                                         crop_yield_units_conv_tibble = crop_yield_unit_conversions,
                                         crop_income_units_conv_tibble = crop_price_unit_conversions)


    crop_columns <- c("crop_harvest_kg_per_year",
                      "crop_consumed_kg_per_year",
                      "crop_sold_kg_per_year",
                      "crop_income_per_year",
                      "crop_price")


    missing_crop_columns <-check_columns_in_data(rhomis_data,
                                                 loop_columns=crop_columns)
    if(length(missing_crop_columns)>=0 & length(missing_crop_columns) < length(crop_columns)){
        columns_to_widen <- crop_columns[crop_columns %in% missing_crop_columns==F]
        crop_data <- map_to_wide_format(
            data = rhomis_data,
            name_column = "crop_name",
            column_prefixes = columns_to_widen,
            types = rep("num", length(columns_to_widen))
        )

        if ("crop_price"%in% names(crop_data))
        {
            crop_price <- crop_data[["crop_price"]]
            crop_price$id_rhomis_dataset <- rhomis_data[["id_rhomis_dataset"]]
            crop_price <- crop_price %>% dplyr::mutate_all(replace_infinite) %>% dplyr::group_by(id_rhomis_dataset )%>%  dplyr::summarise_all(mean, na.rm = TRUE)
            prices$mean_crop_price_lcu_per_kg <- crop_price
        }

        data_to_bind <- make_new_dataset(rhomis_data)
        crop_data <- lapply(crop_data, function(x){
            dplyr::bind_cols(data_to_bind, x)
        })



    }

    if(length(missing_crop_columns)==length(crop_columns)){
        crop_data <- NULL
        warning("No extra outputs generated for livestock loops")
    }



    ###############
    # Livestock calculations
    ###############

    livestock_weights <- make_per_project_conversion_tibble(proj_id_vector = rhomis_data[["id_rhomis_dataset"]],unit_conv_tibble = livestock_weights)

    rhomis_data <- livestock_calculations_all(rhomis_data,
                                              livestock_weights_conv_tibble = livestock_weights,
                                              eggs_amount_unit_conv_tibble = eggs_unit_conversion,
                                              eggs_price_time_units_conv_tibble = eggs_price_unit_conversion,
                                              honey_amount_unit_conv_tibble = honey_unit_conversion,
                                              milk_amount_unit_conv_tibble = milk_unit_conversion,
                                              milk_price_time_unit_conv_tibble = milk_price_unit_conversion
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
        "eggs_price_per_kg"
    )

    price_datasets <- c(
        "livestock_price_per_animal",
        "meat_price_per_kg",
        "milk_price_per_litre",
        "eggs_price_per_kg"
    )

    missing_livestock_columns <-check_columns_in_data(rhomis_data,
                                                      loop_columns=livestock_loop_columns,
                                                      warning_message = "Could not write extra outputs for these columns")

    if(length(missing_livestock_columns)>=0 & length(missing_livestock_columns) < length(livestock_loop_columns)){
        columns_to_widen <- livestock_loop_columns[livestock_loop_columns %in% missing_livestock_columns==F]
        livestock_data <- map_to_wide_format(
            data = rhomis_data,
            name_column = "livestock_name",
            column_prefixes = columns_to_widen,
            types = rep("num", length(columns_to_widen))
        )

        for (price_data_set in price_datasets){
            if (price_data_set%in% names(livestock_data))
            {
                price_df <- livestock_data[[price_data_set]]
                price_df$id_rhomis_dataset <- rhomis_data[["id_rhomis_dataset"]]
                mean_price_df <- price_df %>% dplyr::mutate_all(replace_infinite) %>% dplyr::group_by(id_rhomis_dataset )%>%  dplyr::summarise_all(mean, na.rm = TRUE)
                prices[[paste0("mean_",price_data_set)]] <- mean_price_df
            }

        }


        data_to_bind <- make_new_dataset(rhomis_data)
        livestock_data <- lapply(livestock_data, function(x){
            dplyr::bind_cols(data_to_bind, x)
        })

    }

    if(length(missing_livestock_columns)==length(livestock_loop_columns)){
        livestock_data <- NULL
        warning("No extra outputs generated for livestock loops")
    }



    ###############
    # Demographics
    ###############

    if(length(check_columns_in_data(rhomis_data,"hh_pop_rep_num"))==0)
    {
        indicator_data$hh_size_members <- calculate_household_size_members(rhomis_data)
    }
    if(length(check_columns_in_data(rhomis_data,"hh_pop_rep_num"))==0)
    {
        indicator_data$hh_size_MAE <- calculate_MAE(rhomis_data)
    }

    if ("household_type"%in%colnames(rhomis_data)==T)
    {
        indicator_data$household_type <- rhomis_data[["household_type"]]
    }
    if ("household_type"%in%colnames(rhomis_data)==F)
    {
        warning('"household_type" does not exist in dataset')
    }

    if ("education_level"%in%colnames(rhomis_data)==T)
    {
        indicator_data$head_education_level <- rhomis_data[["education_level"]]

    }
    if ("education_level"%in%colnames(rhomis_data)==F)
    {
        warning('"education_level" does not exist in dataset')
    }


    ###############
    # Land use
    ###############

    if (all(c("unitland","landcultivated","landowned")%in%colnames(rhomis_data)))
    {
        indicator_data <- dplyr::bind_cols(indicator_data, land_size_calculation(rhomis_data, unit_conv_tibble = land_unit_conversion))
    }

    ###############
    # Food security
    ###############
    if ("food_worst_month"%in%colnames(rhomis_data)==T)
    {
        indicator_data$worst_food_security_month <- rhomis_data[["food_worst_month"]]
    }
    if ("food_worst_month"%in%colnames(rhomis_data)==F)
    {
        warning('"food_worst_month" does not exist in dataset')
    }

    if ("food_best_month"%in%colnames(rhomis_data)==T)
    {
        indicator_data$best_food_security_month <- rhomis_data[["food_best_month"]]
    }
    if ("food_best_month"%in%colnames(rhomis_data)==F)
    {
        warning('"food_best_month" does not exist in dataset')
    }

    indicator_data <- dplyr::bind_cols(indicator_data, food_security_calculations(rhomis_data))

    ###############
    # Dietary diversity
    ###############

    hdds_data <- hdds_calc(rhomis_data)
    indicator_data <- dplyr::bind_cols(indicator_data,hdds_data)

    #---------------------------------------------------------------
    # Totals
    #---------------------------------------------------------------

    missing_columns <- check_columns_in_data(rhomis_data,loop_columns = "crop_income_per_year",
                                             warning_message = "Could not calculate crop income")
    if(length(missing_columns)==0){
        indicator_data$crop_income <- total_crop_income(rhomis_data)
    }

    indicator_data$livestock_income <- total_livestock_income(rhomis_data)


    if (!is.null(indicator_data$crop_income) & !is.null(indicator_data$livestock_income) & "offfarm_income_proportion" %in% colnames(rhomis_data)){
        total_and_off_farm_income <- total_and_off_farm_incomes(rhomis_data,
                                                                total_crop_income = indicator_data$crop_income,
                                                                total_livestock_income = indicator_data$livestock_income
        )
        indicator_data$total_income <- total_and_off_farm_income$total_income
        indicator_data$off_farm_income <- total_and_off_farm_income$off_farm_income

        rhomis_data <- gendered_off_farm_income_split(rhomis_data)
    }

    # Off farm incomes

    off_farm_columns <- c("offfarm_income_name","offfarm_year_round", "offfarm_month", "offfarm_who_control_revenue")

    missing_off_farm_columns <-check_columns_in_data(rhomis_data,
                                                     loop_columns=off_farm_columns)
    if(length(missing_off_farm_columns)>=0 & length(missing_off_farm_columns) < length(off_farm_columns) & "offfarm_income_name" %in% missing_off_farm_columns==F){
        columns_to_widen <- off_farm_columns[off_farm_columns %in% missing_off_farm_columns==F]
        off_farm_data <- map_to_wide_format(
            data = rhomis_data,
            name_column = "offfarm_income_name",
            column_prefixes = columns_to_widen,
            types = rep("chr", length(columns_to_widen))
        )

        data_to_bind <- make_new_dataset(rhomis_data)
        off_farm_data <- lapply(off_farm_data, function(x){
            dplyr::bind_cols(data_to_bind, x)
        })


    }

    if(length(missing_off_farm_columns)==length(off_farm_columns)){
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
