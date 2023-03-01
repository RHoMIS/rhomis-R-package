
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
#' @param calorie_conversions A list of calorie conversions for different products
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
        calorie_conversions,
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

        rhomis_data$iso_country_code <-  indicator_data$iso_country_code
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

    # Land Area
    missing_columns <- check_columns_in_data(rhomis_data,
                                             individual_columns = c("landcultivated"),
                                             warning_message = "Could not calculate land cultivated"
    )
    if (length(missing_columns) == 0) {
        indicator_data <- dplyr::bind_cols(indicator_data, land_size_calculation(rhomis_data, unit_conv_tibble = units_and_conversions$land_area_to_ha))

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

        if (is.null(units_and_conversions$livestock_count_to_tlu)){
            units_and_conversions$livestock_count_to_tlu <- make_per_project_conversion_tibble(rhomis_data$id_rhomis_dataset,livestock_count_to_tlu)
        }
        units_and_conversions$livestock_count_to_tlu <- units_and_conversions$livestock_count_to_tlu[duplicated(units_and_conversions$livestock_count_to_tlu)==F,]
        rhomis_data <- clean_tlu_column_names(rhomis_data, units_and_conversions$livestock_name_to_std,units_and_conversions$livestock_count_to_tlu)
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

    if ("foodshortagetime_months_which" %in% colnames(rhomis_data) ==T){
        food_shortage_months <- split_string_categories_to_dummy(rhomis_data$foodshortagetime_months_which, seperator = " ")
        na_rows <- rep(FALSE, nrow(rhomis_data))
        if ("na" %in% colnames(food_shortage_months)){
            na_rows <- food_shortage_months$na
            food_shortage_months$na <- NULL
        }
        number_hungry_months <- rowSums(food_shortage_months, na.rm=F)
        number_hungry_months[na_rows] <- NA
        indicator_data$nr_months_food_shortage <- number_hungry_months
    }
    if ("foodshortagetime_months_which" %in% colnames(rhomis_data) ==F){
        warning('"foodshortagetime_months_which" does not exist in dataset, cannot calculate number of hungry months')

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

    if ("iso_country_code" %in% colnames(rhomis_data)){


        ppi_result <- ppi_score(rhomis_data,country_code_column = rhomis_data$iso_country_code)

        if (!is.null(ppi_result)){
            if(nrow(ppi_result)==nrow(indicator_data)){
                indicator_data <- indicator_data %>% dplyr::bind_cols(ppi_result)
            }

        }

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





    rhomis_data <- ntfp_calories_and_values(tree_aid_df = rhomis_data,
                                            price_conversions = prices,
                                            calorie_conversions =calorie_conversions)


    indicator_data <- ntfp_totals(rhomis_data,
                                  indicator_data,
                                  fp_products = fp_products)


    results <- value_gender_fa_calculations(
        processed_data = rhomis_data,
        indicator_data = indicator_data,
        calorie_conversions = calorie_conversions,
        prices = prices,
        gender_categories = gender_categories
    )


    results$crop_data <- crop_data
    results$livestock_data <- livestock_data
    results$off_farm_data <- off_farm_data




    return(results)
}

#' Calculate all of the Local Indicators
#'
#' @param base_path Tha folder where the analysis is being conducted
#' @param file_path The direct file path to the indicator file
#' @param id_type The type of ID being provided ("string" or "column")
#' @param proj_id The project id string or column name
#' @param form_id The form id string or column name
#' @param gender_categories The gender categories to examine
#' @param unique_id_col  The column containing unique household ids
#' @param hh_id_col The column containing household ids to use, could be the same as unique_id_col. Household IDs can be linked to older longitudinal surveys when merging datasets.
#' @param overwrite Whether or not to overwrite household ids
#'
#' @return
#' @export
#'
#' @examples
calculate_indicators_local <- function(
        base_path="./",
        file_path,
        id_type=c("string", "column"),
        proj_id,
        form_id,
        gender_categories = pkg.env$gender_categories,
        unique_id_col = "_uuid",
        hh_id_col = NULL,
        overwrite=F

){

    # indicator_data <- read_folder_of_csvs(folder = paste0(base_path, "indicator_data/"))[[1]]
    # Read raw data
    rhomis_data <- load_rhomis_csv(
        file_path = file_path,
        id_type = id_type,
        proj_id = proj_id,
        form_id = form_id,
        unique_id_col = unique_id_col,
        hh_id_col = hh_id_col,
        overwrite=overwrite
    )


    units_and_conversions <- load_local_units(paste0( base_path,"conversions_stage_1/"), id_rhomis_dataset = rhomis_data[["id_rhomis_dataset"]])



    secondary_units <- sapply(names(pkg.env$secondary_units), function(unit_name){
        file_name <- paste0(base_path,"conversions_stage_2/",unit_name,".csv")

        if (file.exists(file_name)){
            return(readr::read_csv(file_name,  col_types = readr::cols()))
        }
    }, simplify = F)

    units_and_conversions <- c(units_and_conversions, secondary_units)

    prices <- sapply(pkg.env$price_conversion_list, function(unit_name){
        file_name <- paste0(base_path,"conversions_stage_2/",unit_name,".csv")

        if (file.exists(file_name)){
            return(readr::read_csv(file_name,col_types = readr::cols()))
        }
    }, simplify = F)

    calorie_conversions <- sapply(pkg.env$calorie_conversion_list, function(unit_name){
        file_name <- paste0(base_path,"conversions_stage_2/",unit_name,".csv")

        if (file.exists(file_name)){
            return(readr::read_csv(file_name, col_types = readr::cols()))
        }
    }, simplify = F)







    # Read in units and conversions

    # Read in Secondary Units

    # Read in prices

    # Read in calories

    results <- calculate_indicators(
        rhomis_data,
        units_and_conversions,
        prices,
        calorie_conversions,
        gender_categories)



    lapply(names(results), function(x) {
        data_to_write <- results[[x]]
        if(length(data_to_write)==0){
            return()
        }
        if (x == "processed_data" | x == "indicator_data") {
            new_folder <- paste0(base_path, x)
            dir.create(new_folder, showWarnings = F)

            path <- paste0(new_folder, "/", x, ".csv")
            readr::write_csv(data_to_write, path)
            return()
        }




        if (x == "extra_outputs" |
            x == "crop_data" |
            x == "livestock_data" |
            x == "off_farm_data" ) {
            new_folder <- paste0(base_path, x)

            write_list_of_df_to_folder(list_of_df = data_to_write, folder = new_folder)
        }






    })







    return(results)


}

#' Calculate Indicators Server
#'
#' Rpackage file: 03-calculate-indicators.R
#'
#' @param central_url The url of the ODK-central
#' @param central_email The email of the ODK-central account being used
#' @param central_password The password of the ODK-central account being used
#' @param project_name The name of the ODK-central project being processed
#' @param form_name The name of the ODK-central form being processed
#' @param central_test_case This flag is used for running a test-sample dataset from ODK the inst/sample_central_project/ folder
#' @param database The name of the database you would like to save results to
#' @param isDraft Whether or not the ODK form you are working with is a draft
#' or a final version. Only relevant if you are processing a project from ODK central
#' @param gender_categories Which gender groups to consider
#' @param base_folder Folder where to store zipped outputs
#'
#' @return
#' @export
#'
#' @examples
calculate_indicators_server <- function(
        central_url,
        central_email,
        central_password,
        project_name,
        form_name,
        database,
        isDraft,
        central_test_case = FALSE,
        gender_categories = pkg.env$gender_categories,
        base_folder="~/rhomis_datasets/"
){
    raw_data <- load_rhomis_central(
        central_url=central_url,
        central_email=central_email,
        central_password=central_password,
        project_name=project_name,
        form_name=form_name,
        database=database,
        isDraft=isDraft,
        central_test_case=central_test_case
    )

    rhomis_data <- raw_data

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

    secondary_units <- sapply(names(pkg.env$secondary_units), function(unit_name){
        secondary_unit <- extract_units_from_db(database,
                                                url = "mongodb://localhost",
                                                projectID = project_name,
                                                formID = form_name,
                                                conversion_type = unit_name,
                                                collection = "units_and_conversions"
        )
        if (!is.null(secondary_unit)){
            if ("conversion" %in% colnames(secondary_unit)){
                secondary_unit$conversion <- as.numeric(secondary_unit$conversion)

            }

        }
        return(secondary_unit)
    }, simplify = F)

    units_and_conversions <- c(units_and_conversions, secondary_units)

    prices <- sapply(pkg.env$price_conversion_list, function(unit_name){
        price <- extract_units_from_db(database,
                                       url = "mongodb://localhost",
                                       projectID = project_name,
                                       formID = form_name,
                                       conversion_type = unit_name,
                                       collection = "units_and_conversions"
        )
        if (!is.null(price)){
            if ("conversion" %in% colnames(price)){
                price$conversion <- as.numeric(price$conversion)

            }

        }
        return(price)


    }, simplify = F)

    calorie_conversions <- sapply(pkg.env$calorie_conversion_list, function(unit_name){
        calorie_converison <- extract_units_from_db(database,
                                                    url = "mongodb://localhost",
                                                    projectID = project_name,
                                                    formID = form_name,
                                                    conversion_type = unit_name,
                                                    collection = "units_and_conversions"
        )

        if (!is.null(calorie_converison)){
            if ("conversion" %in% colnames(calorie_converison)){
                calorie_converison$conversion <- as.numeric(calorie_converison$conversion)
            }
        }
        return(calorie_converison)


    }, simplify = F)











    # Read raw data

    # Read in units and conversions

    # Read in Secondary Units

    # Read in prices

    # Read in calories

    results <- calculate_indicators(
        rhomis_data,
        units_and_conversions,
        prices,
        calorie_conversions,
        gender_categories
    )


    clean_project_name <- tolower(project_name)
    clean_project_name <- trimws(clean_project_name)
    clean_project_name <- gsub(" ", "_", clean_project_name, fixed=T)
    clean_project_name <- gsub("[^_[:^punct:]]", "_", clean_project_name,perl=T)

    clean_form_name <- tolower(form_name)
    clean_form_name <- trimws(clean_form_name)
    clean_form_name <- gsub(" ", "_", clean_form_name, fixed=T)
    clean_form_name <- gsub("[^_[:^punct:]]", "_", clean_form_name,perl=T)
    clean_form_name <- paste0(clean_project_name,"_",clean_form_name)

    dir.create(base_folder,showWarnings = F)
    dir.create(paste0(base_folder,
                      clean_project_name,
                      "/"),
               showWarnings = F)
    dir.create(paste0(base_folder,
                      clean_project_name,
                      "/",
                      clean_form_name,
                      "/"),showWarnings = F)

    dir.create(paste0(base_folder,
                      clean_project_name,
                      "/",
                      clean_form_name,
                      "/"),showWarnings = F)


    # final_zi

    readme_string <- write_docs(project_name, form_name)






    readr::write_file(readme_string,paste0(base_folder,
                                           clean_project_name,
                                           "/",
                                           clean_form_name,
                                           "/README.txt"))




    if (central_test_case==F){

        projects <- get_projects(
            central_url,
            central_email,
            central_password
        )
        projectID <- projects$id[projects$name == project_name]


        # Get central formID
        forms <- get_forms(
            central_url,
            central_email,
            central_password,
            projectID
        )
        formID <- forms$xmlFormId[forms$name == form_name]

        get_xls_form(central_url = central_url,
                     central_email = central_email,
                     central_password = central_password,
                     projectID =projectID ,
                     formID = formID,
                     isDraft = isDraft,
                     file_destination =  paste0(base_folder,
                                                clean_project_name,
                                                "/",
                                                clean_form_name,
                                                "/",clean_form_name,"_survey.xls"))
    }


    indicator_table_mapping <- indicator_mapping_to_df()

    dir.create(paste0(base_folder,
                      clean_project_name,
                      "/",
                      clean_form_name,
                      "/indicator_explanations/"),showWarnings = F)
    readr::write_csv(indicator_table_mapping, paste0(base_folder,clean_project_name,"/",clean_form_name,"/indicator_explanations/indicator_explanations.csv"))




    dir.create(paste0(base_folder,
                      clean_project_name,
                      "/",
                      clean_form_name,
                      "/raw_data"),showWarnings = F)
    readr::write_csv(raw_data, paste0(base_folder,clean_project_name,"/",clean_form_name,"/raw_data/raw_data.csv"))



    new_folder <- paste0(base_folder,
                         clean_project_name,
                         "/",
                         clean_form_name,
                         "/conversions_stage_1/")
    write_list_of_df_to_folder(list_of_df = units_and_conversions[names(units_and_conversions) %in% pkg.env$unit_file_names], folder = new_folder)


    new_folder <- paste0(base_folder,
                         clean_project_name,
                         "/",
                         clean_form_name,
                         "/conversions_stage_2/")
    write_list_of_df_to_folder(list_of_df = c(secondary_units,prices, calorie_conversions), folder = new_folder)

    lapply(names(results), function(x) {
        data_to_write <- results[[x]]
        if(length(data_to_write)==0){
            return()
        }

        if (x == "processed_data") {
            save_data_set_to_db(
                data = data_to_write,
                data_type = "processedData",
                database = database,
                url = "mongodb://localhost",
                projectID = project_name,
                formID = form_name
            )
            dir.create(paste0(base_folder,clean_project_name,"/",clean_form_name,"/processed_data/"),showWarnings = F)
            readr::write_csv(data_to_write, paste0(base_folder,clean_project_name,"/",clean_form_name,"/processed_data/processed_data.csv"))


            return()
        }

        if (x == "indicator_data") {
            save_data_set_to_db(
                data = data_to_write,
                data_type = "indicatorData",
                database = database,
                url = "mongodb://localhost",
                projectID = project_name,
                formID = form_name
            )

            dir.create(paste0(base_folder,clean_project_name,"/",clean_form_name,"/indicator_data/"),showWarnings = F)
            readr::write_csv(data_to_write, paste0(base_folder,clean_project_name,"/",clean_form_name,"/indicator_data/indicator_data.csv"))

            return()
        }


        if (x == "extra_outputs" |
            x == "crop_data" |
            x == "livestock_data" |
            x == "off_farm_data" ) {
            save_list_of_df_to_db(
                list_of_df = data_to_write,
                projectID = project_name,
                formID = form_name,
                database = database,
                url = "mongodb://localhost"
            )

            new_folder <- paste0(base_folder,clean_project_name,"/",clean_form_name,"/",x)
            write_list_of_df_to_folder(list_of_df = data_to_write, folder = new_folder)
        }
        set_project_tag_to_true(database = database,
                                url = url,
                                projectID=project_name,
                                formID=form_name,
                                project_tag="finalIndicators")

    })

    parent_folder <-  paste0(base_folder,clean_project_name,"/")
    form_folder <- paste0("../",clean_form_name)

    folder_path <- paste0(base_folder,clean_project_name,"/",clean_form_name,"/")
    zip_output_path <- paste0(base_folder,clean_project_name,"/",clean_form_name,".zip")

    command <- paste0("cd ",parent_folder,";"," zip -r ",zip_output_path, " *")

    system(command)

    command <- paste0("rm -rf ",folder_path)

    system(command)

    connection <- connect_to_db("projectData", database, "mongodb://localhost")
    connection$update(paste0('{"projectID":"', project_name, '","formID":"', form_name, '"}'),
                      paste0('{"$set":{"zip_file_path": ', '"', zip_output_path,  '"}}')
    )
    connection$disconnect()


    return(results)


}
