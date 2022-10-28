
crop_and_livestock_calcs_all <- function(
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

    missing_crop_columns <- check_columns_in_data(rhomis_data,
                                                  loop_columns = "crop_price",
                                                  warning_message = "Could not export crop prices"
    )

    if (length(missing_crop_columns)==0)
    {

        # Identifying which crops were grown in which project
        crops_per_project <- map_to_wide_format(
            data = rhomis_data,
            name_column = "crop_name",
            column_prefixes = "crop_name",
            types = rep("chr")
        )
        crops_per_project <- crops_per_project[["crop_name"]]
        crops_per_project$id_rhomis_dataset <- rhomis_data[["id_rhomis_dataset"]]

        crops_per_project <- crops_per_project %>% tidyr::pivot_longer(!id_rhomis_dataset, names_to = "survey_value", values_to = "conversion")
        crops_per_project <- crops_per_project[!is.na(crops_per_project$conversion),]
        crops_per_project$conversion <- NULL
        crops_per_project <- crops_per_project[duplicated(crops_per_project)==F,]

        # Getting Prices Out
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

        crop_price <- crops_per_project %>%
            merge(crop_price, by.x=c("id_rhomis_dataset","survey_value"),
                  by.y=c("id_rhomis_dataset","survey_value"),
                  all.x = T,
                  all.y = F)



        prices$mean_crop_price_lcu_per_kg <- crop_price

        data_to_bind <- make_new_dataset(rhomis_data)
        crop_data <- lapply(crop_data, function(x) {
            dplyr::bind_cols(data_to_bind, x)
        })
    }else{
        crop_price <- tibble::as_tibble(list(
            unit_type="crop_price_lcu_per_kg",
            survey_value=NA,
            conversion=NA
        ))

        crop_price <- make_per_project_conversion_tibble(rhomis_data$id_rhomis_dataset, crop_price)
    }


    # results <- run_preliminary_calculations(
    #             rhomis_data = rhomis_data,
    #             gender_categories = gender_categories,
    #             units = units
    #         )


    if ("livestock_weights" %in% names(units_and_conversions)==F){
        livestock_weights <- make_per_project_conversion_tibble(proj_id_vector = rhomis_data[["id_rhomis_dataset"]], unit_conv_tibble = livestock_weight_kg)
    }else{
        livestock_weights <-   units_and_conversions$livestock_weights
    }

    rhomis_data <- livestock_calculations_all(rhomis_data,
                                              livestock_weights_conv_tibble = livestock_weights,
                                              eggs_amount_unit_conv_tibble = units_and_conversions$eggs_amount_to_pieces_per_year,
                                              eggs_price_time_units_conv_tibble = units_and_conversions$eggs_price_to_lcu_per_year,
                                              honey_amount_unit_conv_tibble = units_and_conversions$honey_amount_to_l,
                                              milk_amount_unit_conv_tibble = units_and_conversions$milk_amount_to_l,
                                              milk_price_time_unit_conv_tibble = units_and_conversions$milk_price_to_lcu_per_l,
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
                                                       loop_columns = price_datasets,
                                                       warning_message = "Could not get prices for these products"
    )



    for (price_data_set in price_datasets) {

        if (price_data_set %in% missing_livestock_columns==T){

                livestock_price <- tibble::as_tibble(list(
                    unit_type= paste0("mean_", price_data_set),
                    survey_value=NA,
                    conversion=NA
                ))
                livestock_price <- make_per_project_conversion_tibble(rhomis_data$id_rhomis_dataset, crop_price)

                prices[[paste0("mean_", price_data_set)]] <- livestock_price

            next()
        }

        livestock_per_project <- map_to_wide_format(
            data = rhomis_data,
            name_column = "livestock_name",
            column_prefixes = "livestock_name",
            types = rep("chr")
        )
        livestock_per_project <- livestock_per_project[["livestock_name"]]
        livestock_per_project$id_rhomis_dataset <- rhomis_data[["id_rhomis_dataset"]]

        livestock_per_project <- livestock_per_project %>% tidyr::pivot_longer(!id_rhomis_dataset, names_to = "survey_value", values_to = "conversion")
        livestock_per_project <- livestock_per_project[!is.na(livestock_per_project$conversion),]
        livestock_per_project$conversion <- NULL
        livestock_per_project <- livestock_per_project[duplicated(livestock_per_project)==F,]

        livestock_data <- map_to_wide_format(
            data = rhomis_data,
            name_column = "livestock_name",
            column_prefixes = price_data_set,
            types = rep("num")
        )
        if (price_data_set %in% names(livestock_data)) {
            price_df <- livestock_data[[price_data_set]]
            price_df$id_rhomis_dataset <- rhomis_data[["id_rhomis_dataset"]]
            mean_price_df <- price_df %>%
                dplyr::mutate_all(replace_infinite) %>%
                dplyr::group_by(id_rhomis_dataset) %>%
                dplyr::summarise_all(mean, na.rm = TRUE)

            mean_price_df <- mean_price_df %>% tidyr::pivot_longer(!id_rhomis_dataset, names_to = "survey_value", values_to = "conversion")
            mean_price_df$unit_type<- paste0("mean_", price_data_set)

            mean_price_df <- livestock_per_project %>%
                merge(mean_price_df, by.x=c("id_rhomis_dataset","survey_value"),
                      by.y=c("id_rhomis_dataset","survey_value"),
                      all.x = T,
                      all.y = F)

            prices[[paste0("mean_", price_data_set)]] <- mean_price_df
        }

    }










    # Adding Extra units to convert
    # TLU Conversions
    # Calorie Conversions

    calorie_conversions_dfs <- check_existing_calorie_conversions(rhomis_data)
    calorie_conversions_dfs$staple_crop <- make_per_project_conversion_tibble(proj_id_vector = rhomis_data[["id_rhomis_dataset"]], unit_conv_tibble = list(
        "staple_crop" = c("maize")
    ))




    # Livestock Weights, TLU conversions, and anything
    # else that relies
    secondary_units <- extract_secondary_units(
        units_and_conversions

    )






    # Adding NTFP calculations
    rhomis_data <- fp_calculations_all(tree_aid_df = rhomis_data,
                                       units_and_conversions = units_and_conversions

    )
    ntfp_prices_and_calories <- extract_fp_price_and_calorie_conv(tree_aid_df = rhomis_data)
    prices <- c(prices,ntfp_prices_and_calories$prices)
    calorie_conversions_dfs <- c(calorie_conversions_dfs,ntfp_prices_and_calories$calorie_conversions)



    # Assemble all outputs ready to write to file
    results <- list(
        rhomis_data=rhomis_data,
        prices=prices,
        calorie_conversions=calorie_conversions_dfs,
        secondary_conversions=secondary_units
    )











    return(results)
}


#' Get Secondary Conversions
#'
#' Rpackage file: 02-calculate-prices.R
#'
#' @param rhomis_data A "raw" rhomis dataset
#' @param units_and_conversions The units and conversions table (usually already loaded)
#' @param gender_categories The gender categories for the survey
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

    results <- crop_and_livestock_calcs_all(
        rhomis_data=rhomis_data,
        units_and_conversions=units_and_conversions,
        gender_categories = gender_categories)

    return(results)



}








#' Calculate Prices CSV
#'
#' @param file_path Filepath to a RHoMIS Survey CSV
#' @param base_path The folder where you want to save any outputs, usually your current working directory ("./")
#' @param id_type RHoMIS surveys have form and project IDs. Sometimes the form and project IDs are included as a column in the dataset (id_type="column"), or the IDs are specified by the user at the point of processing (id_type="string")
#' @param proj_id If ID type was string, this should be a string, if ID type was column, this should be a column name containing project IDs
#' @param form_id If ID type was string, this should be a string, if ID type was column, this should be a column name containing form IDs
#' @param unique_id_col The column containing unique household ids
#'
#' Rpackage file: 02-calculate-prices.R
#'
#' @return
#' @export
#'
#' @examples
calculate_prices_csv <- function(
        base_path="./",
        file_path,
        id_type=c("string", "column"),
        proj_id,
        form_id,
        unique_id_col = "_uuid"

){

    rhomis_data <- load_rhomis_csv(
        file_path = file_path,
        id_type = id_type,
        proj_id = proj_id,
        form_id = form_id,
        unique_id_col = unique_id_col
    )


    units <- load_local_units(paste0( base_path,"conversions_stage_1/"), id_rhomis_dataset = rhomis_data[["id_rhomis_dataset"]])

    secondary_units <- get_secondary_conversions(
        rhomis_data=rhomis_data,
        units_and_conversions=units,
        gender_categories = pkg.env$gender_categories)


    write_list_of_df_to_folder(list_of_df = secondary_units$calorie_conversions,
                               folder = paste0(base_path, ".original_stage_2_conversions"))
    write_list_of_df_to_folder(list_of_df = secondary_units$calorie_conversions,
                               folder = paste0(base_path, "conversions_stage_2"),
                               converted_values = T)

    write_list_of_df_to_folder(list_of_df = secondary_units$prices,
                               folder = paste0(base_path, ".original_stage_2_conversions"))
    write_list_of_df_to_folder(list_of_df = secondary_units$prices,
                               folder = paste0(base_path, "conversions_stage_2"),
                               converted_values = T)

    write_list_of_df_to_folder(list_of_df = secondary_units$secondary_conversions,
                               folder = paste0(base_path, ".original_stage_2_conversions"))
    write_list_of_df_to_folder(list_of_df = secondary_units$secondary_conversions,
                               folder = paste0(base_path, "conversions_stage_2"),
                               converted_values = T)

    return(secondary_units)


}


#' Calculate_prices_server
#'
#' Rpackage file: 02-calculate-prices.R
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
#'
#' @return
#' @export
#'
#' @examples
calculate_prices_server <- function(
        central_url,
        central_email,
        central_password,
        project_name,
        form_name,
        database,
        isDraft,
        central_test_case = FALSE
){


    # Load the RHoMIS Dataset from ODK central
    rhomis_data <- load_rhomis_central(
        central_url=central_url,
        central_email=central_email,
        central_password=central_password,
        project_name=project_name,
        form_name=form_name,
        database=database,
        isDraft=isDraft,
        central_test_case=central_test_case
    )

    save_data_set_to_db(
        data = rhomis_data,
        data_type = "rawData",
        database = database,
        url = "mongodb://localhost",
        projectID = project_name,
        formID = form_name
    )

    unit_list <- find_db_units(
        projectID = project_name,
        formID = form_name,
        url = "mongodb://localhost",
        collection = "projectData",
        database = database
    )
    units <- load_all_db_units(unit_list,
                               projectID = project_name,
                               formID = form_name,
                               database = database,
                               id_rhomis_dataset = rhomis_data[["id_rhomis_dataset"]]
    )

    secondary_units <- get_secondary_conversions(
        rhomis_data=rhomis_data,
        units_and_conversions=units,
        gender_categories = pkg.env$gender_categories)

    save_multiple_conversions(
        database = database,
        url = "mongodb://localhost",
        projectID = project_name,
        formID = form_name,
        conversion_data = secondary_units$prices,
        conversion_types = names(secondary_units$prices),
        collection="units_and_conversions",
        converted_values=T
    )

    save_multiple_conversions(
        database = database,
        url = "mongodb://localhost",
        projectID = project_name,
        formID = form_name,
        conversion_data = secondary_units$prices,
        conversion_types = names(secondary_units$prices),
        collection = "unmodified_units"
    )

    save_multiple_conversions(
        database = database,
        url = "mongodb://localhost",
        projectID = project_name,
        formID = form_name,
        conversion_data = secondary_units$calorie_conversions,
        conversion_types = names(secondary_units$calorie_conversions),
        collection="units_and_conversions",
        converted_values=T

    )

    save_multiple_conversions(
        database = database,
        url = "mongodb://localhost",
        projectID = project_name,
        formID = form_name,
        conversion_data = secondary_units$calorie_conversions,
        conversion_types = names(secondary_units$calorie_conversions),
        collection = "unmodified_units"
    )


    save_multiple_conversions(
        database = database,
        url = "mongodb://localhost",
        projectID = project_name,
        formID = form_name,
        conversion_data = secondary_units$secondary_conversions,
        conversion_types = names(secondary_units$secondary_conversions),
        collection="units_and_conversions",
        converted_values=T

    )

    save_multiple_conversions(
        database = database,
        url = "mongodb://localhost",
        projectID = project_name,
        formID = form_name,
        conversion_data = secondary_units$secondary_conversions,
        conversion_types = names(secondary_units$secondary_conversions),
        collection = "unmodified_units"
    )

    set_project_tag_to_true(database = database,
                            url =  "mongodb://localhost",
                            projectID=project_name,
                            formID=form_name,
                            project_tag="pricesCalculated")
    return(secondary_units)
}
