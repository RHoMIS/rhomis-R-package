

#' Convert NTFP Units
#'
#' Switch Out NTFP units for numeric
#' conversion factors
#'
#' @param unit_data The columns in the raw data containing units
#' @param units_conversions The list of unit conversion tibbles
#'
#' @return
#' @export
#'
#' @examples
convert_ntfp_units <- function(
        unit_data,
        units_conversions

){
    # Lapply applies functions in parallel, unlike
    # loops which work sequentially
    converted_data <- lapply(unit_data, function(x) {

        # A table containing your units
        household_data_tibble <- tibble::as_tibble(
            list(
                survey_value = x
            )
        )

        # Joining the two tables to match conversion factors
        converted_data <- dplyr::left_join(household_data_tibble,
                                           units_conversions,
                                           by = c(

                                                   "id_rhomis_dataset" =
                                                       "id_rhomis_dataset",
                                                   "survey_value" = "survey_value"

                                           )
        )

        # Extracting the conversion factor only
        return(converted_data[["conversion"]])
    }) %>% dplyr::bind_cols()

    return(converted_data)
}



#' Calculate fp Harvest
#'
#' Calculate the amount of forest products harvested in KG
#'
#' @param tree_aid_df A tree aid data frame
#' @param fp_harvest_conversions The conversion table for NTFP harvests
#' @param name_column The column containing the name of the product
#' @param amount_column The column containing the amount
#' @param unit_column The column containing the survey unit
#'
#' @return
#' @export
#'
#' @examples
calculate_fp_harvest <- function(
        tree_aid_df,
        fp_harvest_conversions,
        name_column,
        amount_column,
        unit_column

){
    #Checking whether the columns are in the dataset
    missing_columns <- check_columns_in_data(tree_aid_df,
                                             loop_columns = c(name_column, amount_column,unit_column),
                                             warning_message = "Cannot Calculate NTFP fruit harvested"
    )


    # If the columns are missing, simply return the dataset
    if (length(missing_columns)!=0){
        return(tree_aid_df)
    }


    # Find the number of loops on forest products
    number_of_fp_loops <- find_number_of_loops(tree_aid_df,"fp_name") #

    # Identifying the columns I need to use
    fp_harvested_columns <- paste0(amount_column,"_", c(1:number_of_fp_loops))
    fp_harvested_unit_columns <- paste0(unit_column,"_", c(1:number_of_fp_loops))

    # Subsetting the data, getting those columns out
    fp_harvest_data <- tree_aid_df[fp_harvested_columns]
    fp_harvest_units_data <- tree_aid_df[fp_harvested_unit_columns]

    # Converting the units for those columns
    fp_harvest_units_converted <- switch_units(data_to_convert = fp_harvest_units_data,id_vector = tree_aid_df$id_rhomis_dataset,unit_tibble = fp_harvest_conversions)


    # Multiplying the units and the amounts
    fp_harvest_kg <- fp_harvest_data*fp_harvest_units_converted

    new_column_name <- paste0(amount_column,"_kg")

    colnames(fp_harvest_kg) <- paste0(new_column_name,"_", c(1:number_of_fp_loops))

    tree_aid_df <- add_column_after_specific_column(
        data = tree_aid_df,
        new_data = fp_harvest_kg,
        new_column_name = new_column_name,
        old_column_name = amount_column,
        loop_structure = T
    )


    return(tree_aid_df)

}


#' FP Proportions All
#'
#' Calculate numeric proportions NTFPs sold and consumed
#'
#' @param tree_aid_df A tree aid df
#' @param use The use that you want to identify proportions for
#' @param use_column The column containing the use
#' @param prop_column The column containing the proportions
#' @param new_column_name The new column name you want to produce
#'
#' @return
#' @export
#'
#' @examples
fp_proportions_all <-  function(
        tree_aid_df,
        use,
        use_column,
        prop_column,
        new_column_name
){
    #Checking whether the columns are in the dataset
    missing_columns <- check_columns_in_data(tree_aid_df,
                                             loop_columns = c(use_column, prop_column),
                                             warning_message = "Cannot calculate numeric NTFP proportions"
    )

    # If the columns are missing, simply return the dataset
    if (length(missing_columns)!=0){
        return(tree_aid_df)
    }

    # Find loop number
    number_of_loops <- find_number_of_loops(tree_aid_df, use_column)

    # Calculate the numeric proprtions
    ntfp_proportions_numeric <- sapply(c(1:number_of_loops), function(x) proportions_calculation(tree_aid_df, use = use, use_column =use_column, prop_column = prop_column, loop_number = x))
    colnames(ntfp_proportions_numeric) <- paste0(new_column_name, "_", c(1:number_of_loops))

    # Add back into the original dataset
    ntfp_proportions_numeric <- tibble::as_tibble(ntfp_proportions_numeric)
    tree_aid_df <- add_column_after_specific_column(
        data = tree_aid_df,
        new_data = ntfp_proportions_numeric,
        new_column_name = new_column_name,
        old_column_name = prop_column,
        loop_structure = T
    )

    return(tree_aid_df)
}


#' NTFP sold and consumed Calculation
#'
#' Calculate the amounts of NTFPs sold and
#' consumed in KG
#' @param data The dataset
#' @param fp_harvest_kg The column containing ntfp harvests
#' @param fp_amount_sold_kg The column to be created for fp sold
#' @param fp_prop_sold_numeric Numeric proportion columns for fp sold
#' @param fp_amount_consumed_kg Amount consumed columns to be created
#' @param fp_props_process_numeric Proportions processed columns
#' @param fp_amount_process_kg Amount processed columns to be created
#' @param fp_props_process_sold_numeric The proportions processed sold numeric columns
#' @param fp_amount_process_sold_kg The amount processed and and sold in kg (to be created by function)
#' @param fp_prop_process_consumed_numeric The proportion processed consumed numeric
#' @param fp_amount_process_consumed_kg The amount processed and sold in kilograms (column to be created)
#' @param fp_prop_consumed_numeric The numeric proportion of NTFP consumed
#'
#' @return
#' @export
#'
#' @examples
ntfp_sold_and_consumed_calculation <- function(

    data,
    fp_harvest_kg,

    fp_prop_sold_numeric,
    fp_amount_sold_kg,

    fp_prop_consumed_numeric,
    fp_amount_consumed_kg,



    fp_props_process_numeric,
    fp_amount_process_kg,

    fp_props_process_sold_numeric,
    fp_amount_process_sold_kg,

    fp_prop_process_consumed_numeric,
    fp_amount_process_consumed_kg
) {
    # NON-PROCESSED COLUMNS
    # Beginning with ntfp sold
    number_of_loops <- find_number_of_loops(data, name_column = "fp_name")
    harvested_columns <- paste0(fp_harvest_kg, "_", c(1:number_of_loops))
    sold_columns <- paste0(fp_prop_sold_numeric, "_", c(1:number_of_loops))

    if (all(harvested_columns %in% colnames(data)) == F) {
        warning(paste0("Missing Columns:",harvested_columns,". Calculate amounts harvested before calculating amounts sold\n"))
    }
    if (all(sold_columns %in% colnames(data)) == F) {
        warning(paste0("Missing Columns:",sold_columns,". Have not calculated the numeric proportions of amount of non-timber forest products sold. Calculate proportions sold before calculating amounts sold\n"))
    }

    if (all(harvested_columns %in% colnames(data)) == T & all(sold_columns %in% colnames(data)) == T) {


        harvest_data <- data[harvested_columns]
        sold_prop_data <- data[sold_columns]

        amount_sold_kg <- tibble::as_tibble(harvest_data * sold_prop_data)
        colnames(amount_sold_kg) <- paste0(fp_amount_sold_kg, "_", c(1:number_of_loops))

        data <- add_column_after_specific_column(
            data = data,
            new_data = amount_sold_kg,
            new_column_name = fp_amount_sold_kg,
            old_column_name = fp_prop_sold_numeric,
            loop_structure = T
        )
    }

    # Moving on to ntfp consumed
    number_of_loops <- find_number_of_loops(data, name_column = "fp_name")
    harvested_columns <- paste0(fp_harvest_kg, "_", c(1:number_of_loops))
    consumed_columns <- paste0(fp_prop_consumed_numeric, "_", c(1:number_of_loops))


    if (all(harvested_columns %in% colnames(data)) == F) {
        warning(paste0("Missing Columns:",harvested_columns,". Calculate amounts harvested before calculating amounts consumed\n"))
    }
    if (all(consumed_columns %in% colnames(data)) == F) {
        warning(paste0("Missing Columns:",consumed_columns,". Have not calculated the numeric proportions of amount of non-timber forest products consumed Calculate proportions sold before calculating amounts consumed\n"))
    }
    if (all(harvested_columns %in% colnames(data)) == T & all(consumed_columns %in% colnames(data)) == T) {
        harvest_data <- data[harvested_columns]
        consumed_prop_data <- data[consumed_columns]

        amount_consumed_kg <- tibble::as_tibble(harvest_data * consumed_prop_data)
        colnames(amount_consumed_kg) <- paste0(fp_amount_consumed_kg, "_", c(1:number_of_loops))

        data <- add_column_after_specific_column(
            data = data,
            new_data = amount_consumed_kg,
            new_column_name = fp_amount_consumed_kg,
            old_column_name = fp_prop_consumed_numeric,
            loop_structure = T
        )
    }





    # PROCESSED COLUMNS
    # Beginning with ntfp processed
    number_of_loops <- find_number_of_loops(data, name_column = "fp_name")
    harvested_columns <- paste0(fp_harvest_kg, "_", c(1:number_of_loops))
    processed_columns <- paste0(fp_props_process_numeric, "_", c(1:number_of_loops))

    if (all(harvested_columns %in% colnames(data)) == F) {
        warning(paste0("Missing Columns:",harvested_columns,". Have not calculated the amounts harvested in kg. Calculate amounts harvested before calculating amounts processed\n"))
    }
    if (all(processed_columns %in% colnames(data)) == F) {
        warning(paste0("Missing Columns:",processed_columns,". Have not calculated the numeric proportions of amount of non-timber forest products processed. Calculate proportions processed before calculating amounts processed\n"))
    }


    if (all(harvested_columns %in% colnames(data)) == T & all(processed_columns %in% colnames(data)) == T) {

        harvest_data <- data[harvested_columns]
        processed_prop_data <- data[processed_columns]

        amount_processed_kg <- tibble::as_tibble(harvest_data * processed_prop_data)
        colnames(amount_processed_kg) <- paste0(fp_amount_process_kg, "_", c(1:number_of_loops))

        data <- add_column_after_specific_column(
            data = data,
            new_data = amount_processed_kg,
            new_column_name = fp_amount_process_kg,
            old_column_name = fp_props_process_numeric,
            loop_structure = T
        )
    }

    # PROCESSED SOLD
    # Beginning with ntfp processed sold
    number_of_loops <- find_number_of_loops(data, name_column = "fp_name")
    processed_columns <- paste0(fp_amount_process_kg, "_", c(1:number_of_loops))
    processed_sold_columns <- paste0(fp_props_process_sold_numeric, "_", c(1:number_of_loops))

    if (all(processed_columns %in% colnames(data)) == F) {
        warning(paste0("Missing Columns:",processed_columns,". Have not calculated the amounts processed in kg. Calculate amounts processed before calculating amount of processed ntfp which was sold\n"))
    }

    if (all(processed_sold_columns %in% colnames(data)) == F) {
        warning(paste0("Missing Columns:",processed_sold_columns,". Have not calculated the numeric proportions of amount of non-timber forest products processed and sold. Calculate proportions processed and sold before calculating amounts processed and sold\n"))
    }

    if (all(processed_columns %in% colnames(data)) == T & all(processed_sold_columns %in% colnames(data)) == T) {

        processed_data <- data[processed_columns]
        processed_sold_prop_data <- data[processed_sold_columns]

        amount_processed_sold_kg <- tibble::as_tibble(processed_data * processed_sold_prop_data)
        colnames(amount_processed_sold_kg) <- paste0(fp_amount_process_sold_kg, "_", c(1:number_of_loops))

        data <- add_column_after_specific_column(
            data = data,
            new_data = amount_processed_sold_kg,
            new_column_name = fp_amount_process_sold_kg,
            old_column_name = fp_props_process_sold_numeric,
            loop_structure = T
        )
    }


    # PROCESSED CONSUMED
    # Beginning with ntfp processed sold
    number_of_loops <- find_number_of_loops(data, name_column = "fp_name")
    processed_columns <- paste0(fp_amount_process_kg, "_", c(1:number_of_loops))
    processed_consumed_columns <- paste0(fp_prop_process_consumed_numeric, "_", c(1:number_of_loops))

    if (all(processed_columns %in% colnames(data)) == F) {
        warning(paste0("Missing Columns:",processed_columns,". Have not calculated the amounts processed in kg. Calculate amounts processed before calculating amount of processed ntfp which was consumed\n"))
    }

    if (all(processed_consumed_columns %in% colnames(data)) == F) {
        warning(paste0("Missing Columns:",processed_consumed_columns,". Have not calculated the numeric proportions of amount of non-timber forest products processed and consumed Calculate proportions processed and consumed before calculating amounts processed and sold\n"))
    }

    if (all(processed_columns %in% colnames(data)) == T & all(processed_consumed_columns %in% colnames(data)) == T) {

        processed_data <- data[processed_columns]
        processed_consumed_prop_data <- data[processed_consumed_columns]

        amount_processed_consumed_kg <- tibble::as_tibble(processed_data * processed_consumed_prop_data)
        colnames(amount_processed_consumed_kg) <- paste0(fp_amount_process_consumed_kg, "_", c(1:number_of_loops))

        data <- add_column_after_specific_column(
            data = data,
            new_data = amount_processed_consumed_kg,
            new_column_name = fp_amount_process_consumed_kg,
            old_column_name = fp_prop_process_consumed_numeric,
            loop_structure = T
        )
    }




    return(data)
}


#' NTFP Income Calculation
#'
#' Calculate the Income from NTFPs
#' in LCU per year
#'
#' @param data The main Data set
#' @param unit_conv_tibble Conversion table
#' @param fp_sold_kg_per_year_column Column for the forest product sold per year
#' @param fp_sold_units_column Column with the forest product sold units
#' @param fp_sold_income_column Column with the amount of FP sold income
#' @param new_price_column The name of the new price column to be produced
#' @param new_fp_sold_income The name of the new FP income column to be produced
#'
#' @return
#' @export
#'
#' @examples
fp_income_calculations <- function(data,
                                   unit_conv_tibble = NULL,
                                   fp_sold_kg_per_year_column,
                                   fp_sold_units_column, # a column to be created
                                   fp_sold_income_column,
                                   new_fp_sold_income,
                                   new_price_column
) {



    number_of_loops <- find_number_of_loops(data, name_column = "fp_name")

    fp_sold_columns <- paste0(fp_sold_kg_per_year_column, "_", c(1:number_of_loops)) #fruit_amount_sold_kg
    fp_sold_unit_columns <- paste0(fp_sold_units_column, "_", c(1:number_of_loops)) #is this frequency column? (e.g. 'year') #fruit_sold_frequency_1
    fp_sold_income_columns <- paste0(fp_sold_income_column, "_", c(1:number_of_loops)) #fruit_sold_income_1

    if (all(fp_sold_columns %in% colnames(data)) == F) {
        warning(paste0("Have not calculated the amounts sold in kg. Calculate amounts sold before calculating income"))
        return(data)
    }
    if (all(fp_sold_unit_columns %in% colnames(data)) == F) {
        warning(paste0("Have not converted the non-timber forest product price quantity units yet. Convert these units before calculating incomes sold"))
        return(data)

    }



    fp_sold_units_data <- data[fp_sold_unit_columns]
    fp_sold_units_numeric <- switch_units(data_to_convert = fp_sold_units_data,id_vector = data$id_rhomis_dataset,unit_tibble = unit_conv_tibble)



    fp_sold_amount <- data[fp_sold_columns]
    fp_sold_income <- data[fp_sold_income_columns]


    # Multiplying values which do not have "total_income_per_year_unit
    fp_sold_income_per_year <- fp_sold_income %>% dplyr::mutate_all(as.numeric)

    fp_sold_income_per_year <- fp_sold_income_per_year * fp_sold_units_numeric

    fp_sold_income_per_year[fp_sold_amount==0] <- 0


    colnames(fp_sold_income_per_year) <- paste0(new_fp_sold_income, "_", c(1:number_of_loops))
    data <- add_column_after_specific_column(
        data = data,
        new_data = fp_sold_income_per_year,
        new_column_name = new_fp_sold_income,
        old_column_name = fp_sold_income_column,
        loop_structure = T
    )

    fp_price <- fp_sold_income_per_year / fp_sold_amount
    colnames(fp_price) <- paste0(new_price_column, "_", c(1:number_of_loops))

    data <- add_column_after_specific_column(
        data = data,
        new_data = fp_price,
        new_column_name = new_price_column,
        old_column_name = new_fp_sold_income,
        loop_structure = T
    )

    return(data)
}



#' Value or Calorie Calculations item Consumed
#'
#' @param data
#' @param name_column
#' @param amount_consumed_column
#' @param conversion_tibble
#' @param price_column_name
#' @param converted_column_name
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
        amounts_df <- amounts_df  %>% dplyr::mutate_all(as.numeric)



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






#' FP Calculations All
#'
#' Main NTFP calculations
#'
#' @param tree_aid_df Tree aid dataset
#' @param units_and_conversions An object containing a list of conversion tables
#'
#' @return
#' @export
#'
#' @examples
fp_calculations_all <- function(
        tree_aid_df,
        units_and_conversions

){

    for (fp_product in fp_products){

        # Amount Harvested
        tree_aid_df <- calculate_fp_harvest(
            tree_aid_df=tree_aid_df,
            fp_harvest_conversions=units_and_conversions$fp_amount_to_kg,
            name_column=fp_product$fp_name,
            amount_column=fp_product$amount,
            unit_column=fp_product$amount_units
        )

        # Numeric proportions sold
        tree_aid_df <- fp_proportions_all(
            tree_aid_df=tree_aid_df,
            use="sell",
            use_column=fp_product$use_column,
            prop_column=fp_product$sold_prop_column,
            new_column_name=paste0(fp_product$base_name,"_sold_prop_numeric")
        )

        # Numeric proportions consumed
        tree_aid_df <- fp_proportions_all(
            tree_aid_df=tree_aid_df,
            use="eat",
            use_column=fp_product$use_column,
            prop_column=fp_product$consumed_column,
            new_column_name=paste0(fp_product$base_name,"_eaten_prop_numeric")
        )

        # Numeric proportions sold
        tree_aid_df <- fp_proportions_all(
            tree_aid_df=tree_aid_df,
            use="process",
            use_column=fp_product$use_column,
            prop_column=fp_product$processed_column,
            new_column_name=paste0(fp_product$base_name,"_process_prop_numeric")
        )

        # Numeric proportions processed and eaten
        tree_aid_df <- fp_proportions_all(
            tree_aid_df=tree_aid_df,
            use="process",
            use_column=fp_product$use_column,
            prop_column=fp_product$processed_eaten_column,
            new_column_name=paste0(fp_product$base_name,"_process_eaten_prop_numeric")
        )



        # Numeric proportions processed and sold
        tree_aid_df <- fp_proportions_all(
            tree_aid_df=tree_aid_df,
            use="process",
            use_column=fp_product$use_column,
            prop_column=fp_product$processed_sold_column,
            new_column_name=paste0(fp_product$base_name,"_process_sold_prop_numeric")
        )


        # Calculating all amounts sold, consumed, processed, processed and eaten, processed and sold
        tree_aid_df <- ntfp_sold_and_consumed_calculation(

            data=tree_aid_df,
            fp_harvest_kg=paste0(fp_product$amount,"_kg"),

            fp_prop_sold_numeric=paste0(fp_product$base_name,"_sold_prop_numeric"),
            fp_amount_sold_kg=paste0(fp_product$amount,"_sold_kg"),

            fp_prop_consumed_numeric=paste0(fp_product$base_name,"_eaten_prop_numeric"),
            fp_amount_consumed_kg=paste0(fp_product$amount,"_eaten_kg"),

            fp_props_process_numeric=paste0(fp_product$base_name,"_process_prop_numeric"),
            fp_amount_process_kg=paste0(fp_product$amount,"_processed_kg"),

            fp_props_process_sold_numeric=paste0(fp_product$base_name,"_process_sold_prop_numeric"),
            fp_amount_process_sold_kg=paste0(fp_product$amount,"_process_sold_kg"),

            fp_prop_process_consumed_numeric=paste0(fp_product$base_name,"_process_eaten_prop_numeric"),
            fp_amount_process_consumed_kg=paste0(fp_product$amount,"_process_eaten_kg")

        )


        tree_aid_df <- fp_income_calculations(
            data=tree_aid_df,
            unit_conv_tibble = units_and_conversions$fp_income_per_freq_to_lcu_per_year,
            fp_sold_kg_per_year_column=paste0(fp_product$amount,"_sold_kg"),
            fp_sold_units_column=fp_product$income_frequency, # a column to be created
            fp_sold_income_column=fp_product$income_column,
            new_fp_sold_income=paste0(fp_product$base_name,"_sold_income_per_year"),
            new_price_column=paste0(fp_product$base_name,"_price_lcu_per_kg")
        )

        tree_aid_df <- fp_income_calculations(
            data=tree_aid_df,
            unit_conv_tibble = units_and_conversions$fp_income_per_freq_to_lcu_per_year,
            fp_sold_kg_per_year_column=paste0(fp_product$amount,"_process_sold_kg"),
            fp_sold_units_column=fp_product$income_frequency, # a column to be created
            fp_sold_income_column=fp_product$income_column,
            new_fp_sold_income=paste0(fp_product$base_name,"_process_sold_income_per_year"),
            new_price_column=paste0(fp_product$base_name,"_process_price_lcu_per_kg")
        )




    }

    return(tree_aid_df)


}


#' NTFP Calories and Values
#'
#' Function for calculating calories and values for forest products consumed/sold
#'
#' @param tree_aid_df The main NTFP dataframe
#' @param price_conversions List of price conversions
#' @param calorie_conversions List of calorie conversions
#'
#' @return
#' @export
#'
#' @examples
ntfp_calories_and_values <- function(tree_aid_df,
                                     price_conversions,
                                     calorie_conversions){
    for (fp_product in fp_products){
        # Calories consumed

        converion_table_name <- paste0(fp_product$base_name,"_calories_kcal_per_kg")
        if (converion_table_name %in% names(calorie_conversions)){
            if (!is.null(calorie_conversions[[converion_table_name]])){

                if (nrow(calorie_conversions[[converion_table_name]])>0){

                    tree_aid_df <- value_or_calorie_calculations_item_consumed(
                        data = tree_aid_df,
                        name_column = "fp_name",
                        amount_consumed_column = paste0(fp_product$amount,"_eaten_kg"),
                        conversion_tibble = calorie_conversions[[converion_table_name]],
                        price_column_name = paste0(paste0(fp_product$amount,"_calories_kcal_per_kg")),
                        converted_column_name = paste0(paste0(fp_product$amount,"_calories_consumed_kcal_per_year")))
                }
            }
        }



        # Processed Calories consumed
        converion_table_name <- paste0(fp_product$base_name,"_process_calories_kcal_per_kg")

        if (converion_table_name %in% names(calorie_conversions)){
            if (!is.null(calorie_conversions[[converion_table_name]])){

                if (nrow(calorie_conversions[[converion_table_name]])>0){

                    tree_aid_df <- value_or_calorie_calculations_item_consumed(
                        data = tree_aid_df,
                        name_column = "fp_name",
                        amount_consumed_column = paste0(fp_product$amount,"_process_eaten_kg"),
                        conversion_tibble = calorie_conversions[[converion_table_name]],
                        price_column_name = paste0(paste0(fp_product$amount,"_process_calories_kcal_per_kg")),
                        converted_column_name = paste0(paste0(fp_product$amount,"_process_calories_consumed_kcal_per_year")))
                }
            }
        }
        # Value consumed
        converion_table_name <- paste0(fp_product$base_name,"_price_lcu_per_kg")

        if (converion_table_name %in% names(price_conversions)){
            if (!is.null(price_conversions[[converion_table_name]])){

                if (nrow(price_conversions[[converion_table_name]])>0){

                    tree_aid_df <- value_or_calorie_calculations_item_consumed(
                        data = tree_aid_df,
                        name_column = "fp_name",
                        amount_consumed_column = paste0(fp_product$amount,"_eaten_kg"),
                        conversion_tibble = price_conversions[[converion_table_name]],
                        price_column_name = paste0(paste0(fp_product$amount,"_price_lcu_per_kg")),
                        converted_column_name = paste0(paste0(fp_product$amount,"_value_consumed_lcu_per_year")))
                }
            }
        }
        # Processed Value consumed
        converion_table_name <- paste0(fp_product$base_name,"_process_price_lcu_per_kg")

        if (converion_table_name %in% names(price_conversions)){
            if (!is.null(price_conversions[[converion_table_name]])){
                if (nrow(price_conversions[[converion_table_name]])>0){

                    tree_aid_df <- value_or_calorie_calculations_item_consumed(
                        data = tree_aid_df,
                        name_column = "fp_name",
                        amount_consumed_column = paste0(fp_product$amount,"_process_eaten_kg"),
                        conversion_tibble = price_conversions[[converion_table_name]],
                        price_column_name = paste0(paste0(fp_product$amount,"_process_price_lcu_per_kg")),
                        converted_column_name = paste0(paste0(fp_product$amount,"_process_value_consumed_lcu_per_year")))

                }
            }
        }
    }

    return(tree_aid_df)
}

#' NTFP Totals
#'
#' Calculate total ntfp income, value consumed,
#' calories consumed, processed income, value
#' of processed ntfp consumed, processed
#' calories consumed
#'
#' @param tree_aid_df Tree aid data frame
#' @param indicator_df Data frame containing table of indicators
#' @param fp_products List of all of the forest products
#'
#' @return
#' @export
#'
#' @examples
ntfp_totals <- function(tree_aid_df,
                        indicator_df,
                        fp_products=fp_products
){
    number_of_tree_aid_loops <- find_number_of_loops(tree_aid_df,"fp_name")
    if (number_of_tree_aid_loops==0){
        return(indicator_df)
    }
    if (number_of_tree_aid_loops>0){

        ntfp_income <- ntfp_total_individual(tree_aid_df,
                                             fp_products = fp_products,
                                             income = T,
                                             warning_message= "Issue calculating ntfp incomes")

        ntfp_processed_income <- ntfp_total_individual(tree_aid_df,
                                                       fp_products = fp_products,
                                                       processed_income = T,
                                                       warning_message= "Issue calculating ntfp processed incomes")

        total_ntfp_income <- tibble::as_tibble(list(
            ntfp_income=ntfp_income,
            ntfp_processed_income=ntfp_processed_income
        ))
        na_rows <- rowSums(is.na(total_ntfp_income))==ncol(total_ntfp_income)
        total_ntfp_income <- rowSums(total_ntfp_income, na.rm = T)
        total_ntfp_income[na_rows] <- NA



        ntfp_value <- ntfp_total_individual(tree_aid_df,
                                            fp_products = fp_products,
                                            value = T,
                                            warning_message= "Issue calculating ntfp value consumed")

        ntfp_processed_value <- ntfp_total_individual(tree_aid_df,
                                                      fp_products = fp_products,
                                                      processed_value = T,
                                                      warning_message= "Issue calculating ntfp processed value consumed")

        total_ntfp_value <- tibble::as.tibble(list(
            ntfp_value=ntfp_value,
            ntfp_processed_value=ntfp_processed_value
        ))
        na_rows <- rowSums(is.na(total_ntfp_value))==ncol(total_ntfp_value)
        total_ntfp_value <- rowSums(total_ntfp_value, na.rm = T)
        total_ntfp_value[na_rows] <- NA


        ntfp_calories <- ntfp_total_individual(tree_aid_df,
                                               fp_products = fp_products,
                                               calories = T,
                                               warning_message= "Issue calculating ntfp calories consumed")

        ntfp_processed_calories <- ntfp_total_individual(tree_aid_df,
                                                         fp_products = fp_products,
                                                         processed_calories = T,
                                                         warning_message= "Issue calculating ntfp processed calories consumed")

        total_ntfp_calories <- tibble::as.tibble(list(
            ntfp_calories=ntfp_calories,
            ntfp_processed_calories=ntfp_processed_calories
        ))
        na_rows <- rowSums(is.na(total_ntfp_calories))==ncol(total_ntfp_calories)
        total_ntfp_calories <- rowSums(total_ntfp_calories, na.rm = T)
        total_ntfp_calories[na_rows] <- NA

        indicator_df$ntfp_income <- total_ntfp_income
        indicator_df$value_ntfp_consumed <- total_ntfp_value
        indicator_df$ntfp_consumed_calories_kcal_per_hh_per_year <- total_ntfp_calories

        return(indicator_df)
    }

}



#' NTFP Totals
#'
#' Calculate total ntfp income, value consumed,
#' calories consumed, processed income, value
#' of processed ntfp consumed, processed
#' calories consumed
#'
#' @param tree_aid_df A tree aid dataset
#' @param fp_products Forest products
#' @param income Whether to calculate income only
#' @param value Whether to calculate value only
#' @param calories Whether to calculate calories only
#' @param processed_income Whether to calculate processed income only
#' @param processed_value Whether to calculate processed value consumed only
#' @param processed_calories Whether to calculate processed calories consumed only
#' @param warning_message Warning message to give for any missing columns
#'
#' @return
#' @export
#'
#' @examples
ntfp_total_individual <- function(tree_aid_df,
                                  fp_products=fp_products,
                                  income=F,
                                  value=F,
                                  calories=F,
                                  processed_income=F,
                                  processed_value=F,
                                  processed_calories=F,
                                  warning_message
){

    #ntfp_income <- []
    #list <- []

    main_args <- c(income,
                   value,
                   calories,
                   processed_income,
                   processed_value,
                   processed_calories)

    if (sum(main_args)!=1){
        warning("Must only specify one argument as true:\n
                   income, \n
                   value, \n
                   calories,\n
                   processed_income, \n
                   processed_value,\n
                   processed_calories.")

        return(rep(NA, nrow(tree_aid_df)))
    }

    if (income){
        suffix <- "_sold_income_per_year"
    }else if (value){
        suffix <- "_amount_value_consumed_lcu_per_year"
    }else if (calories){
        suffix <- "_amount_calories_consumed_kcal_per_year"
    }else if (processed_income){
        suffix <- "_process_sold_income_per_year"
    }else if (processed_value){
        suffix <- "_amount_process_value_consumed_lcu_per_year"
    }else if (processed_calories){
        suffix <- "_amount_process_calories_consumed_kcal_per_year"
    } else{
        return(rep(NA, nrow(tree_aid_df)))
    }

    # For each product in list
    columns_to_return <- c()
    for (fp_product in fp_products){


        column_base <- paste0(fp_product$base_name, suffix)

        # Check whether the column names in list are in the data and assign warning message if any are missing
        # Create list called missing_columns containing the missing column names
        missing_columns <- check_columns_in_data(
            data = tree_aid_df,
            loop_columns = column_base,
            warning_message = warning_message
        )

        # If no columns are missing, add the column names within list to the total column
        if (length(missing_columns)==0){

            loop_number <- find_number_of_loops(tree_aid_df,column_base)
            total_columns <- paste0(column_base,"_",c(1:loop_number))
            columns_to_return <- c(columns_to_return,total_columns)

        }



    }

    if (length(columns_to_return)>0){

        totals <- rowSums(tree_aid_df[columns_to_return],na.rm=T)
        na_rows <- rowSums(is.na(tree_aid_df[columns_to_return]))==length(columns_to_return)
        totals[na_rows] <- NA
    }else {
        totals <- rep(NA, nrow(tree_aid_df))
    }

    return(totals)
}


#' Extract FP Price and Calorie Values
#'
#' @param tree_aid_df Tree Aid Dataset
#'
#' @return
#' @export
#'
#' @examples
extract_fp_price_and_calorie_conv <- function(tree_aid_df){
    prices <- list()
    calorie_conversions <- list()

    missing_columns <-  check_columns_in_data(tree_aid_df,
                                              loop_columns = "fp_name",
                                              individual_columns = "id_rhomis_dataset",
                                              warning="Won't extract NTFP columns, missing the following column:")


    for (fp_product in fp_products){

        price_column <- paste0(fp_product$base_name,"_price_lcu_per_kg")
        missing_columns <-  check_columns_in_data(tree_aid_df,
                                                  loop_columns = price_column,
                                                  warning="Won't extract NTFP prices, missing the following column:")

        if (length(missing_columns)==0){
            prices_df <- map_to_wide_format(tree_aid_df,name_column = "fp_name", column_prefixes = price_column, types="num")[[1]]
            product_names <- colnames(prices_df)
            prices_df <- colMeans(prices_df, na.rm = T) %>% tibble::as_tibble()
            prices_df$survey_value <- product_names
            colnames(prices_df)[colnames(prices_df)=="value"] <- "conversion"
            prices_df <- make_per_project_conversion_tibble(tree_aid_df$id_rhomis_dataset,prices_df)
            prices_df$conversion_type <- price_column
            prices_df <- prices_df[,c("id_rhomis_dataset","conversion_type","survey_value", "conversion")]

            prices[[price_column]] <- prices_df
        }

        process_price_column <- paste0(fp_product$base_name,"_process_price_lcu_per_kg")
        missing_columns <-  check_columns_in_data(tree_aid_df,
                                                  loop_columns = process_price_column,
                                                  warning="Won't extract NTFP processed prices, missing the following column:")

        if (length(missing_columns)==0){
            processed_prices_df <- map_to_wide_format(tree_aid_df,name_column = "fp_name", column_prefixes = process_price_column, types="num")[[1]]
            product_names <- colnames(processed_prices_df)
            processed_prices_df <- colMeans(processed_prices_df, na.rm = T) %>% tibble::as_tibble()
            processed_prices_df$survey_value <- product_names
            colnames(processed_prices_df)[colnames(processed_prices_df)=="value"] <- "conversion"
            processed_prices_df <- make_per_project_conversion_tibble(tree_aid_df$id_rhomis_dataset,processed_prices_df)
            processed_prices_df$conversion_type <- process_price_column
            processed_prices_df <- processed_prices_df[,c("id_rhomis_dataset","conversion_type","survey_value", "conversion")]

            prices[[process_price_column]] <- processed_prices_df

        }

        all_fp_products <- find_loop_number_and_extract_values(tree_aid_df,"fp_name") %>% tibble::as_tibble()
        colnames(all_fp_products)[colnames(all_fp_products)=="value"] <- "survey_value"

        if (nrow(all_fp_products)>0){
            calorie_column <- paste0(fp_product$base_name,"_calories_kcal_per_kg")
            calorie_conversion <- all_fp_products
            calorie_conversion$conversion <- NA
            calorie_conversion$conversion_type <- calorie_column
            calorie_conversion <- make_per_project_conversion_tibble(tree_aid_df$id_rhomis_dataset,calorie_conversion)
            calorie_conversion <- calorie_conversion[,c("id_rhomis_dataset","conversion_type","survey_value", "conversion")]
            calorie_conversions[[calorie_column]] <- calorie_conversion


            processed_calories_column <- paste0(fp_product$base_name,"_process_calories_kcal_per_kg")
            processed_calorie_conversion <- all_fp_products
            processed_calorie_conversion$conversion <- NA
            processed_calorie_conversion$conversion_type <- processed_calories_column
            processed_calorie_conversion <- make_per_project_conversion_tibble(tree_aid_df$id_rhomis_dataset,processed_calorie_conversion)
            processed_calorie_conversion <- processed_calorie_conversion[,c("id_rhomis_dataset","conversion_type","survey_value", "conversion")]
            calorie_conversions[[processed_calories_column]] <- processed_calorie_conversion



        }




    }

    results <- list(
        prices=prices,
        calorie_conversions=calorie_conversions
    )



    return(results)

}



replace_fp_other_units <- function(tree_aid_df, fp_list){

    looped_units <- list(
        "fruit_amount_units" = "fruit_amount_units_other"
    )

    looped_units_merged <- sapply(names(looped_units), function(x) {
        number_of_loops <- find_number_of_loops(data, x)
        if (number_of_loops > 0) {
            main_column <- paste0(x, "_", 1:number_of_loops)
            other_column <- paste0(looped_units[[x]], "_", 1:number_of_loops)
        }
        if (number_of_loops == 0) {
            main_column <- paste0(x, "_", 1)
            other_column <- paste0(looped_units[[x]], "_", 1)
        }
        setNames(other_column, main_column)
    }, simplify = T)

    looped_units_merged <- unlist(unname(looped_units_merged))

    units_to_change <- c(individual_units, looped_units_merged)


    result <- sapply(colnames(data), function(x) {
        if (x %in% names(units_to_change)) {
            if (units_to_change[[x]] %in% colnames(data)) {
                other_column <- units_to_change[[x]]
                new_column <- replace_unit_column_with_other_single(
                    data[[x]],
                    data[[other_column]]
                )
                return(new_column)
            } else {
                return(data[[x]])
            }
        }
        if (x %in% names(units_to_change) == F) {
            return(data[[x]])
        }
    }, simplify = F)





    result <- tibble::as_tibble(result)

}



