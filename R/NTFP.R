

#' Convert NTFP Units
#'
#' Switch Out NTFP units
#'
#' @param unit_data
#' @param units_conversions
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
#' @param tree_aid_df
#' @param fp_harvest_conversions
#' @param name_column
#' @param amount_column
#' @param unit_column
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
        return(NULL)
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
    fp_harvest_units_converted <- convert_units(unit_data = fp_harvest_units_data,
                                                units_conversions=fp_harvest_conversions
    )

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
#' @param tree_aid_df
#' @param use
#' @param use_column
#' @param prop_column
#' @param new_column_name
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
#'
#' @param data
#' @param fp_harvest_kg
#' @param fp_props_sold_numeric
#' @param fp_amount_sold_kg
#' @param fp_prop_consumed_numeric
#' @param fp_prop_consumed_kg
#'
#' @return
#' @export
#'
#' @examples
ntfp_sold_and_consumed_calculation <- function(
        data,
        fp_harvest_kg,

        fp_props_sold_numeric,
        fp_amount_sold_kg,

        fp_prop_consumed_numeric,
        fp_prop_consumed_kg
) {
    # Beginning with ntfp sold
    number_of_loops <- find_number_of_loops(tree_aid_df, name_column = "fp_name")
    harvested_columns <- paste0(fp_harvest_kg, "_", c(1:number_of_loops)) #fruit_amount_kg_1 how do we adapt this as isn't the same column structure as crop
    sold_columns <- paste0(fp_props_sold_numeric, "_", c(1:number_of_loops))

    if (all(harvested_columns %in% colnames(data)) == F) {
        stop("Have not calculated the amounts harvested in kg. Calculate amounts harvested before calculating amounts sold")
    }
    if (all(sold_columns %in% colnames(data)) == F) {
        stop("Have not calculated the numeric proportions of amount of non-timber forest products sold. Calculate proportions sold before calculating amounts sold")
    }

    harvest_data <- data[harvested_columns]
    sold_prop_data <- data[sold_columns]

    amount_sold_kg <- tibble::as_tibble(harvest_data * sold_prop_data)
    colnames(amount_sold_kg) <- paste0(fp_amount_sold_kg, "_", c(1:number_of_loops))

    data <- add_column_after_specific_column(
        data = data,
        new_data = amount_sold_kg,
        new_column_name = fp_amount_sold_kg,
        old_column_name = fp_props_sold_numeric,
        loop_structure = T
    )

    # Moving on to crops consumed
    number_of_loops <- find_number_of_loops(data, name_column = "fp_name")
    harvested_columns <- paste0(fp_harvest_kg, "_", c(1:number_of_loops))
    consumed_columns <- paste0(fp_prop_consumed_numeric, "_", c(1:number_of_loops))

    if (all(harvested_columns %in% colnames(data)) == F | all(consumed_columns %in% colnames(data)) == F) {
        warning("Have not calculated the amounts harvested in kg or amounts consumed Calculate amounts harvested before calculating amounts consumed")
    }
    if (all(harvested_columns %in% colnames(data)) == T & all(consumed_columns %in% colnames(data)) == T) {
        harvest_data <- data[harvested_columns]
        consumed_prop_data <- data[consumed_columns]

        amount_consumed_kg <- tibble::as_tibble(harvest_data * consumed_prop_data)
        colnames(amount_consumed_kg) <- paste0(fp_prop_consumed_kg, "_", c(1:number_of_loops))

        data <- add_column_after_specific_column(
            data = data,
            new_data = amount_consumed_kg,
            new_column_name = fp_prop_consumed_kg,
            old_column_name = fp_prop_consumed_numeric,
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
#' @param data
#' @param unit_conv_tibble
#' @param fp_sold_kg_per_year_column
#' @param fp_sold_units_column
#' @param fp_sold_income_column
#' @param new_fp_sold_income
#' @param product_type
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
                                   product_type # gemma added, , "fruit_price"
) {

    number_of_loops <- find_number_of_loops(data, name_column = "fp_name")

    fp_sold_columns <- paste0(fp_sold_kg_per_year_column, "_", c(1:number_of_loops)) #fruit_amount_sold_kg
    fp_sold_unit_columns <- paste0(fp_sold_units_column, "_", c(1:number_of_loops)) #is this frequency column? (e.g. 'year') #fruit_sold_frequency_1
    fp_sold_income_columns <- paste0(fp_sold_income_column, "_", c(1:number_of_loops)) #fruit_sold_income_1

    if (all(fp_sold_columns %in% colnames(data)) == F) {
        stop("Have not calculated the amounts sold in kg. Calculate amounts sold before calculating income")
    }
    if (all(fp_sold_unit_columns %in% colnames(data)) == F) {
        stop("Have not converted the non-timber forest product price quantity units yet. Convert these units before calculating incomes sold")
    }



    fp_sold_units_data <- data[fp_sold_unit_columns]
    fp_sold_units_numeric <- convert_units(unit_data = fp_sold_units_data,
                                           units_conversions=unit_conv_tibble
    )

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
    colnames(fp_price) <- paste0(product_type, "_", c(1:number_of_loops))

    data <- add_column_after_specific_column(
        data = data,
        new_data = fp_price,
        new_column_name = product_type,
        old_column_name = new_fp_sold_income,
        loop_structure = T
    )

    return(data)
}




