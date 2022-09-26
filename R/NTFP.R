

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
    fp_harvest_units_converted <- convert_ntfp_units(unit_data = fp_harvest_units_data,
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
#' @param rhomis_data
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
        rhomis_data,
        fp_harvest_kg,

        fp_props_sold_numeric,
        fp_amount_sold_kg,

        fp_prop_consumed_numeric,
        fp_prop_consumed_kg
) {
    # Beginning with ntfp sold
    number_of_loops <- find_number_of_loops(rhomis_data, name_column = "fp_name")
    harvested_columns <- paste0(fp_harvest_kg, "_", c(1:number_of_loops)) #fruit_amount_kg_1 how do we adapt this as isn't the same column structure as crop
    sold_columns <- paste0(fp_props_sold_numeric, "_", c(1:number_of_loops))

    if (all(harvested_columns %in% colnames(rhomis_data)) == F) {
        stop("Have not calculated the amounts harvested in kg. Calculate amounts harvested before calculating amounts sold")
    }
    if (all(sold_columns %in% colnames(rhomis_data)) == F) {
        stop("Have not calculated the numeric proportions of amount of non-timber forest products sold. Calculate proportions sold before calculating amounts sold")
    }

    harvest_data <- rhomis_data[harvested_columns]
    sold_prop_data <- rhomis_data[sold_columns]

    amount_sold_kg <- tibble::as_tibble(harvest_data * sold_prop_data)
    colnames(amount_sold_kg) <- paste0(fp_amount_sold_kg, "_", c(1:number_of_loops))

    rhomis_data <- add_column_after_specific_column(
        data = rhomis_data,
        new_data = amount_sold_kg,
        new_column_name = fp_amount_sold_kg,
        old_column_name = fp_props_sold_numeric,
        loop_structure = T
    )

    # Moving on to crops consumed
    number_of_loops <- find_number_of_loops(rhomis_data, name_column = "fp_name")
    harvested_columns <- paste0(fp_harvest_kg, "_", c(1:number_of_loops))
    consumed_columns <- paste0(fp_prop_consumed_numeric, "_", c(1:number_of_loops))

    if (all(harvested_columns %in% colnames(rhomis_data)) == F | all(consumed_columns %in% colnames(rhomis_data)) == F) {
        warning("Have not calculated the amounts harvested in kg or amounts consumed Calculate amounts harvested before calculating amounts consumed")
    }
    if (all(harvested_columns %in% colnames(rhomis_data)) == T & all(consumed_columns %in% colnames(rhomis_data)) == T) {
        harvest_data <- rhomis_data[harvested_columns]
        consumed_prop_data <- rhomis_data[consumed_columns]

        amount_consumed_kg <- tibble::as_tibble(harvest_data * consumed_prop_data)
        colnames(amount_consumed_kg) <- paste0(fp_prop_consumed_kg, "_", c(1:number_of_loops))

        rhomis_data <- add_column_after_specific_column(
            data = rhomis_data,
            new_data = amount_consumed_kg,
            new_column_name = fp_prop_consumed_kg,
            old_column_name = fp_prop_consumed_numeric,
            loop_structure = T
        )
    }

    return(rhomis_data)
}


#' NTFP Income Calculation
#'
#' Calculate the Income from NTFPs
#' in LCU per year
#'
#' @param rhomis_data
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
fp_income_calculations <- function(rhomis_data,
                                   unit_conv_tibble = NULL,
                                   fp_sold_kg_per_year_column,
                                   fp_sold_units_column, # a column to be created
                                   fp_sold_income_column,
                                   new_fp_sold_income,
                                   product_type # gemma added, , "fruit_price"
) {

    number_of_loops <- find_number_of_loops(rhomis_data, name_column = "fp_name")

    fp_sold_columns <- paste0(fp_sold_kg_per_year_column, "_", c(1:number_of_loops)) #fruit_amount_sold_kg
    fp_sold_unit_columns <- paste0(fp_sold_units_column, "_", c(1:number_of_loops)) #is this frequency column? (e.g. 'year') #fruit_sold_frequency_1
    fp_sold_income_columns <- paste0(fp_sold_income_column, "_", c(1:number_of_loops)) #fruit_sold_income_1

    if (all(fp_sold_columns %in% colnames(rhomis_data)) == F) {
        stop("Have not calculated the amounts sold in kg. Calculate amounts sold before calculating income")

    }
    if (all(fp_sold_unit_columns %in% colnames(rhomis_data)) == F) {
        stop("Have not converted the non-timber forest product price quantity units yet. Convert these units before calculating incomes sold")
    }



    fp_sold_units_data <- rhomis_data[fp_sold_unit_columns]
    fp_sold_units_numeric <- convert_ntfp_units(unit_data = fp_sold_units_data,
                                                units_conversions=unit_conv_tibble
    )

    fp_sold_amount <- rhomis_data[fp_sold_columns]
    fp_sold_income <- rhomis_data[fp_sold_income_columns]




    # Multiplying values which do not have "total_income_per_year_unit
    fp_sold_income_per_year <- fp_sold_income %>% dplyr::mutate_all(as.numeric)

    fp_sold_income_per_year <- fp_sold_income_per_year * fp_sold_units_numeric

    fp_sold_income_per_year[fp_sold_amount==0] <- 0


    colnames(fp_sold_income_per_year) <- paste0(new_fp_sold_income, "_", c(1:number_of_loops))
    rhomis_data <- add_column_after_specific_column(
        data = rhomis_data,
        new_data = fp_sold_income_per_year,
        new_column_name = new_fp_sold_income,
        old_column_name = fp_sold_income_column,
        loop_structure = T
    )

    fp_price <- fp_sold_income_per_year / fp_sold_amount
    colnames(fp_price) <- paste0(product_type, "_", c(1:number_of_loops))

    rhomis_data <- add_column_after_specific_column(
         data= rhomis_data,
        new_data = fp_price,
        new_column_name = product_type,
        old_column_name = new_fp_sold_income,
        loop_structure = T
    )

    return(rhomis_data)
}


fp_calculations_all <- function(
        rhomis_data,
        units_and_conversions
){


    # Calculating Harvest from forest products
    if ("fp_amount_to_kg" %in% names(units_and_conversions))
    {
        rhomis_data <- calculate_fp_harvest(
            tree_aid_df=rhomis_data,
            fp_harvest_conversions=units_and_conversions$fp_amount_to_kg,
            name_column="fp_name",
            amount_column="fruit_amount",
            unit_column="fruit_amount_units"
        )

        # nut
        rhomis_data  <- calculate_fp_harvest(
            tree_aid_df=rhomis_data,
            fp_harvest_conversions=units_and_conversions$fp_amount_to_kg,
            name_column="fp_name",
            amount_column="nut_amount",
            unit_column="nut_amount_units"
        )

        # leaves
        rhomis_data <- calculate_fp_harvest(
            tree_aid_df=rhomis_data,
            fp_harvest_conversions=units_and_conversions$fp_amount_to_kg,
            name_column="fp_name",
            amount_column="leaves_amount",
            unit_column="leaves_amount_units"
        )

        # bark
        rhomis_data <- calculate_fp_harvest(
            tree_aid_df=rhomis_data,
            fp_harvest_conversions=units_and_conversions$fp_amount_to_kg,
            name_column="fp_name",
            amount_column="bark_amount",
            unit_column="bark_amount_units"
        )

        # roots
        rhomis_data <- calculate_fp_harvest(
            tree_aid_df=rhomis_data,
            fp_harvest_conversions=units_and_conversions$fp_amount_to_kg,
            name_column="fp_name",
            amount_column="roots_amount",
            unit_column="roots_amount_units"
        )

        # gum
        rhomis_data <- calculate_fp_harvest(
            tree_aid_df=rhomis_data,
            fp_harvest_conversions=units_and_conversions$fp_amount_to_kg,
            name_column="fp_name",
            amount_column="gum_amount",
            unit_column="gum_amount_units"
        )
    }

    # Calculating NTFP proportion sold
    rhomis_data <- fp_proportions_all(
        rhomis_data,
        use="sell",
        use_column="fruit_use",
        prop_column="fruit_sold_prop",
        new_column_name="fruit_sold_prop_numeric"
    )

    # nut
    rhomis_data <- fp_proportions_all(
        rhomis_data,
        use="sell",
        use_column="nut_use",
        prop_column="nut_sold_prop",
        new_column_name="nut_sold_prop_numeric"
    )

    # leaves
    rhomis_data <- fp_proportions_all(
        rhomis_data,
        use="sell",
        use_column="leaves_use",
        prop_column="leaves_sold_prop",
        new_column_name="leaves_sold_prop_numeric"
    )

    # bark
    rhomis_data <- fp_proportions_all(
        rhomis_data,
        use="sell",
        use_column="bark_use",
        prop_column="bark_sold_prop",
        new_column_name="bark_sold_prop_numeric"
    )

    # roots
    rhomis_data <- fp_proportions_all(
        rhomis_data,
        use="sell",
        use_column="roots_use",
        prop_column="roots_sold_prop",
        new_column_name="roots_sold_prop_numeric"
    )

    # gum
    rhomis_data <- fp_proportions_all(
        rhomis_data,
        use="sell",
        use_column="gum_use",
        prop_column="gum_sold_prop",
        new_column_name="gum_sold_prop_numeric"
    )


    rhomis_data <- fp_proportions_all(
        rhomis_data,
        use="eat",
        use_column="fruit_use",
        prop_column="fruit_eaten_prop",
        new_column_name="fruit_eaten_prop_numeric"
    )

    # nut
    rhomis_data <- fp_proportions_all(
        rhomis_data,
        use="eat",
        use_column="nut_use",
        prop_column="nut_eaten_prop",
        new_column_name="nut_eaten_prop_numeric"
    )

    # leaves
    rhomis_data <- fp_proportions_all(
        rhomis_data,
        use="eat",
        use_column="leaves_use",
        prop_column="leaves_consumed_prop",
        new_column_name="leaves_consumed_prop_numeric"
    )

    # bark
    rhomis_data <- fp_proportions_all(
        rhomis_data,
        use="eat",
        use_column="bark_use",
        prop_column="bark_eaten_prop",
        new_column_name="bark_eaten_prop_numeric"
    )

    # roots
    rhomis_data <- fp_proportions_all(
        rhomis_data,
        use="eat",
        use_column="roots_use",
        prop_column="roots_eaten_prop",
        new_column_name="roots_eaten_prop_numeric"
    )

    # gum
    rhomis_data <- fp_proportions_all(
        rhomis_data,
        use="eat",
        use_column="gum_use",
        prop_column="gum_eaten_prop",
        new_column_name="gum_eaten_prop_numeric"
    )

    # shea
    # honey



    # (4c) PROPORTIONS PROCESSED SOLD

    # Sold processed proportion columns
    # fruit
    rhomis_data <- fp_proportions_all(
        rhomis_data,
        use="process",
        use_column="fruit_use",
        prop_column="fruit_process_sold_prop",
        new_column_name="fruit_process_sold_prop_numeric"
    )

    # nut
    rhomis_data <- fp_proportions_all(
        rhomis_data,
        use="process",
        use_column="nut_use",
        prop_column="nut_process_sold_prop",
        new_column_name="nut_process_sold_prop_numeric"
    )

    # leaves
    rhomis_data <- fp_proportions_all(
        rhomis_data,
        use="process",
        use_column="leaves_use",
        prop_column="leaves_process_sold_prop",
        new_column_name="leaves_process_sold_prop_numeric"
    )

    # bark
    rhomis_data <- fp_proportions_all(
        rhomis_data,
        use="process",
        use_column="bark_use",
        prop_column="bark_process_sold_prop",
        new_column_name="bark_process_sold_prop_numeric"
    )

    # roots
    rhomis_data <- fp_proportions_all(
        rhomis_data,
        use="process",
        use_column="roots_use",
        prop_column="roots_process_sold_prop",
        new_column_name="roots_process_sold_prop_numeric"
    )

    # gum
    rhomis_data <- fp_proportions_all(
        rhomis_data,
        use="process",
        use_column="gum_use",
        prop_column="gum_process_sold_prop",
        new_column_name="gum_process_sold_prop_numeric"
    )

    # shea
    # honey



    # (4d) PROPORTIONS PROCESSED EATEN / CONSUMED

    # Consumed/eaten processed proportion columns
    # fruit
    rhomis_data <- fp_proportions_all(
        rhomis_data,
        use="process",
        use_column="fruit_use",
        prop_column="fruit_process_eaten_prop",
        new_column_name="fruit_process_eaten_prop_numeric"
    )

    # nut
    rhomis_data <- fp_proportions_all(
        rhomis_data,
        use="process",
        use_column="nut_use",
        prop_column="nut_process_eaten_prop",
        new_column_name="nut_process_eaten_prop_numeric"
    )

    # leaves
    rhomis_data <- fp_proportions_all(
        rhomis_data,
        use="process",
        use_column="leaves_use",
        prop_column="leaves_process_consumed_prop",
        new_column_name="leaves_process_consumed_prop_numeric"
    )

    # bark
    rhomis_data <- fp_proportions_all(
        rhomis_data,
        use="process",
        use_column="bark_use",
        prop_column="bark_process_eaten_prop",
        new_column_name="bark_process_eaten_prop_numeric"
    )

    # roots
    rhomis_data <- fp_proportions_all(
        rhomis_data,
        use="process",
        use_column="roots_use",
        prop_column="roots_process_eaten_prop",
        new_column_name="roots_process_eaten_prop_numeric"
    )

    # gum
    rhomis_data <- fp_proportions_all(
        rhomis_data,
        use="process",
        use_column="gum_use",
        prop_column="gum_process_eaten_prop",
        new_column_name="gum_process_eaten_prop_numeric"
    )


    # (5) END CALCULATION AMOUNT EATEN AND SOLD

    # Create function

    # Run end calculations for eaten and sold for each product
    # fruit
    rhomis_data <- ntfp_sold_and_consumed_calculation(
        rhomis_data=rhomis_data,
        fp_harvest_kg="fruit_amount_kg",
        fp_props_sold_numeric="fruit_sold_prop_numeric",
        fp_amount_sold_kg="fruit_amount_sold_kg",

        fp_prop_consumed_numeric="fruit_eaten_prop_numeric",
        fp_prop_consumed_kg="fruit_amount_eaten_kg"
    )

    # nut
    rhomis_data <- ntfp_sold_and_consumed_calculation(
        rhomis_data=rhomis_data,
        fp_harvest_kg="nut_amount_kg",
        fp_props_sold_numeric="nut_sold_prop_numeric",
        fp_amount_sold_kg="nut_amount_sold_kg",

        fp_prop_consumed_numeric="nut_eaten_prop_numeric",
        fp_prop_consumed_kg="nut_amount_eaten_kg"
    )

    # leaves
    rhomis_data <- ntfp_sold_and_consumed_calculation(
        rhomis_data=rhomis_data,
        fp_harvest_kg="leaves_amount_kg",
        fp_props_sold_numeric="leaves_sold_prop_numeric",
        fp_amount_sold_kg="leaves_amount_sold_kg",

        fp_prop_consumed_numeric="leaves_consumed_prop_numeric",
        fp_prop_consumed_kg="leaves_amount_eaten_kg"
    ) # LEAVES NOT WORKING, NEED TO TROUBLESHOOT

    # bark
    rhomis_data <- ntfp_sold_and_consumed_calculation(
        rhomis_data=rhomis_data,
        fp_harvest_kg="bark_amount_kg",
        fp_props_sold_numeric="bark_sold_prop_numeric",
        fp_amount_sold_kg="bark_amount_sold_kg",

        fp_prop_consumed_numeric="bark_eaten_prop_numeric",
        fp_prop_consumed_kg="bark_amount_eaten_kg"
    )

    # roots
    rhomis_data <- ntfp_sold_and_consumed_calculation(
        rhomis_data=rhomis_data,
        fp_harvest_kg="roots_amount_kg",
        fp_props_sold_numeric="roots_sold_prop_numeric",
        fp_amount_sold_kg="roots_amount_sold_kg",

        fp_prop_consumed_numeric="roots_eaten_prop_numeric",
        fp_prop_consumed_kg="roots_amount_eaten_kg"
    )

    # gum
    rhomis_data <- ntfp_sold_and_consumed_calculation(
        rhomis_data=rhomis_data,
        fp_harvest_kg="gum_amount_kg",
        fp_props_sold_numeric="gum_sold_prop_numeric",
        fp_amount_sold_kg="gum_amount_sold_kg",

        fp_prop_consumed_numeric="gum_eaten_prop_numeric",
        fp_prop_consumed_kg="gum_amount_eaten_kg"
    )



    # (6) INCOME

    # Create NTFP income calculation function






    # Conducting the calculation
    if ("fp_income_per_freq_to_lcu_per_year" %in% names(units_and_conversions))
    {
        rhomis_data <- fp_income_calculations(
            rhomis_data = rhomis_data,
            fp_sold_kg_per_year_column = "fruit_amount_sold_kg",
            fp_sold_units_column = "fruit_sold_frequency",
            fp_sold_income_column = "fruit_sold_income",
            new_fp_sold_income = "fruit_sold_income_per_year",
            unit_conv_tibble = units_and_conversions$fp_income_per_freq_to_lcu_per_year,
            product_type = "fruit_price"
        )

        rhomis_data <- fp_income_calculations(
            rhomis_data = rhomis_data,
            fp_sold_kg_per_year_column = "nut_amount_sold_kg",
            fp_sold_units_column = "nut_sold_frequency",
            fp_sold_income_column = "nut_sold_income",
            new_fp_sold_income = "nut_sold_income_per_year",
            unit_conv_tibble = units_and_conversions$fp_income_per_freq_to_lcu_per_year,
            product_type = "nut_price"
        )

        rhomis_data <- fp_income_calculations(
            rhomis_data = rhomis_data,
            fp_sold_kg_per_year_column = "leaves_amount_sold_kg",
            fp_sold_units_column = "leaves_sold_price_quantityunits",
            fp_sold_income_column = "leaves_sold_income",
            new_fp_sold_income = "leaves_sold_income_per_year",
            unit_conv_tibble = units_and_conversions$fp_income_per_freq_to_lcu_per_year,
            product_type = "leaves_price"
        )

        rhomis_data <- fp_income_calculations(
            rhomis_data = rhomis_data,
            fp_sold_kg_per_year_column = "bark_amount_sold_kg",
            fp_sold_units_column = "bark_sold_price_quantityunits",
            fp_sold_income_column = "bark_sold_income",
            new_fp_sold_income = "bark_sold_income_per_year",
            unit_conv_tibble = units_and_conversions$fp_income_per_freq_to_lcu_per_year,
            product_type = "bark_price"
        )

        rhomis_data <- fp_income_calculations(
            rhomis_data = rhomis_data,
            fp_sold_kg_per_year_column = "roots_amount_sold_kg",
            fp_sold_units_column = "roots_sold_price_quantityunits",
            fp_sold_income_column = "roots_sold_income",
            new_fp_sold_income = "roots_sold_income_per_year",
            unit_conv_tibble = units_and_conversions$fp_income_per_freq_to_lcu_per_year,
            product_type = "roots_price"
        )

        rhomis_data <- fp_income_calculations(
            rhomis_data = rhomis_data,
            fp_sold_kg_per_year_column = "gum_amount_sold_kg",
            fp_sold_units_column = "gum_sold_price_quantityunits",
            fp_sold_income_column = "gum_sold_income_per_freq",
            new_fp_sold_income = "gum_sold_income_per_year",
            unit_conv_tibble = units_and_conversions$fp_income_per_freq_to_lcu_per_year,
            product_type = "gum_price"
        )
    }




}



replace_fp_other_units <- function(tree_aid_df, fp_list){

    looped_units <- list(
        "fruit_amount_units" = "fruit_amount_units_other",
        "fruit_amount_units"=""
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



