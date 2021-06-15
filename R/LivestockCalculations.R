


#' Calculate Livestock Price
#'
#' Calculate the prices of whole livestock sold
#'
#' @param data Dataset containing livestock sold and livestock sale income
#'
#' @return
#' @export
#'
#' @examples
price_per_livestock <- function(data){

    number_of_loops <- find_number_of_loops(data, name_column="livestock_sold")
    sold_columns <- paste0("livestock_sold","_", c(1:number_of_loops))
    income_columns <- paste0("livestock_sale_income","_", c(1:number_of_loops))
    price_columns <- paste0("livestock_price_per_animal","_",c(1:number_of_loops))

    sold_data <- data[sold_columns]
    income_data <- data[income_columns]

    livestock_sale_prices <- income_data/sold_data
    colnames(livestock_sale_prices) <-price_columns

    data <- add_column_after_specific_column(data,
                                             new_data = livestock_sale_prices,
                                             new_column_name = "livestock_price_per_animal",
                                             old_column_name = "livestock_sale_income",
                                             loop_structure = T

    )

    return(data)
}


#' Meat amount calculations
#'
#' A function to calculate the amount of meat
#' produced based on the number of animals killed,
#' and some conversion factors which can calculate
#' how much meat can be collected from an animal which
#' has been killed.
#'
#' @param animal_names_column The column which contains the names of the animals
#' @param animal_weights_column The column which contains the
#' weights for converting the animals
#' @param data RHoMIS data including information on livestock
#' names, and the number of livestock killed for meat
#' @param animal_weights_conversion_table The containing conversion factors
#' for animal names into animal weights
#'
#' @return
#' @export
#'
#' @examples
meat_amount_calculation <- function(data,
                                    animal_weights_conversion_table=livestock_weights,
                                    animal_names_column="animal",
                                    animal_weights_column="weight_kg"){



    number_of_loops <- find_number_of_loops(data,"livestock_name")
    livestock_name_columns <- paste0("livestock_name","_",c(1:number_of_loops))
    animals_killed_columns <- paste0("killed_for_meat","_",c(1:number_of_loops))

    livestock_name_data <- data[livestock_name_columns]
    killed_for_meat_data <- data[animals_killed_columns]

    livestock_weight_data <-switch_units(livestock_name_data,
                                         units = animal_weights_conversion_table[[animal_names_column]],
                                         conversion_factors = animal_weights_conversion_table[[animal_weights_column]])


    meat_weight_kg <- livestock_weight_data*killed_for_meat_data
    colnames(meat_weight_kg) <- paste0("meat_kg_per_year", "_", c(1:number_of_loops))


    data <- add_column_after_specific_column(data =data,
                                             new_data = meat_weight_kg,
                                             new_column_name = "meat_kg_per_year",
                                             old_column_name = "killed_for_meat",
                                             loop_structure = T)


    return(data)
}

#' Meat Use Calculations
#'
#' Calculating the numeric proportions of meat used for
#' eating and selling
#'
#' @param data The RHoMIS data containing livestock loops
#'
#' @return
#' @export
#'
#' @examples
meat_uses <- function(data){



    number_of_loops <- find_number_of_loops(data, "meat_kg_per_year")

    meat_sold_props_numeric <- sapply(c(1:number_of_loops), function(x) proportions_calculation(data, use = "sell", use_column = "meat_use", prop_column = "meat_sell_amount", loop_number = x))
    colnames(meat_sold_props_numeric) <- paste0("meat_sold_props_numeric","_",c(1:number_of_loops))
        meat_sold_props_numeric<- tibble::as_tibble(meat_sold_props_numeric)

    data <- add_column_after_specific_column(data = data,
                                             new_data = meat_sold_props_numeric,
                                             new_column_name = "meat_sold_props_numeric",
                                             old_column_name = "meat_sell_amount",
                                             loop_structure =T
                                             )


    meat_consumed_props_numeric <- sapply(c(1:number_of_loops), function(x) proportions_calculation(data, use = "eat", use_column = "meat_use", prop_column = "meat_consumed_amount", loop_number = x))
    colnames(meat_consumed_props_numeric) <- paste0("meat_consumed_props_numeric","_",c(1:number_of_loops))
    meat_consumed_props_numeric<- tibble::as_tibble(meat_consumed_props_numeric)
    data <- add_column_after_specific_column(data = data,
                                             new_data = meat_consumed_props_numeric,
                                             new_column_name = "meat_consumed_props_numeric",
                                             old_column_name = "meat_consumed_amount",
                                             loop_structure =T)


    return(data)
}



#' Gender Split of Livestock Information
#'
#' Whole livestock, and products produced from livestock
#' are divided among male and female farmers. This function
#' determines how these values are split
#'
#' @param data RHoMIS data including information on number
#' of livestock sold and who sells this livestock
#'
#' @return
#' @export
#'
#' @examples
gender_split_livestock <- function(data){

    # data <- insert_gender_columns_in_core_data(data,
    #                                            original_column = "livestock_sale_income",
    #                                            control_column = "livestock_who_sells",
    #                                            loop_structrue=T)




}

