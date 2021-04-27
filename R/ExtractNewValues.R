library(tidyverse)



#' Finding Unique Values
#'
#' A function to find the unique values for all of a given tibble.
#'
#' @param data A tibble for which we hope to find all of the unique values
#'
#' @return A list of the unique values, NAs excluded
#' @export
#'
#' @examples
#' data <- tibble(crop_name_1=c("maize","cassava","millet"),
#' crop_name_2=c("potato",NA,"cucumber"))
#' # Expect the result c("maize", "cassava", "millet","potato", "cucumber")
find_unique_values <- function(data){


    # Finding all of the unique values
    all_values <- as.character(unlist(lapply(data, function(x) unique(x))))
    # Removing NA column
    all_values <- all_values[!is.na(all_values)]
    # Finding unique values from all the columns
    unique_values <- unique(all_values)
    return(unique_values)
}


#' Extract New Units
#'
#' A function for extracting any new values from a survey
#'
#' @param data The whole data-set from which the units are to be extracted from
#' @param loop_or_individual_column Options are: "loop", "column". Specifying whether you want to extract new values from a loop or a column.
#' @param column_pattern The pattern which is used to identify the columns. For example if trying to extract new crop names, these come in the form "crop_name_1", "crop_name_2", "crop_name_3". The pattern here would be "crop_name"
#' @param number_of_loops If loop_or_individual_column=="loop", then here we specify how many loops to search for.
#' @param column_name If loop_or_individual_column=="column", then here we specify which column to search for
#'
#' @return A list of the individual values for that value (excluding NA values)
#' @export
#'
#' @examples
#'
#' # Example code for a crop loop
#' column_pattern <- "crop_name"
#' number_of_loops <- 2
#' data <- tibble(crop_name_1=c("maize","cassava","millet"),
#'                random_columns=c("maize","cassava","millet"),
#'                crop_name_2=c("potato",NA,"cucumber"))
#' result <- extract_new_values(data, loop_or_individual_column="loop",column_pattern="crop_name", number_of_loops=2)
#' # expected_result is: c("maize","cassava","millet","potato","cucumber")
#'
#' # Example code for an individual column

extract_new_values <- function(data, loop_or_individual_column="loop",column_name=NA,column_pattern=NA, number_of_loops=NA){
    # Extracting new values for a RHoMIS tibble
    if (loop_or_individual_column=="loop")
    {
        if (is.na(column_pattern)){
            stop("You have selected to extract new values from loops. Please specify a 'column_pattern' for those loop.")
        }
        if (is.na(number_of_loops)){
            stop("You have selected to extract new values from loops. Please specify a 'number_of_loops' for those loops.")
        }
        relevant_data <- data[,paste0(column_pattern,"_",1:number_of_loops)]
        unique_values <- find_unique_values(relevant_data)
        return(unique_values)
    }

    if (loop_or_individual_column=="column")
    {
        if (is.na(column_name)){
            stop("You have selected to extract new values from a specific column. Please specify a 'column_name'.")
        }


        relevant_data <- data[,column_name]
        unique_values <- find_unique_values(relevant_data)
        return(unique_values)
    }
}

find_loop_number_and_extract_values <- function(data, column_pattern){

    # Creating a pattern to search through the column names of the data
    regex_pattern <- paste0("^",column_pattern,"_[[:digit:]]") # Finding columns which start with "column_pattern_Integer"

    # Finding the relevant column names
    relevant_columns <- grep(regex_pattern, colnames(data), value=T)
    # The number of loops for this column
    number_of_loops <- length(relevant_columns)

    new_values <- extract_new_values(data, loop_or_individual_column="loop",column_pattern=column_pattern, number_of_loops=number_of_loops)

    return(new_values)


}





#' Extract New Core Units
#'
#' A function to extract the new values from a core RHoMIS survey. Note that this function will not work on any dataset which has modified the core questions
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
extract_new_core_units <- function(data)
{
    loop_values_to_extract <- c("crop_name",
                                "livestock_name",
                                "crop_yield_units",
                                "crop_yield_units_other",
                                "crop_sold_price_quantityunits",
                                "crop_price_quantityunits_other",
                                "unitland",
                                "areaunits_other",
                                "unitland_owned",
                                "unitland_rentin",
                                "unitland_rentout",
                                "milk_units",
                                "milk_amount_units_other",
                                "milk_sold_price_timeunits",
                                "milk_amount_time_units_other",
                                "bees_honey_production_units",
                                "bees_honey_production_units_other",
                                "eggs_units",
                                "eggs_amount_units_other",
                                "eggs_sold_price_timeunits",
                                "eggs_sold_price_timeunits_other")


    # Return a named list when applying the function
    loop_results <- sapply(loop_values_to_extract, function(x) find_loop_number_and_extract_values(data,x), simplify = F)

    individual_columns_to_extract <- c("fertiliser_units","fertiliser_units_other")
    column_results <- sapply(individual_columns_to_extract, function(x) extract_new_values(data,loop_or_individual_column="column", column_name=x), simplify = F)



    categories_to_merge <- list(crop_name=c("crop_name"),
                                livestock_name=c("livestock_name"),
                                crop_yield_units=c("crop_yield_units_other"),
                                crop_sold_price_quantityunits=c("crop_price_quantityunits_other"),
                                unitland= c("areaunits_other","unitland_owned","unitland_rentin","unitland_rentout"),
                                milk_units=c("milk_amount_units_other"),
                                milk_sold_price_timeunits=c("milk_amount_time_units_other"),
                                bees_honey_production_units=c("bees_honey_production_units_other"),
                                eggs_units=c("eggs_amount_units_other"),
                                eggs_sold_price_timeunits=c("eggs_sold_price_timeunits_other"),
                                fertiliser_units=c("fertiliser_units_other"))



    final_result <- c(loop_results,column_results)

    final_result<-sapply(names(categories_to_merge),function(x) merge_and_simplify_core_values(final_result,main_item=x,categories_to_merge), simplify = F)


    return(final_result)


}


merge_and_simplify_core_values <- function(list_of_unique_core_values, main_item,categories_to_merge){


    extras_categories_merge<-categories_to_merge[[main_item]]
    extra_values_to_merge <- list_of_unique_core_values[extras_categories_merge]
    extra_values_to_merge <- unlist(extra_values_to_merge)
    extra_values_to_merge <- unique(extra_values_to_merge)

    list_of_unique_core_values[[main_item]] <- c(list_of_unique_core_values[[main_item]],extra_values_to_merge)
    list_of_unique_core_values[[main_item]] <- unique(list_of_unique_core_values[[main_item]])

    return(list_of_unique_core_values[[main_item]])





}



