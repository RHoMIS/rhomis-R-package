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


