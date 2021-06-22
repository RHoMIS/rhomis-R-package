library(readr)
library(tibble)
library(dplyr)
library(purrr)
library(magrittr)
#' Load RHoMIS
#'
#' Reads the RHoMIS data from a local file.
#'
#' @param path The path to the RHoMIS file. If
#'
#' @return Returns a tibble of the RHoMIS dataset.
#' @export
#'
#' @examples
#' path <- "C:/User/Desktop/RHoMIS_Project/Data/raw_data.csv"
#' # data <- load_rhomis(path)
#' # Data will be loaded in tibble format
load_rhomis <- function(path){
    data <- readr::read_csv(path, na=c("","na","-999","NA","n/a"))
    return(data)
}

#' Switch Units
#'
#' In RHoMIS, sometimes lists or data frames contain units which need to be converted
#' from a character format (e.g. "kg","sacks_100kg") into a numeric conversion (e.g. 1, 100).
#' This function does just that.
#'
#' @param data_to_convert This list or data frame which needs to be converted
#' @param units A list of the character units which need to be converted (see example)
#' @param conversion_factors A list of converstion factors for the unit list (see example)
#'
#' @return Returns the data (in its original format) with the units in their numerical format
#' @export
#'
#' @examples
#'# Converting a list
#' units <- c("kg", "sacks_50kg", "wheel_barrows_100kg","litres")
#' conversion_factors <- c(1,50,100,1)
#' list_to_convert <- c("kg", "sacks_50kg","other_unit",NA, NA, "wheel_barrows_100kg","litres")
#' switch_units(list_to_convert, units, conversion_factors)
#' # Converting a data frame or tibble
#' units <- c("kg", "sacks_50kg", "wheel_barrows_100kg","litres")
#' conversion_factors <- c(1,50,100,1)
#' tibble_to_convert <- tibble::as_tibble(list("maize"=c("kg", "other_random_unit","wheel_barrows_100kg"),
#'                                     "cassava"=c("sacks_50kg",NA,"another_random_unit"),
#'                                     "banana"=c("bunches", "wheel_barrows_100kg",NA)))
#' switch_units(tibble_to_convert, units, conversion_factors)
#'
switch_units <- function(data_to_convert, units, conversion_factors){

    if (length(units)!=length(unique(units))){
        stop("You were trying to convert units, but the list of units you provided has duplicates")
    }

    if (length(units)!=length(conversion_factors)){
        stop("You were trying to convert units, but the list of units you provided is not the same length as the list of conversion factors. There must be a mismatch")
    }

    # Converting the two lists into a tibble which can be searched
    unit_conv_tibble <- tibble::as_tibble(list(unit=units, conversion_factors=conversion_factors))


    if ("tbl" %in% class(data_to_convert) | "tbl_df" %in% class(data_to_convert) | "data.frame" %in% class(data_to_convert))
    {
        converted_data <- data_to_convert %>% purrr::map_df(function(x) replace_unit_list(x,unit_conv_tibble))
    }
    if((is.list(data_to_convert) | is.vector(data_to_convert)) & ("tbl" %in% class(data_to_convert)==F & "tbl_df" %in% class(data_to_convert)==F & "data.frame" %in% class(data_to_convert)==F))
    {
        converted_data <- replace_unit_list(data_to_convert, unit_conv_tibble)
    }

    return(converted_data)
}


#' Replace unit list
#'
#' Replacing all of the character units in an R list
#'
#' @param list_to_convert The list of character units you need to convert
#' @param unit_conv_tibble The tibble of unit conversions
#'
#' @return A list of numerically converted units
#' @export
#'
#' @examples
#' list_to_convert <- c("kg", "sacks_50kg","other_unit",NA, NA, "wheel_barrows_100kg","litres")
#' unit_conv_tibble <- tibble::as_tibble(list(unit=c("kg", "sacks_50kg", "wheel_barrows_100kg","litres"),
#'                                    conversion_factors= c(1,50,100,1)))
#' replace_unit_list(list_to_convert,unit_conv_tibble)
replace_unit_list <- function(list_to_convert, unit_conv_tibble){
    converted_list <- list_to_convert %>% purrr::map(function(x) replace_unit_with_conversion_factor(x, unit_conv_tibble)) %>%
        unlist()
    return(converted_list)
}

#' Replace unit with conversion factor
#'
#' A function to replace individual units with their appropriate conversion factors
#'
#' @param item_to_convert The character unit which needs to be converted
#' @param unit_conv_tibble A table of units, and their numerical conversion factor. With the column names "unit" and "conversion_factors"
#'
#' @return A numeric conversion of the original character unit
#' @export
#'
#' @examples
#'
#' item_to_convert <- "wheel_barrows_100kg"
#' unit_conv_tibble <- tibble::as_tibble(list(unit=c("kg", "sacks_50kg", "wheel_barrows_100kg","litres"), conversion_factors= c(1,50,100,1)))
#' replace_unit_with_conversion_factor(item_to_convert, unit_conv_tibble)

replace_unit_with_conversion_factor <- function(item_to_convert, unit_conv_tibble){

    if (item_to_convert %in%unit_conv_tibble$unit)
    {
        converted_item <- unit_conv_tibble$conversion_factors[unit_conv_tibble$unit==item_to_convert]
    }else{
        converted_item <- NA
    }

    return(converted_item)

}

#' Convert All Columns to Lower Case
#'
#' Convert all character columns in the data set
#' to lower case
#'
#' @param data A tibble to be converted to lower case
#'
#' @return
#' @export
#'
#' @examples
convert_all_columns_to_lower_case <- function(data){

    data <- data %>% dplyr::mutate_all(convert_column_to_lowercase)
    return(data)



}

#' Convert Column to Lowercase
#'
#' Convert an individual column to lower case
#' if it is a character column
#'
#' @param column Column to be converted.
#'
#' @return
#' @export
#'
#' @examples
convert_column_to_lowercase <- function(column){
    if(is.character(column)){
        return(tolower(column))
    }else{
        return(column)
    }

}

