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
        return(converted_data)

    }
    if((is.list(data_to_convert) | is.vector(data_to_convert)) & ("tbl" %in% class(data_to_convert)==F & "tbl_df" %in% class(data_to_convert)==F & "data.frame" %in% class(data_to_convert)==F))
    {
        converted_data <- replace_unit_list(data_to_convert, unit_conv_tibble)
        return(converted_data)

    }

    warning("Could not identify type of list")
    return(data_to_convert)

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


    if (item_to_convert %in%unit_conv_tibble$unit & !is.na(item_to_convert))
    {
        converted_item <- unit_conv_tibble$conversion_factors[unit_conv_tibble$unit==item_to_convert & !is.na(unit_conv_tibble$unit)]
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

#' Replace crop and livestock other
#'
#' Replace "other1", "other2", and "other3" text
#' entries in the crop and livestock names
#'
#' @param data A whole rhomis dataset
#'
#' @return
#' @export
#'
#' @examples
replace_crop_and_livestock_other <- function(data){

    number_of_loops <- find_number_of_loops(data,"crop_name")
    if(number_of_loops==0){
        warning('Could not find any "crop_name_X" columns.')
        return(data)
    }


    crop_name_columns <- paste0("crop_name_",1:number_of_loops)
    other_1 <- NULL
    other_2 <- NULL
    other_3 <- NULL
    if ("crops_other1" %in% colnames(data)){
        other_1 <- data[["crops_other1"]]
    }
    if ("crops_other2" %in% colnames(data)){
        other_2 <- data[["crops_other2"]]
    }
    if ("crops_other3" %in% colnames(data)){
        other_3 <- data[["crops_other3"]]
    }
    data[crop_name_columns] <- data %>%
        dplyr::select(crop_name_columns) %>%
        dplyr::mutate_all(replace_name_column_with_other, other_1,other_2,other_3)



    number_of_loops <- find_number_of_loops(data,"livestock_name")
    if(number_of_loops==0){
        warning('Could not find any "livestock_name_X" columns.')
        return(data)
    }


    livestock_name_columns <- paste0("livestock_name_",1:number_of_loops)
    other_1 <- NULL
    other_2 <- NULL
    other_3 <- NULL
    if ("livestock_other1" %in% colnames(data)){
        other_1 <- data[["livestock_other1"]]
    }
    if ("livestock_other2" %in% colnames(data)){
        other_2 <- data[["livestock_other2"]]
    }
    if ("livestock_other3" %in% colnames(data)){
        other_3 <- data[["livestock_other3"]]
    }
    data[livestock_name_columns] <- data %>%
        dplyr::select(livestock_name_columns) %>%
        dplyr::mutate_all(replace_name_column_with_other,other_1,other_2,other_3)



    return(data)

}

#' Replace Name Column with Other
#'
#' @param main_column The column you are searching for "other" values in (e.g. crop_name)
#' @param other_column1 The first "other" column you are searching through, e.g. "crops_other1"
#' @param other_column2 The second "other" column you are searching through, e.g. "crops_other2"
#' @param other_column3 The third "other" column you are searching through, e.g. "crops_other3"
#'
#' @return
#' @export
#'
#' @examples
replace_name_column_with_other<- function(main_column, other_column1,other_column2,other_column3){


    if(!is.null(other_column1)){
        index <- which(main_column=="other1")
        main_column[index] <- other_column1[index]
    }

    if(!is.null(other_column2)){
        index <- which(main_column=="other2")
        main_column[index] <-  other_column2[index]

    }

    if(!is.null(other_column3)){
        index <- which(main_column=="other3")
        main_column[index] <-  other_column3[index]

    }

    return(main_column)

}



#' Replace unit with Single Value
#'
#' For two columns, identify where
#' "other" is written, and replace
#' it with th appropriate text entry
#'
#' @param unit_column The main unit column
#' @param unit_other_column The column containing the free text entry
#'
#' @return
#' @export
#'
#' @examples
replace_unit_column_with_other_single <- function(unit_column, unit_other_column){


    index <- which(unit_column=="other")

    if(length(index)!=0)
    {
        unit_column[index]<- unit_other_column[index]
    }


    return(unit_column)

}


#' Replacing Units with other Units
#'
#' In the rhomis survey text entries are recorded
#' as "other". The actual text is stored in another variable.
#' This can be problematic for analysis. This function brings
#' the "other" units into where they are needed.
#'
#' @param data The whole rhomis dataset
#'
#' @return
#' @export
#'
#' @examples
replace_units_with_other_all <- function(data){



    #--------------------------------------------------------
    #
    #--------------------------------------------------------
    individual_units <- list("unitland"="areaunits_other",
                             "unitland_owned"="areaunits_other_own",
                             "unitland_rentin"="areaunits_other_rent",
                             "unitland_rentout"="areaunits_other_rent",
                             "fertiliser_units"="fertiliser_units_other")


    looped_units <- list(
        "crop_yield_units"="crop_yield_units_other",
        "crop_sold_price_quantityunits"="crop_price_quantityunits_other",
        "milk_units"="milk_amount_units_other",
        "milk_sold_price_timeunits"="milk_amount_time_units_other",
        "bees_honey_production_units"="bees_honey_production_units_other",
        "eggs_units"="eggs_amount_units_other",
        "eggs_sold_price_timeunits"="eggs_sold_price_timeunits_other"
    )

    looped_units_merged <- sapply(names(looped_units), function(x) {
        number_of_loops <- find_number_of_loops(data, x)
        if(number_of_loops>0)
        {
        main_column <- paste0(x,"_",1:number_of_loops)
        other_column <- paste0(looped_units[[x]],"_",1:number_of_loops)
        }
        if(number_of_loops==0)
        {
            main_column <- paste0(x,"_",1)
            other_column <- paste0(looped_units[[x]],"_",1)
        }
        setNames(other_column,main_column)
        }, simplify=T)

    looped_units_merged <- unlist(unname(looped_units_merged))

    units_to_change <- c(individual_units,looped_units_merged)


    result <- sapply(colnames(data), function(x){

        if(x %in% names(units_to_change))
        {
            if( units_to_change[[x]]%in% colnames(data))
            {

                other_column <- units_to_change[[x]]
                new_column <- replace_unit_column_with_other_single(data[[x]],data[[other_column]])
                return(new_column)
            }else{

                return(data[[x]])
            }



        }
        if(x %in% names(units_to_change)==F){
            return(data[[x]])
        }
    }, simplify = F)





    result <- tibble::as_tibble(result)

    return(result)

}





#' Write a List of DataFrames to Folder
#'
#' Take a list of multiple outputs and write them all to a single folder
#'
#' @param list_of_df The list of dataframes (must be a named list)
#' @param folder The name of the folder where they are to be written
#'
#' @return
#' @export
#'
#' @examples
write_list_of_df_to_folder <- function(list_of_df, folder){
    folder_name <- paste0("./",folder)
    dir.create(folder_name, showWarnings = F)


    sapply(names(list_of_df), function(x) {
        file_path <- paste0(folder_name,"/",x,".csv")
        readr::write_csv(list_of_df[[x]], file_path)
    })


}


