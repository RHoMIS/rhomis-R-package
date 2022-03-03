library(readr)
library(tibble)
library(dplyr)
library(purrr)
library(magrittr)


#' Make Per Project conversion tibble
#'
#' If no conversion table is provided, construct
#' a conversion tibble, including project IDs, from the
#' the in built conversion tables
#'
#' @param proj_id_vector A vector of project IDs
#' @param unit_conv_tibble A conversion table containing columns "survey_value" and "conversion_factor"
#'
#' @return
#' @export
#'
#' @examples
make_per_project_conversion_tibble <- function(
        proj_id_vector,
        unit_conv_tibble

){

    individual_ids <- unique(proj_id_vector)

    by_project_conv <- sapply(individual_ids, function(x) {
        temp_conv_table <- unit_conv_tibble
        temp_conv_table$id_rhomis_dataset <- x
        return(temp_conv_table)

    }, simplify = F) %>% dplyr::bind_rows()

    return(by_project_conv)

}


#' Switch Units
#'
#' In RHoMIS, sometimes lists or data frames contain units which need to be converted
#' from a character format (e.g. "kg","sacks_100kg") into a numeric conversion (e.g. 1, 100).
#' This function does just that.
#'
#' @param unit_tibble A tibble containing
#' @param data_to_convert This list or data frame which needs to be converted
#' @param id_vector Column containing the RHoMIS id, used to subset the units by project data frame
#'
#' @return Returns the data (in its original format) with the units in their numerical format
#' @export
#'
#' @examples
#'# Converting a list
#'
switch_units <- function(data_to_convert, unit_tibble, id_vector){

    # Converting the two lists into a tibble which can be searched

    if ("tbl" %in% class(data_to_convert) | "tbl_df" %in% class(data_to_convert) | "data.frame" %in% class(data_to_convert))
    {
        converted_data <- lapply(data_to_convert, function(x){
            household_data_tibble <- tibble::as_tibble(
                list(
                    survey_value=x,
                    id_rhomis_dataset=id_vector
                ))


             converted_data <- dplyr::left_join(household_data_tibble,
                                                unit_tibble,
                                                by=c("id_rhomis_dataset"="id_rhomis_dataset", "survey_value"="survey_value"))
             converted_data
            return(converted_data[["conversion"]])
        }) %>% dplyr::bind_cols()

        # converted_data <- data_to_convert %>% purrr::map_df(function(x) replace_unit_list(x,unit_conv_tibble))
        return(converted_data)

    }
    if((is.list(data_to_convert) | is.vector(data_to_convert)) & ("tbl" %in% class(data_to_convert)==F & "tbl_df" %in% class(data_to_convert)==F & "data.frame" %in% class(data_to_convert)==F))
    {

        household_data_tibble <- tibble::as_tibble(
            list(
                survey_value=data_to_convert,
                id_rhomis_dataset=id_vector
            )
        )

        converted_data <- dplyr::left_join(household_data_tibble,
                         unit_tibble,
                         by=c("id_rhomis_dataset"="id_rhomis_dataset", "survey_value"="survey_value"))

        return(converted_data[["conversion"]])

    }

    warning("Could not identify type of list")
    return(data_to_convert)

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


