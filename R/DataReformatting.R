library(tibble)
library(tidyr)
library(dplyr)


#' Split string to dummy columns
#'
#' Many RHoMIS columns come in the format c("cropA cropB cropC", "cropD cropA").
#' This function helps split these into more interperatable dummy columns
#'
#' @param x A vector which needs to be split
#' @param seperator The separating character
#'
#' @return
#' @export
#'
#' @examples
split_string_categories_to_dummy <- function(x, seperator)
{
    x[is.na(x)] <- "NA"
    x <- tolower(x)
    split <- strsplit(x, seperator, fixed = T)
    all_potential_value <- unique(unlist(strsplit(x, seperator,
                                                  fixed = T)))
    boolean_nested_list <- lapply(split, function(x) create_nested_lest(longer_list = all_potential_value,
                                                                        shorter_list = x))
    df_to_return <- tibble::as_tibble(do.call(rbind, boolean_nested_list),check.names = F)

    return(df_to_return)
}

create_nested_lest <- function (longer_list, shorter_list)
{
    temp_list <- longer_list %in% shorter_list
    names(temp_list) <- longer_list
    return(temp_list)
}

#' Merge original and other units
#'
#' Many of the RHoMIS variables come from multiple
#' select, with the option to specify "other" by
#' free-text entry. This function allows you to bring
#' the "other" category into the main column, facilitating analysis.
#'
#'
#' @param data The data you would like to convert
#' @param main_column The main column you would like to add the "other" options
#' @param other_column The name of the "other" column where you would like to
#' integrate your values
#' @param loop_structure Whether or not the "other" columns you are converting come
#' in a loop, or whether it is simple a pair of columns
#'
#' @return
#' @export
#'
#' @examples
#'



merge_original_and_other_units <- function(data, main_column,other_column,loop_structure){
    if (loop_structure==T){
        number_of_loops <- find_number_of_loops(data,main_column)
        new_column_names <- paste0(main_column,"_", c(1:number_of_loops))
        new_columns <- sapply(c(1:number_of_loops),
                              function(x) merge_original_and_other_unit_single_pair(data[[paste0(main_column,"_",x)]],data[[paste0(other_column,"_",x)]])
        )
        colnames(new_columns) <- new_column_names
        new_columns <- tibble::as_tibble(new_columns)
        return(new_columns)

    }
    if(loop_structure==F){
        new_values <- merge_original_and_other_unit_single_pair(data[[main_column]],data[[other_column]])
        new_column <- tibble::as_tibble(list(x=new_values))
        colnames(new_column) <- main_column

        return(new_column)
    }
}


#' Merge original and other units pair
#'
#' Many of the RHoMIS variables come from multiple
#' select, with the option to specify "other" by
#' free-text entry. This function allows you to bring
#' the "other" category into the main column, for a single pair of columns.
#'
#' @param original_column the original column you would like to change
#' @param other_column the other values that need to be brought into the main column
#'
#' @return
#' @export
#'
#' @examples
merge_original_and_other_unit_single_pair <- function(original_column, other_column){
    original_column[!is.na(other_column)] <- other_column[!is.na(other_column)]
    return(original_column)
}





#' Add column after specific column
#'
#' During the data processing, some new columns need
#' to be added, with unit conversions or newly calculated
#' values. This function allows us to add those columns
#' in specific locations to make analysis easier.
#'
#' @param data The data containing the original columns
#' @param new_data The new data which we want to include
#' @param new_column_name The column name (or name pattern for loops) for the
#' new data we would like to add
#' @param old_column_name The marker column name (or name pattern for loops)
#' where we want to add the new data
#' @param loop_structure A boolean indicating whether this information is
#' in a loop format (TRUE) or a single pair of variables (FALSE)
#'
#' @return
#' @export
#'
#' @examples
add_column_after_specific_column <- function(data,new_data, new_column_name=NULL, old_column_name, loop_structure){
    if(loop_structure==T){
        if (is.null(new_column_name)){
            stop("When adding new columns to a loop structure, must specify the base name for the new columns")
        }
        number_of_loops_old <- find_number_of_loops(data,name_column = old_column_name)
        number_of_loops_new <- find_number_of_loops(new_data,name_column = new_column_name)
        if(number_of_loops_old!=number_of_loops_new){
            stop("The datasets you are merging do not have the same number loops")
        }

        for(loop in 1:number_of_loops_old){
            new_column_to_add <- new_data[paste0(new_column_name,"_",loop)]
            data <-tibble::add_column(.data=data, new_column_to_add,.after=paste0(old_column_name,"_",loop))
        }

        return(data)

    }

    if(loop_structure==F){
        data <- tibble::add_column(.data=data, new_data,.after=old_column_name)
        return(data)
    }
}


#' Proportions for individual proportion types
#'
#' A function for calculating the numeric proportions of crops which are sold,
#' consumed, or fed to livestock
#'
#' @param data A standard RHoMIS data set
#' @param use The use of crops being examined
#' This could include "eat", "sell", "feed_livestock"
#' @param use_column The column which includes the uses for this item
#' @param loop_number The number of the loop which is being processed
#' @param prop_column The column containing the proportions for this use
#'
#' @return
#' @export
#'
#' @examples
proportions_calculation <- function(data, use,use_column, prop_column, loop_number=NULL){

    if(use!="sell"&use!="eat"&use!="feed_livestock" &use!="use"){
        stop("Invalid 'use' defined for crop proportions")
    }

    if (!is.null(loop_number))
    {
        use_data <- data[[paste0(use_column,"_",loop_number)]]
        proportions_data <- data[[paste0(prop_column,"_",loop_number)]]
    }

    if (is.null(loop_number))
    {
        use_data <- data[[use_column]]
        proportions_data <- data[[prop_column]]
    }


    single_uses <- strsplit(use_data," ")
    single_uses <- sapply(single_uses,function(x)length(x))
    single_uses <- single_uses==1 & !is.na(use_data) & grepl(use,use_data)

    proportions_data <- switch_units(proportions_data, units = proportion_conversions$unit, conversion_factors = proportion_conversions$conversion)
    proportions_data[single_uses]<-1

    return(proportions_data)

}


#' Collapse list of tibbles
#'
#' A useful function for collapsing a list of tibbles,
#' used predominantly for gender calculations
#'
#' @param list_of_tibbles A list of tibbles which need to be collapsed
#'
#' @return
#' @export
#'
#' @examples
collapse_list_of_tibbles <- function(list_of_tibbles){
    new_data <- lapply(names(list_of_tibbles), FUN=function(x){
        temp_data <- list_of_tibbles[[x]]
        colnames(temp_data) <- paste0(colnames(temp_data),"_",x)
        return(temp_data)

    })

    new_data <- dplyr::bind_cols(new_data)

    return(new_data)
}

