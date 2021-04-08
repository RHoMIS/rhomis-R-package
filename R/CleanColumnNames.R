
#---------------------------------------------------------------------------------------------------
#' Shorten Individual Name
#'
#' Used to abbreviate a single column name based on its last value. Typical RHoMIS column names come
#' in the for "xxxx/xxxxx/xxxxx/important_name". This function helps to extract only the final value
#' "important name".
#'
#' @param column_name The specific item which needs shortening
#' @param seperator The character separating parts of the item
#'
#' @return Shortened string of the essential column name
#' @export
#'
#' @examples
#'
#' long_name <- "xxxx/xxxxx/xxxxx/important_name"
#' shortened_name <- shorten_individual_column_name(column_name=long_name, seperator="/")
#' # Shortened name will be equal "important_name"
#'
shorten_individual_column_name <- function(column_name, seperator){
    split_name <- unlist(strsplit(column_name, seperator))
    return(split_name[length(split_name)])
}
#---------------------------------------------------------------------------------------------------
#' Shorten Multiple Column Names
#'
#' @param column_names The list of column names which need to be shortened
#' @param seperator  The string seperating parts of each column name
#'
#' @return A list of
#' @export
#'
#' @examples
#'
#' long_names <- c("xxxx/xxxxx/xxxxx/important_name_1",
#'                 "xxxx/xxxxx/xxxxx/important_name_2",
#'                 "xxxx/xxxxx/xxxxx/important_name_3")
#' seperator <- "/"
#'
#' split_list <- unlist(lapply(long_names,function(name) shorten_individual_column_name(name,seperator)))

shorten_multiple_column_names <- function(long_names, seperator){

    split_list <- unlist(lapply(long_names,function(name) shorten_individual_column_name(name,seperator)))
}
#---------------------------------------------------------------------------------------------------
#' Modify Loop Name
#'
#' @param column_name
#' @param loop_type
#' @param seperator
#'
#' @return
#' @export
#'
#' @examples
modify_loop_name <- function(column_name, loop_type){

    # Get rid of everything before the opening square bracket
    # Can modify and generalise to exclude the "loop_type" variable
    pattern <- paste0(".*",loop_type,"\\[")
    repeat_number <- gsub(pattern,"",column_name)
    # Get rid of everything after closing square bracket
    repeat_number <- gsub("\\].*","",repeat_number)

    # Combining original column name with extra number
    new_column_name <- paste0(column_name,"_",repeat_number)
    return(new_column_name)
}



#---------------------------------------------------------------------------------------------------

#' Modify Loop Names
#'
#' @param column_names The column names which need to be shortened
#' @param loop_type What loop_type of loop is concerned, the options include "crop_repeat", "livestock_repeat", "offfarm_income_repeat".
#' @param seperator The string separating parts of each column name
#'
#' @return A list of column names which need to be modified
#' @export
#'
#' @examples
modify_loop_column_names <- function(column_names, loop_type) {
    # Finding all of the items with pattern "xxx/type[number]/column_name"
    repeat_columns <- grep(paste0(loop_type,"\\[.\\]"),column_names)

    # Modifying the relevant columns
    column_names[repeat_columns] <- unlist(lapply(column_names[repeat_columns], function(x) modify_loop_name(x, loop_type)))
    return(column_names)

}
#---------------------------------------------------------------------------------------------------
#' Modify all types of loop in RHoMIS
#'
#' The RHoMIS survey is structured using a series of important loops. These are often labelled "crop_repeat[x]".
#' It is important to be modify all of these loops to have a simpler column name format for later analysis
#'
#' @param column_names A list of all of the column names which
#' @param repeat_columns A list of all of the types of repeat column which need to be changed
#'
#' @return
#' @export
#'
#' @examples
modify_all_loop_column_names <- function(column_names, repeat_columns){
    for (repeat_column in repeat_columns){
        column_names<-modify_loop_column_names(column_names, repeat_column)
    }
    return(column_names)
}
#---------------------------------------------------------------------------------------------------

#' Clean Column Names
#'
#' A single functions to shorten and clean all RHoMIS column names.
#'
#' @param column_names A list of all of the column names
#' @param seperator The seperator which seperates all of the parts of the column names
#' @param repeat_columns A list of all of the repeat loops contained in the RHoMIS dataset
#'
#' @return A cleaned list of all of the column names
#' @export
#'
#' @examples
#'
clean_column_names <- function(column_names, seperator, repeat_columns){
    column_names <- modify_all_loop_column_names(column_names, repeat_columns)
    column_names <- shorten_multiple_column_names(column_names,seperator)

    return(column_names)
}


