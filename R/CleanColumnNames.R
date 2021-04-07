
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
#'
shorten_multiple_column_names <- function(long_names, seperator){

    seperator <- "/"
    split_list <- unlist(lapply(long_names,function(name) shorten_individual_column_name(name,seperator)))
}



