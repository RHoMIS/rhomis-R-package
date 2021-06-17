
#' Check columns in data
#'
#' For many of the functions to work, we need to ensure that
#' the correct columns are present in the tibble. This
#' function checks whether the columns are present, both
#' for looped, and individual columns
#'
#' @param data The dataset which needs checking
#' @param loop_columns The columns which are contained in a loop
#' @param individual_columns The columns which appear individually
#'
#' @return
#' @export
#'
#' @examples
check_columns_in_data <- function(data, loop_columns=NULL,individual_columns=NULL){

    if (!is.null(loop_columns)){
        columns_exist <- check_columns_individual(data,loop_columns)
    }

    if (!is.null(individual_columns)){
        columns_exist <- check_columns_individual(data,loop_columns)
    }

}

#' Check Columns Loop
#'
#' Check whether columns are present in loop type data
#'
#' @param data Data to be checked
#' @param columns Column patterns that we are verifying
#'
#' @return
#' @export
#'
#' @examples
check_columns_loop <- function(data,columns){

}

#' Check Columns Individual
#'
#' Checking whether individual columns are present in the data
#' we are examining
#'
#' @param data The data we are checking
#' @param columns The column names we are looking for
#'
#' @return
#' @export
#'
#' @examples
check_columns_individual <- function(data,columns){

}
