
#' Check columns in data
#'
#' For many of the functions to work, we need to ensure that
#' the correct columns are present in the tibble. This
#' function checks whether the columns are present, both
#' for looped, and individual columns
#'
#' Rpackage file: DataContentsCheck.R
#'
#' @param data The dataset which needs checking
#' @param loop_columns The columns which are contained in a loop
#' @param individual_columns The columns which appear individually
#' @param warning_message The warning message (perhaps the name of the function you are calling)
#' to indicate where the problem may be happening. A warning message will already be included
#' if the data is missing. This warning information can be supplied to give some context
#'
#' @return
#' @export
#'
#' @examples
check_columns_in_data <- function(data, loop_columns = NULL, individual_columns = NULL, warning_message = NULL) {

  # data=data
  # loop_columns = c(
  # "bees_honey_sell_amount")
  # warning_message = "Could not calculate honey amounts sold or consumed"
  #

  missing_loop_columns <- c()
  missing_individual_columns <- c()

  if (!is.null(loop_columns)) {
    # columns <- c("bees_honey_sell_amount")
    missing_loop_columns <- check_columns_loop(data, loop_columns)
  }

  if (!is.null(individual_columns)) {
    missing_individual_columns <- check_columns_individual(data, individual_columns)
  }

  all_missing_columns <- c(
    missing_individual_columns,
    missing_loop_columns
  )

  if (length(all_missing_columns) > 0) {
    warning(paste0(
      "\n",
      warning_message,
      "\nThe following columns are missing from the dataset: \n",
      paste0("missing_variable: ", all_missing_columns, collapse = "\n"),
      "\n---------------------------------------------"
    ))
  }
  return(all_missing_columns)
}

#' Check Columns Loop
#'
#' Check whether columns are present in loop type data
#'
#' Rpackage file: DataContentsCheck.R
#'
#' @param data Data to be checked
#' @param columns Column patterns that we are verifying
#'
#' @return
#' @export
#'
#' @examples
check_columns_loop <- function(data, columns) {
  loop_columns <- sapply(columns, function(x) {
    number_of_loops <- find_number_of_loops(data, x)

    if (number_of_loops > 0) {
      loop_columns <- paste0(x, "_", 1:number_of_loops)
    } else {
      loop_columns <- x
    }
    return(loop_columns)
  }, simplify = F)

  single_columns_mask <- sapply(loop_columns, function(x) length(x) == 1)
  single_columns <- unname(unlist(loop_columns[single_columns_mask]))

  multiple_columns <- loop_columns[!single_columns_mask]

  all_loop_columns <- unname(unlist(multiple_columns))

  missing_columns <- all_loop_columns[all_loop_columns %in% colnames(data) == F]

  if (length(single_columns) > 0) {
    missing_columns <- append(missing_columns, single_columns)
  }

  return(missing_columns)
}

#' Check Columns Individual
#'
#' Checking whether individual columns are present in the data
#' we are examining
#'
#' Rpackage file: DataContentsCheck.R
#'
#' @param data The data we are checking
#' @param columns The column names we are looking for
#'
#' @return
#' @export
#'
#' @examples
check_columns_individual <- function(data, columns) {
  missing_columns <- columns[columns %in% colnames(data) == F]

  return(missing_columns)
}


check_all_columns <-function(file_path){

    dataset <- readr::read_csv(file_path,
                               col_types = readr::cols(),
                               na = c("n/a", "-999", "-99", "NA"),
                               locale = readr::locale(encoding = "UTF8"))


    columns_to_check <- colnames(dataset)


    rhomis::indicator_list
}
