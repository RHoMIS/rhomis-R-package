#' This script includes a series of functions which can be
#' used to help clean column names of data collected with RHoMIS.
#--------------------------------------------------------------
#' Shorten Individual Name
#'
#' Used to abbreviate a single column name
#' based on its last value. Typical RHoMIS
#' column names come in the form "xxxx/xxxxx/xxxxx/important_name".
#' This function helps to extract only the final value
#' "important name".
#'
#' Rpackage file: CleanColumnNames.R
#'
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
#' shortened_name <- shorten_individual_column_name(
#'   column_name = long_name,
#'   seperator = "/"
#' )
#' # Shortened name will be equal "important_name"
shorten_individual_column_name <- function(column_name, seperator) {
    split_name <- unlist(strsplit(column_name, paste0("\\", seperator)))
    return(split_name[length(split_name)])
}
#--------------------------------------------------------------
#' Shorten Multiple Column Names
#'
#' Used to abbreviate multiple column
#' names based on their last values.
#' Typical RHoMIS column names come
#' in the for "xxxx/xxxxx/xxxxx/important_name".
#' This function helps to extract only the final value
#' "important name". It will do this for multiple column names
#'
#' Rpackage file: CleanColumnNames.R
#'
#' @param long_names The list of column names which need to be shortened
#' @param seperator  The string seperating parts of each column name
#'
#' @return A list of shortened column names
#' @export
#'
#' @examples
#'
#' long_names <- c(
#'   "xxxx/xxxxx/xxxxx/important_name_1",
#'   "xxxx/xxxxx/xxxxx/important_name_2",
#'   "xxxx/xxxxx/xxxxx/important_name_3"
#' )
#' seperator <- "/"
#'
#' shortened_names <- shorten_multiple_column_names(long_names, seperator)
shorten_multiple_column_names <- function(long_names, seperator) {
    split_list <- unlist(lapply(long_names, function(name) shorten_individual_column_name(name, seperator)))
}
#--------------------------------------------------------------
#' Modify Loop Name
#'
#' Many of the variables in RHoMIS are
#' collected through a looping structure
#'
#' Rpackage file: CleanColumnNames.R
#'
#' @param column_name The individual column name to be changed
#' @param loop_type What is the loop type e.g. "crop"
#'
#' @return Returns a single item for the new column name
#' @export
#'
#' @examples
#'
#' column_name <- "SECTION_Crop_Productivity/crop_repeat[2]/crop_name"
#' loop_type <- "crop_repeat"
#' modified_name <- modify_loop_name(column_name, loop_type)
#' # Will return: "SECTION_Crop_Productivity/crop_repeat[2]/crop_name_2"
modify_loop_name <- function(column_name, loop_type) {
    if (grepl(paste0(loop_type, "\\."), column_name)) {
        open_bracket <- "\\."
        close_bracket <- "\\."
    }

    if (grepl(paste0(loop_type, "\\["), column_name)) {
        open_bracket <- "\\["
        close_bracket <- "\\]"
    }

    if (grepl(paste0(loop_type, "\\["), column_name) &
        grepl(paste0(loop_type, "\\."), column_name)) {
        stop("Data set contains both square brackets and dots in the column name\n
          therefore unable to identify which
          columns are looped and which are not")
    }
    # Get rid of everything before the opening square bracket
    # Can modify and generalise to exclude the "loop_type" variable
    pattern <- paste0(".*", loop_type, open_bracket)
    repeat_number <- gsub(pattern, "", column_name)
    # Get rid of everything after closing square bracket
    repeat_number <- gsub(paste0(close_bracket, ".*"), "", repeat_number)

    # Combining original column name with extra number
    new_column_name <- paste0(column_name, "_", repeat_number)
    return(new_column_name)
}


#' Modify Loop Names
#'
#' A function to modify column names for RHoMIS loops
#'
#' The RHoMIS survey is structured using a
#' series of important loops. These are often
#' labelled:
#' "xxxx/crop_repeat[1]/crop_name",
#' "xxxx/crop_repeat[2]/crop_name",
#' "xxxx/crop_repeat[3]/crop_name"...
#'
#' This function aims to modify those into a more
#' user-friendly format:
#' "crop_name_1",
#' "crop_name_2",
#' "crop_name_3" ...
#'
#' Rpackage file: CleanColumnNames.R
#'
#' @param column_names A list of the column names which
#' need to be shortened. This would generally be all of
#' the column names of the RHoMIS dataset.
#' @param loop_type What loop_type of loop is concerned,
#' this could include options such as
#' "crop_repeat", "livestock_repeat", "offfarm_income_repeat".
#' It is very important that this "type" matches the pattern
#' in the original column names
#'
#' @return A list of column names which need to be modified
#' @export
#'
#' @examples
#' original_columns <- c(
#'   "x/non_repeat_column",
#'   "xxxx/crop_repeat[1]/crop_name",
#'   "xxxx/crop_repeat[2]/crop_name"
#' )
#' loop_type <- "crop_repeat"
#'
#' modified_names <- modify_loop_column_names(original_columns, loop_type)
modify_loop_column_names <- function(column_names, loop_type) {
    repeat_columns <- grep(paste0(loop_type, "\\[.\\]"), column_names)
    double_repeat_columns <- grep(paste0(loop_type, "\\[..\\]"), column_names)
    triple_repeat_columns <- grep(paste0(loop_type, "\\[...\\]"), column_names)

    if (length(repeat_columns) == 0) {
        repeat_columns <- grep(paste0(loop_type, "\\.[[:digit:]]\\."), column_names)
        double_repeat_columns <- grep(paste0(loop_type, "\\.[[:digit:]][[:digit:]]\\."), column_names)
        triple_repeat_columns <- grep(paste0(loop_type, "\\.[[:digit:]][[:digit:]][[:digit:]]\\."), column_names)
    }

    if (length(triple_repeat_columns) > 0) {
        repeat_columns <- c(repeat_columns, triple_repeat_columns)
        repeat_columns <- sort(repeat_columns)
    }
    if (length(double_repeat_columns) > 0) {
        repeat_columns <- c(repeat_columns, double_repeat_columns)
        repeat_columns <- sort(repeat_columns)
    }


    # Modifying the relevant columns
    column_names[repeat_columns] <- unlist(lapply(
        column_names[repeat_columns],
        function(x) {
            modify_loop_name(x, loop_type)
        }
    ))
    return(column_names)
}
#--------------------------------------------------------------
#' Modify all types of loop in RHoMIS
#'
#' The RHoMIS survey is structured using
#' a series of important loops. These are
#' often labelled "crop_repeat[x]". It is
#' important to be modify all of these loops
#' to have a simpler column name format for later analysis
#'
#' Rpackage file: CleanColumnNames.R
#'
#' @param column_names A list of all of the column names which
#' @param repeat_columns A list of all of the types
#' of repeat column which need to be changed
#'
#' @return
#' @export
#'
#' @examples
#' repeat_columns <- c(
#'   "crop_repeat",
#'   "livestock_repeat",
#'   "offfarm_repeat",
#'   "hh_rep"
#' )
#' column_names <- c(
#'   "xxx/crop_repeat[1]/crop_name",
#'   "xxx/livestock_repeat[2]/livestock_name",
#'   "xxx/crop_repeat[3]/crop_name",
#'   "xx/crop_repeat/crop_name",
#'   "x/offfarm_repeat[4]/offfarm_name",
#'   "y/hh_rep[5]/person_name",
#'   "z/crop_repeat/crop_name"
#' )
#' # Will return
#' # c("xxx/crop_repeat[1]/crop_name_1",
#' # "xxx/livestock_repeat[2]/livestock_name_2",
#' # "xxx/crop_repeat[3]/crop_name_3",
#' # "xx/crop_repeat/crop_name",
#' # "x/offfarm_repeat[4]/offfarm_name_4",
#' # "y/hh_rep[5]/person_name_5",
#' # "z/crop_repeat/crop_name")
modify_all_loop_column_names <- function(column_names, repeat_columns) {
    for (repeat_column in repeat_columns) {
        column_names <- modify_loop_column_names(column_names, repeat_column)
    }
    return(column_names)
}

#' Clean Column Names
#'
#' A single functions to shorten and
#' clean all RHoMIS column names.
#'
#' Rpackage file: CleanColumnNames.R
#'
#' @param column_names A list of all of the column names
#'
#' @return A cleaned list of all of the column names
#' @export
#'
#' @examples
#'
#' repeat_columns <- c(
#'   "crop_repeat",
#'   "livestock_repeat",
#'   "offfarm_repeat",
#'   "hh_rep"
#' )
#' seperator <- "/"
#' column_names <- c(
#'   "xxx/crop_repeat[1]/crop_name",
#'   "xxx/livestock_repeat[2]/livestock_name",
#'   "xxx/crop_repeat[3]/crop_name",
#'   "xx/crop_repeat/crop_name",
#'   "x/offfarm_repeat[4]/offfarm_name",
#'   "y/hh_rep[5]/person_name",
#'   "z/crop_repeat/crop_name"
#' )
#'
#' # Will return:
#' # c("crop_name_1",
#' # "livestock_name_2",
#' # "crop_name_3",
#' # "crop_name",
#' # "offfarm_name_4",
#' # "person_name_5",
#' # "crop_name")
clean_column_names <- function(column_names) {

  # list of possible separators for columns in the raw rhomis survey data
  separator_list <- c("\\.", "/", "-")
  repeat_columns <- identify_repeat_columns(column_names)
  new_column_names <- column_names
  # loop over the list of separators to identify
  # the one that applies in this case
  for (sep in separator_list) {
    # check whether the current sep exists in a majority of column name fields
    if (length(grep(sep, new_column_names)) > length(new_column_names)/2) {
      # remove the leading "\\" (needed for "." special char when using grep)
      separator <- gsub("\\\\", "", sep)
      # loop over the columns and modify them
      new_column_names <- modify_all_loop_column_names(new_column_names, 
                                                       repeat_columns)
      new_column_names <- shorten_multiple_column_names(new_column_names, 
                                                        separator)
      # no need to continue loop if relevant sep has been identified
      break
    }
  }
  # make sure column names are all lower case
  new_column_names <- tolower(new_column_names)
  
  # check and replace duplicated
  isduplicated <- duplicated(new_column_names, fromLast = T) | 
    duplicated(new_column_names, fromLast = F)
  if (any(isduplicated)) {
    warning(paste0("\nCleaning of column names has resulted in duplicates.\n", 
                   "Each duplicate will have an underscore as a prefix.\n", 
                   "Duplicated columns can be found below:\n", paste0(new_column_names[isduplicated], 
                                                                      collapse = "\n"), collapse = ""))
  }
  i <- 0
  column_names_slah <- column_names
  while (any(isduplicated) & i < 5) {
    namesduplicated <- column_names_slah[isduplicated]
    pos_slash <- regexpr("/[^/]*$", namesduplicated)
    new_duplicated <- paste(substr(namesduplicated, 
                                   1, pos_slash - 1), substr(namesduplicated, pos_slash + 
                                                               1, nchar(namesduplicated)), sep = "-")
    column_names_slah[isduplicated] <- new_duplicated
    new_duplicated <- modify_all_loop_column_names(new_duplicated, 
                                                   repeat_columns)
    new_duplicated <- shorten_multiple_column_names(new_duplicated, 
                                                    separator)
    new_column_names[isduplicated] <- tolower(new_duplicated)
    isduplicated <- duplicated(new_column_names, fromLast = T) | 
      duplicated(new_column_names, fromLast = F)
    i <- i + 1
  }
  #if duplicates remains, add a number
  if (any(isduplicated)){
    new_column_names[isduplicated] <- paste(new_column_names[isduplicated], 1:sum(isduplicated), sep="-")
  }
  return(new_column_names)
}


#' Identify Repeat Columns
#'
#' Identify repeat loop columns in a rhomis
#' dataset
#'
#' @param column_names The list of column names
#'
#' @return
#' @export
#'
#' @examples
identify_repeat_columns <- function(column_names){

    pattern <- paste0("[[:word:]]+[[:punct:]][[:digit:]]+[[:punct:]]")
    repeats <- stringr::str_extract(column_names, pattern)

    repeats <- gsub("[[:punct:]][[:digit:]]+[[:punct:]]", "",repeats)
    repeats <- repeats[!is.na(repeats)]
    repeats <- unique(repeats)

    return(repeats)






}
