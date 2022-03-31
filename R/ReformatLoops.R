





#' Find number of loops
#'
#' The RHoMIS data is arranged in a looping structure.
#' This function helps identify how many loops there are
#' for example for a variable like "crop_name".
#'
#' @param data The data containing the loops
#' @param name_column The "nam" column for the loops you are looking at, for example "crop_name"
#'
#' @return A single number, the number of loops for that variable
#' @export
#'
#' @examples
#' name_column <- "crop_name"
#' data <- tibble::as_tibble(list(
#'   crop_name_1 = c("banana", "cassava", NA, "millet"),
#'   crop_name_2 = c("cassava", NA, "melon", "maize"),
#'   random_crop_name_2 = c("blue", "green", "red", NA),
#'   crop_name = c("orange", "purple", NA, "black")
#' ))
#' find_number_of_loops(data, name_column)
find_number_of_loops <- function(data, name_column) {
  regex_pattern <- paste0("^", name_column, "_[[:digit:]]") # Finding columns which start with "column_pattern_Integer"
  relevant_columns <- grep(regex_pattern, colnames(data), value = T)
  number_of_loops <- length(relevant_columns)
  return(number_of_loops)
}


#' Find Unique Names
#'
#' A function to find all of the unique names for a particular looped variable (e.g. livestock, crops, off-farm incomes)
#'
#' @param data The data-frame containing the loops of concern
#' @param name_column The original name of the loop (e.g. "crop_name")
#'
#' @return A list of all of the unique entries for the name column
#' @export
#'
#' @examples
#' name_column <- "crop_name"
#' data <- tibble::as_tibble(list(
#'   crop_name_1 = c("banana", "cassava", NA, "millet"),
#'   crop_name_2 = c("cassava", NA, "melon", "maize"),
#'   random_crop_name_2 = c("blue", "green", "red", NA),
#'   crop_name = c("orange", "purple", NA, "black")
#' ))
#' expected_result <- c("banana", "cassava", "millet", "melon", "maize")
#' find_unique_case(data, name_column)
find_unique_case <- function(data, name_column) {
  number_of_loops <- find_number_of_loops(data, name_column)
  name_columns <- data[, paste0(name_column, "_", 1:number_of_loops)]

  # Finding all of the unique values
  all_values <- unname(unlist(lapply(name_columns, function(x) unique(x))))
  # Removing NA column
  all_values <- all_values[!is.na(all_values)]
  # Finding unique values from all the columns
  unique_values <- unique(all_values)
  if (length(unique_values) == 0) {
    unique_values <- c("none")
  }
  return(unique_values)
}


#' Item Number to Column Row Conversion
#'
#' Converting from loop to a format that is easy to analyze.
#'
#' @param data The data containing the loops you need to convert
#' @param name_column The column containing the variable which will eventually
#' column header (see example)
#' @param variable_to_convert What is the variable you would like to link to the name column
#' @param type What is the type of variable you are expecting? "chr" for character,
#' "num" for numeric, "int" for interger, and "fct" for factor.
#'
#'
#' @return
#' @export
#'
#' @examples
#'
#' name_column <- "crop_name"
#' variable_to_convert <- "crop_variable"
#' data <- tibble::as_tibble(list(
#'   crop_name_1 = c("banana", "cassava", NA, "millet"),
#'   crop_name_2 = c("cassava", NA, "melon", "maize"),
#'   random_crop_name_2 = c("blue", "green", "red", NA),
#'   crop_name = c("orange", "purple", NA, "black"),
#'   crop_variable_1 = c("ex1", "ex2", NA, NA),
#'   crop_variable_2 = c("ex3", NA, "ex4", "ex5")
#' ))
#' number_of_loops <- find_number_of_loops(data, name_column)
#' loop_to_column_conversion(data, name_column, variable_to_convert, type = "chr")
loop_to_column_conversion <- function(data, name_column, variable_to_convert, type) {
  unique_names <- find_unique_case(data, name_column)
  number_of_loops <- find_number_of_loops(data, name_column)
  # Obtaining a table of the loop values
  value_table <- data[, paste0(variable_to_convert, "_", 1:number_of_loops)]
  value_table$indexValues <- row.names(value_table)
  value_table <- value_table %>%
    dplyr::group_by(indexValues) %>%
    tidyr::gather(key = "column", value = "value", -indexValues)

  # Obtaining a table of the loop names
  name_table <- data[, paste0(name_column, "_", 1:number_of_loops)]
  name_table$indexNames <- row.names(name_table)
  name_table <- name_table %>%
    dplyr::group_by(indexNames) %>%
    tidyr::gather(key = "column", value = "name", -indexNames)

  # Merging table of loop values, names and indexes
  merged_table <- cbind(name_table[, c("name", "indexNames")], value_table[, c("indexValues", "value")])

  if (!all(merged_table$indexNames == merged_table$indexValues)) {
    stop("Indexes don't match when merging loop information")
  }

  # Converting the long table to wide format
  merged_table$index <- merged_table$indexNames
  merged_table <- merged_table[, c("index", "name", "value")]
  if (all(is.na(merged_table$name))) {
    merged_table$name <- "none"
  }
  merged_table <- merged_table[!duplicated(merged_table[, c("index", "name")]), ]
  merged_table <- merged_table %>%
    dplyr::group_by(index) %>%
    tidyr::pivot_wider(id_cols = index, names_from = name, values_from = value)
  if (!all(merged_table$index == row.names(merged_table))) {
    stop("Indexes don't match when merging loop information")
  }

  merged_table <- tibble::as_tibble(merged_table[, !colnames(merged_table) %in% c("NA", "index")])
  if (type == "chr") {
    merged_table <- merged_table %>% dplyr::mutate_all(as.character)
  }
  if (type == "num") {
    merged_table <- merged_table %>% dplyr::mutate_all(as.numeric)
  }
  if (type == "int") {
    merged_table <- merged_table %>% dplyr::mutate_all(as.integer)
  }
  if (type == "fct") {
    merged_table <- merged_table %>% dplyr::mutate_all(as.factor)
  }
  return(merged_table)
}


#' Map To Wide Format
#'
#' A function to convert RHoMIS loops into a wider named format
#'
#' @param data The original dataframe containing the data to be reformatted
#' @param column_prefixes The columns which need to be converted to the correct format
#' @param types The data types for each of these columns "chr" for character,
#' "num" for numeric, "int" for interger, and "fct" for factor.
#' @param name_column The name column (e.g. crop_name_1)
#'
#' @return a list of data frames for each of the variables
#' @export
#'
#' @examples
#'
#' name_column <- "crop_name"
#' column_prefixes <- c("crop_variable", "crop_unit", "crop_price")
#' types <- c("chr", "chr", "chr")
#'
#' data <- tibble::as_tibble(list(
#'   crop_name_1 = c("banana", "cassava", NA, "millet"),
#'   crop_name_2 = c("cassava", NA, "melon", "maize"),
#'   random_crop_name_2 = c("blue", "green", "red", NA),
#'   crop_name = c("orange", "purple", NA, "black"),
#'   crop_variable_1 = c("ex1", "ex2", NA, NA),
#'   crop_variable_2 = c("ex3", NA, "ex4", "ex5"),
#'   crop_unit_1 = c("unit1", "unit2", "unit3", NA),
#'   crop_unit_2 = c(NA, NA, "unit4", "unit5"),
#'   crop_price_1 = c(NA, NA, NA, NA),
#'   crop_price_2 = c(NA, NA, NA, NA)
#' ))
#' map_to_wide_format(data, name_column, column_prefixes, types)
map_to_wide_format <- function(data, name_column, column_prefixes, types) {
  reformatted_variables <- lapply(c(1:length(column_prefixes)), function(x) {
    loop_to_column_conversion(
      data = data,
      name_column = name_column,
      variable_to_convert = column_prefixes[x],
      type = types[x]
    )
  })
  names(reformatted_variables) <- column_prefixes

  return(reformatted_variables)
}

#-----------------------------------------
### Gender Splits for the data

#' Proportion or NA
#'
#' For a particular gender control item, e.g. "female_adult male_adult" this function
#' works out whether to return NA or a proportion. In this case the proportion controlled
#' by each person would be 0.5
#'
#' @param item A list of the people controlling a particular resource
#'
#' @return
#' @export
#'
#' @examples
#' item <- c("male_adult", "female_adult")
#' prop_or_na(item) # should return 0.5
prop_or_na <- function(item) {
  if (length(item) == 1) {
    if (is.na(item)) {
      return(NA)
    }
  }
  return(1 / length(item))
}

#' Number Controlling Resource
#'
#' For the RHoMIS gender columns, we ask who in the household collects each resource.
#' This function is designed to be applied to each item (spicific row for specific column)
#'
#'
#' @param item The specific string, for which we want to count the
#' number controlling the resource
#'
#' @return A proportion (0-1) for the people controlling resource
#' @export
#'
#' @examples
#'
#' column <- c("male_adult female_adult", "male_adult", NA, "female_youth")
#' proportion_control_per_person(column)
#'
# item <- c("male_adult female_adult")
# proportion_control_per_person(item)
proportion_control_per_person <- function(item) {
  if (all(is.na(item)) == F) {
    item <- strsplit(item, " ")
  }
  # Avoiding Duplicates
  item <- lapply(item, function(x) unique(x))
  # Counting number of people controlling
  item <- unlist(lapply(item, function(x) prop_or_na(x)))
  return(item)
}

#' Check Value is in List
#'
#' In the case of the gender calculations, this function can check whether
#' a particular gender category is found in a string.
#'
#' @param item A list of gender control strings (see example)
#' @param category The category you are trying to identify
#'
#' @return
#' @export
#'
#' @examples
#'
#' item <- c("male_adult female_adult", "male_adult", NA, "female_youth")
#' category <- "female_youth"
#' check_val_in_list(item, category)
#'
#' item <- c("male_adult female_adult", "male_adult", NA, "female_youth")
#' category <- "male_adult"
#' check_val_in_list(item, category)
check_val_in_list <- function(item, category) {
  if (all(is.na(item)) == F) {
    item <- strsplit(item, " ")
  }
  item <- unlist(lapply(item, function(x) category %in% x))

  return(as.numeric(item))
}

#' Gender Control Props
#'
#' Finds the gender control score for a single category (e.g. "male_adult").
#' See example
#'
#' @param genderdf A data frame of gender control strings in the wide format
#' @param numberControllingDF A dataframe indicating the proportion of a resource
#' to be divided up between household members
#' @param category Which category we want to use to calculate the gender control
#' scores (e.g. "male_adult")
#'
#' @return
#' @export
#'
#' @examples
#'
#' # A dataset in the conventional RHoMIS format
#' data <- tibble::as_tibble(list(
#'   crop_name_1 = c("banana", "cassava", NA, "millet"),
#'   crop_name_2 = c("cassava", NA, "melon", "maize"),
#'   random_crop_name_2 = c("blue", "green", "red", NA),
#'   crop_name = c("orange", "purple", NA, "black"),
#'   crop_control_1 = c(
#'     "male_adult female_adult",
#'     "male_adult",
#'     NA,
#'     "female_youth"
#'   ),
#'   crop_control_2 = c("male_youth", NA, "female_youth", "female_adult")
#' ))
#' # Converting the dataset to the wide format
#' # (i.e crops as header, and genders controlling for each crop)
#' wide_data <- map_to_wide_format(
#'   data,
#'   "crop_name",
#'   c("crop_control"),
#'   c("chr")
#' )
#' genderdf <- wide_data$crop_control
#'
#' numberControllingDF <- genderdf %>%
#'   dplyr::mutate(across(
#'     .cols = everything(),
#'     ~ proportion_control_per_person(.x)
#'   ))
#' category <- "male_adult"
#' gender_control_props(genderdf, numberControllingDF, category)
gender_control_props <- function(genderdf, numberControllingDF, category) {
  value <- genderdf %>% dplyr::mutate(across(.cols = everything(), ~ check_val_in_list(item = .x, category = category)))
  return(tibble::as_tibble(value * numberControllingDF))
}


#' Split Gender Data
#'
#' For much of the RHoMIS data
#'
#' @param genderdf A dataframe of gendered control produced
#' by the "map_to_wide_format" (see example)
#' @param gender_categories The categories in the dataset to split by (e.g. "male_youth", "male_adult"...)
#'
#' @return
#' @export
#'
#' @examples
#' data <- tibble::as_tibble(list(
#'   crop_name_1 = c("banana", "cassava", NA, "millet"),
#'   crop_name_2 = c("cassava", NA, "melon", "maize"),
#'   random_crop_name_2 = c("blue", "green", "red", NA),
#'   crop_name = c("orange", "purple", NA, "black"),
#'   crop_control_1 = c(
#'     "male_adult female_adult",
#'     "male_adult",
#'     NA,
#'     "female_youth"
#'   ),
#'   crop_control_2 = c("male_youth", NA, "female_youth", "female_adult")
#' ))
#' wide_data <- map_to_wide_format(
#'   data,
#'   "crop_name",
#'   c("crop_control"), c("chr")
#' )
#' gender_data <- wide_data$crop_control
#' split_gender_data(gender_data)
split_gender_data <- function(genderdf,
                              gender_categories = pkg.env$gender_categories) {
  numberPeopleControlling <- genderdf %>% dplyr::mutate(across(.cols = everything(), ~ proportion_control_per_person(.x)))

  genderControlDFs <- lapply(gender_categories, function(x) gender_control_props(genderdf = genderdf, numberControllingDF = numberPeopleControlling, category = x))
  names(genderControlDFs) <- gender_categories
  return(genderControlDFs)
}

#' Split Gender columns
#'
#' A function to split gender columns into multiple
#' columns based on the different groups
#'
#'
#' @param column The column that needs to be split
#' @param gender_categories The gender categories to
#' be examined in the rhomis dataset
#' @return
#' @export
#'
#' @examples
split_gender_columns <- function(column,
                                 gender_categories = pkg.env$gender_categories) {
  numberPeopleControlling <- proportion_control_per_person(column)

  controlling_df <- sapply(gender_categories, function(x) check_val_in_list(column, category = x))

  prop_controlled <- tibble::as_tibble(controlling_df * numberPeopleControlling)

  return(prop_controlled)
}


#' Insert Gender Columns in Core Data
#'
#' Splitting a particular column in the RHoMIS
#' data by the genders groups which control that resource
#'
#' @param data The original data set containing the numeric data
#' and the gender control column used to split it
#' @param original_column The original numeric column which
#' needs to be split by gender
#' @param control_column The column indicating the groups which are
#' controlling a resource. Must include the values "male_adult","female_adult" etc...
#' @param loop_structure Indicating whether or not the controlled
#' resource is located within a loop structure. So far all gender control
#' columns are located within this looping structure
#' @param gender_control_categories The gender control categories included in the survey.
#' @return
#' @export
#'
#' @examples
insert_gender_columns_in_core_data <- function(data,
                                               original_column,
                                               control_column,
                                               loop_structure = F,
                                               gender_control_categories = pkg.env$gender_categories) {
  if (loop_structure == T) {
    number_of_loops <- find_number_of_loops(data, original_column)

    original_columns_all <- paste0(original_column, "_", c(1:number_of_loops))
    control_columns_all <- paste0(control_column, "_", c(1:number_of_loops))

    # data[[original_columns_all]] <- data[[original_columns_all]] %>% dplyr::mutate_all(as.numeric)

    control_split <- lapply(c(1:number_of_loops), function(x) tibble::as_tibble(as.numeric(data[[original_columns_all[x]]]) * split_gender_columns(data[[control_columns_all[x]]], gender_control_categories)))
    names(control_split) <- original_columns_all


    control_split <- collapse_list_of_tibbles(control_split)

    for (gender_cat in gender_control_categories) {
      data <- add_column_after_specific_column(
        data = data,
        new_data = control_split,
        new_column_name = paste0(gender_cat, "_", original_column),
        old_column_name = control_column,
        loop_structure = T
      )
    }

    return(data)
  }
}



#' Central Loops to RHoMIS Core
#'
#' ODK central data comes in a different format to the conventional RHoMIS
#' datasets. This function allows us to convert the ODK central
#' loops into the format needed for the indicator calculations to properly
#' function
#'
#' @param central_core Core data downloaded from ODK central
#' @param loop_data The sheet of loops which needs to be appended to the dataset
#'
#' @return
#' @export
#'
#' @examples
central_loops_to_rhomis_loops <- function(central_core, loop_data) {
  # The key for the loop data is "PARENT_KEY"
  # The key for the core data is "KEY"
  regex_pattern <- paste0("\\[+\\d+\\]") # Finding columns which start with "column_pattern_Integer"
  repeat_numbers <- stringr::str_extract(loop_data$KEY, regex_pattern)
  repeat_numbers <- as.numeric(gsub("[[:punct:]]", "", repeat_numbers))
  loop_data$rep_number <- repeat_numbers
  number_of_loops_max <- max(repeat_numbers)

  list_of_loops <- lapply(c(1:number_of_loops_max), function(x) {
    tibble::as_tibble(loop_data[loop_data$rep_number == x, ])
  })

  for (index in 1:length(list_of_loops))
  {
    colnames(list_of_loops[[index]])[colnames(list_of_loops[[index]]) != "PARENT_KEY"] <- paste0(colnames(list_of_loops[[index]])[colnames(list_of_loops[[index]]) != "KEY"], "_", index)
  }


  rearranged_loop <- list_of_loops %>% purrr::reduce(dplyr::full_join, by = "PARENT_KEY")

  final_data <- dplyr::full_join(central_core, rearranged_loop, by = c("KEY" = "PARENT_KEY"))
  final_data <- final_data[grepl("PARENT_KEY_[[:digit:]]", colnames(final_data)) == F]
  final_data <- final_data[grepl("rep_number_[[:digit:]]", colnames(final_data)) == F]

  return(final_data)
}