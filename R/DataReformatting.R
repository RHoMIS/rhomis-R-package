




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
split_string_categories_to_dummy <- function(x, seperator) {
  x[is.na(x)] <- "NA"
  x <- tolower(x)
  split <- strsplit(x, seperator, fixed = T)
  all_potential_value <- unique(unlist(strsplit(x, seperator,
    fixed = T
  )))
  boolean_nested_list <- lapply(split, function(x) {
    create_nested_lest(
      longer_list = all_potential_value,
      shorter_list = x
    )
  })
  df_to_return <- tibble::as_tibble(do.call(rbind, boolean_nested_list), check.names = F)

  return(df_to_return)
}

create_nested_lest <- function(longer_list, shorter_list) {
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
merge_original_and_other_units <- function(data, main_column, other_column, loop_structure) {
  if (loop_structure == T) {
    number_of_loops <- find_number_of_loops(data, main_column)
    new_column_names <- paste0(main_column, "_", c(1:number_of_loops))
    new_columns <- sapply(
      c(1:number_of_loops),
      function(x) merge_original_and_other_unit_single_pair(data[[paste0(main_column, "_", x)]], data[[paste0(other_column, "_", x)]])
    )
    colnames(new_columns) <- new_column_names
    new_columns <- tibble::as_tibble(new_columns)
    return(new_columns)
  }
  if (loop_structure == F) {
    new_values <- merge_original_and_other_unit_single_pair(data[[main_column]], data[[other_column]])
    new_column <- tibble::as_tibble(list(x = new_values))
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
merge_original_and_other_unit_single_pair <- function(original_column, other_column) {
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
add_column_after_specific_column <- function(data, new_data, new_column_name = NULL, old_column_name, loop_structure) {
  if (loop_structure == T) {
    if (is.null(new_column_name)) {
      stop("When adding new columns to a loop structure, must specify the base name for the new columns")
    }
    number_of_loops_old <- find_number_of_loops(data, name_column = old_column_name)
    number_of_loops_new <- find_number_of_loops(new_data, name_column = new_column_name)
    if (number_of_loops_old != number_of_loops_new) {
      stop("The datasets you are merging do not have the same number loops")
    }

    for (loop in 1:number_of_loops_old) {
      new_column_to_add <- new_data[paste0(new_column_name, "_", loop)]

      if (new_column_to_add %in% colnames(data)){
        data[new_column_to_add] <- NULL
      }
      data <- tibble::add_column(.data = data, new_column_to_add, .after = paste0(old_column_name, "_", loop))
    }

    return(data)
  }

  if (loop_structure == F) {
    if (any(colnames(new_data) %in% colnames(data))){
        data[colnames(data) %in% colnames(new_data)] <- NULL
      }
    data <- tibble::add_column(.data = data, new_data, .after = old_column_name)
    return(data)
  }
}


#' Switch Column Names and Add Categories For a Project
#'
#' A function for changing the column names
#' e.g, where the column names represent crops,
#' and merging categories.
#'
#' @param data Dataset with categories to be merged
#' @param conversion_tibble A tibble of conversion factors
#' @param id_rhomis_dataset The id of the rhomis dataset to
#' be converted
#'
#' @return
#' @export
#'
#' @example
switch_column_names_and_add_categories_for_specific_project <- function(data,
                                                                        conversion_tibble,
                                                                        id_rhomis_dataset) {
  if (c("id_rhomis_dataset") %in% colnames(data) == F) {
    stop("Cannot merge these data, id_rhomis_dataset not present")
  }

  if (any(c("id_rhomis_dataset", "survey_value", "conversion") %in% colnames(conversion_tibble) == F)) {
    stop("Cannot merge these data, conversion tibble does not contain the correct column names.
    Must contain columns: id_rhomis_dataset, survey_value, and conversion")
  }
  # Create A new blank dataset

  project_data <- data[data$id_rhomis_dataset == id_rhomis_dataset, ]
  project_conversion_tibble <- conversion_tibble[conversion_tibble$id_rhomis_dataset == id_rhomis_dataset, ]
  project_conversion_tibble <- project_conversion_tibble[project_conversion_tibble$survey_value %in% colnames(project_data), ]

  if (any(is.na(project_conversion_tibble$conversion))) {
    categories_to_remove <- project_conversion_tibble$survey_value[is.na(project_conversion_tibble$conversion)]

    project_data <- project_data[colnames(project_data) %in% categories_to_remove == F]
    project_conversion_tibble <- project_conversion_tibble[!is.na(project_conversion_tibble$conversion), ]
  }
  # Loop through the converted values
  new_data_set <- sapply(unique(project_conversion_tibble$conversion), function(converted_value) {

    # Find the survey values which match
    columns_to_merge <- project_conversion_tibble$survey_value[project_conversion_tibble$conversion == converted_value]

    if (converted_value %in% columns_to_merge == F & converted_value %in% colnames(project_data)) {
      columns_to_merge <- c(columns_to_merge, converted_value)
    }

    subset_to_add <- project_data[columns_to_merge]
    result <- rowSums(subset_to_add, na.rm = T)

    result[rowSums(is.na(subset_to_add)) == ncol(subset_to_add)] <- NA
    return(result)
  }, simplify = F) %>% tibble::as_tibble()

  columns_to_replace <- which(colnames(project_data) %in% project_conversion_tibble$survey_value |
    colnames(project_data) %in% project_conversion_tibble$conversion)

  location_to_insert <- min(columns_to_replace)
  if (location_to_insert > 1) {
    location_to_insert <- location_to_insert - 1
  }

  project_data <- project_data[-columns_to_replace]
  project_data <- project_data %>% tibble::add_column(new_data_set, .after = location_to_insert)

  return(project_data)
}

#' Switch Column Names and Add Categories For a Project
#'
#' A function for changing the column names
#' e.g, where the column names represent crops,
#' and merging categories.
#'
#' @param data Dataset with categories to be merged
#' @param conversion_tibble A tibble of conversion factors
#' be converted
#' @param by_project Should
#'
#' @return
#' @export
#'
#' @example
switch_column_names_and_merge_categories <- function(data,
                                                     conversion_tibble,
                                                     by_project = T) {
  if ("id_rhomis_dataset" %in% colnames(data) == F) {
    stop("Cannot merge categories, missing column 'id_rhomis_dataset'")
  }

  if (any(is.na(data$id_rhomis_dataset))) {
    warning("Missing ids for rhomis dataset, projects with no ids will be removed")
  }

  # Making fake ids to work with if do not want to
  if (by_project == F) {
    remove_id <- F
    if ("id_rhomis_dataset" %in% colnames(conversion_tibble) == T) {
      stop("There are ids in the conversion tibble provided
      But the argument 'by_project' is False.")
    }
    if ("id_rhomis_dataset" %in% colnames(data) == F) {
      remove_id <- T
      data$id_rhomis_dataset <- "x"
    }

    conversion_tibble <- make_per_project_conversion_tibble(data$id_rhomis_dataset, conversion_tibble)
    conversion_tibble <- conversion_tibble[duplicated(conversion_tibble) == F, ]
  }

  project_ids <- unique(data$id_rhomis_dataset)

  result <- lapply(project_ids, function(project_id) {
    indexes <- which(data$id_rhomis_dataset == project_id)
    result <- switch_column_names_and_add_categories_for_specific_project(
      data = data,
      conversion_tibble = conversion_tibble,
      id_rhomis_dataset = project_id
    )
    result$merging_index <- indexes
    return(result)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(merging_index) %>%
    dplyr::select(-merging_index)



  columns_to_replace <- which(colnames(data) %in% conversion_tibble$survey_value | colnames(data) %in% conversion_tibble$conversion)
  columns_to_add <- which(colnames(result) %in% conversion_tibble$conversion)

  location_to_insert <- min(columns_to_replace)
  if (location_to_insert > 1) {
    location_to_insert <- location_to_insert - 1
  }

  data <- data[-columns_to_replace]
  data <- data %>% tibble::add_column(result[columns_to_add], .after = location_to_insert)


  if (by_project == F) {
    if (remove_id == T) {
      data$id_rhomis_dataset <- NULL
    }
  }
  return(data)
}


#' Apply Conversion Factor to Columns
#'
#' Sometimes it can be helpful to apply conversion
#' factors to a series of columns, for example to
#' get the calorie values of crops produced. This
#' function allows us to apply conversion factors
#' to an individual project.
#'
#' @param data A tibble to be converted
#' @param conversion_tibble The conversion tibble
#' @param id_rhomis_dataset The id of the project to
#' be converted
#'
#' @return
#' @export
#'
#' @example
apply_conversion_factor_to_columns <- function(data,
                                               conversion_tibble,
                                               id_rhomis_dataset) {
  if (c("id_rhomis_dataset") %in% colnames(data) == F) {
    stop("Cannot merge these data, id_rhomis_dataset not present")
  }

  if (any(c("id_rhomis_dataset", "survey_value", "conversion") %in% colnames(conversion_tibble) == F)) {
    stop("Cannot merge these data, conversion tibble does not contain the correct column names.
    Must contain columns: id_rhomis_dataset, survey_value, and conversion")
  }
  # Create A new blank dataset
  # Subset by project ID
  project_data <- data[data$id_rhomis_dataset == id_rhomis_dataset, ]
  project_conversion_tibble <- conversion_tibble[conversion_tibble$id_rhomis_dataset == id_rhomis_dataset, ]
  # Make sure the conversion factors exist in the dataset
  project_conversion_tibble <- project_conversion_tibble[project_conversion_tibble$survey_value %in% colnames(project_data), ]
  project_data <- project_data[colnames(project_data) %in% project_conversion_tibble$survey_value |
    colnames(project_data) == "id_rhomis_dataset" |
    colnames(project_data) %in% project_conversion_tibble$conversion]

  data_to_convert <- project_data[colnames(project_data) %in% project_conversion_tibble$survey_value |
    colnames(project_data) %in% project_conversion_tibble$conversion]


  data_to_convert <- data_to_convert %>%
    dplyr::mutate_all(as.numeric)

  project_conversion_tibble$conversion <- as.numeric(project_conversion_tibble$conversion)
  project_conversion_tibble <- project_conversion_tibble %>%
    dplyr::select(-id_rhomis_dataset) %>%
    tidyr::pivot_wider(names_from = survey_value, values_from = conversion)
  project_conversion_tibble <- project_conversion_tibble[colnames(data_to_convert)]

  data_to_convert <- sapply(colnames(data_to_convert), function(column_name) {
    zeroes <- data_to_convert[[column_name]] == 0
    converted_column <- as.numeric(unlist(data_to_convert[[column_name]])) * as.numeric(unlist(project_conversion_tibble[[column_name]][1]))
    converted_column[zeroes] <- 0
    return(converted_column)
  }) %>% tibble::as_tibble()

  project_data[colnames(data_to_convert)] <- data_to_convert

  return(project_data)
}

#' Apply Conversion Factor to Columns Multiple Projects
#'
#' Sometimes it can be helpful to apply conversion
#' factors to a series of columns, for example to
#' get the calorie values of crops produced. This
#' function allows us to apply conversion factors
#' to mulptiple projects.
#'
#'
#' @param data A tibble to be converted
#' @param conversion_tibble The conversion tibble
#' @param by_project Whether or not to apply
#' conversion factors by project
#' be converted
#'
#' @return
#' @export
#'
#' @example
apply_conversion_factor_to_columns_multiple_projects <- function(data, conversion_tibble, by_project = T) {
  if ("id_rhomis_dataset" %in% colnames(data) == F) {
    stop("Cannot merge categories, missing column 'id_rhomis_dataset'")
  }

  if (any(is.na(data$id_rhomis_dataset))) {
    warning("Missing ids for rhomis dataset, projects with no ids will be removed")
  }

  # Making fake ids to work with if do not want to
  if (by_project == F) {
    remove_id <- F
    if ("id_rhomis_dataset" %in% colnames(conversion_tibble) == T) {
      stop("There are ids in the conversion tibble provided
      But the argument 'by_project' is False.")
    }
    if ("id_rhomis_dataset" %in% colnames(data) == F) {
      remove_id <- T
      data$id_rhomis_dataset <- "x"
    }

    conversion_tibble <- make_per_project_conversion_tibble(data$id_rhomis_dataset, conversion_tibble)
    conversion_tibble <- conversion_tibble[duplicated(conversion_tibble) == F, ]
  }

  project_ids <- unique(data$id_rhomis_dataset)

  result <- lapply(project_ids, function(project_id) {
    indexes <- which(data$id_rhomis_dataset == project_id)
    result <- apply_conversion_factor_to_columns(
      data = data,
      conversion_tibble = conversion_tibble,
      id_rhomis_dataset = project_id
    )
    result$merging_index <- indexes
    return(result)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(merging_index) %>%
    dplyr::select(-merging_index)



  if (by_project == F) {
    if (remove_id == T) {
      data$id_rhomis_dataset <- NULL
    }
  }
  return(result)
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
proportions_calculation <- function(data, use, use_column, prop_column, loop_number = NULL) {
  if (use != "sell" & use != "eat" & use != "feed_livestock" & use != "use") {
    stop("Invalid 'use' defined for crop proportions")
  }

  if (!is.null(loop_number)) {
    use_data <- data[[paste0(use_column, "_", loop_number)]]
    proportions_data <- data[[paste0(prop_column, "_", loop_number)]]
  }



  if (is.null(loop_number)) {
    use_data <- data[[use_column]]
    proportions_data <- data[[prop_column]]
  }

  if (all(is.na(use_data)) == F) {
    single_uses <- strsplit(as.character(use_data), " ")
  }
  if (all(is.na(use_data)) == T) {
    single_uses <- use_data
  }

  single_uses <- sapply(single_uses, function(x) length(x))
  single_uses <- single_uses == 1 & !is.na(use_data) & grepl(use, use_data)

  other_uses <- !is.na(use_data) & grepl(use, use_data) == F


  id_col <- rep("x", nrow(data))
  unit_conv_tibble <- make_per_project_conversion_tibble(proj_id_vector = id_col, unit_conv_tibble = proportion_conversions)
  proportions_data <- switch_units(proportions_data, unit_tibble = unit_conv_tibble, id_vector = id_col)
  proportions_data[single_uses] <- 1
  proportions_data[other_uses] <- 0

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
collapse_list_of_tibbles <- function(list_of_tibbles) {
  new_data <- lapply(names(list_of_tibbles), FUN = function(x) {
    temp_data <- list_of_tibbles[[x]]
    colnames(temp_data) <- paste0(colnames(temp_data), "_", x)
    return(temp_data)
  })

  new_data <- dplyr::bind_cols(new_data)

  return(new_data)
}