
#' Widen Gender Columns
#'
#' Take gender categories and a processed dataset,
#' reformat the gender control variables into a more
#' useable format
#'
#' Rpackage file: GenderControlAll.R
#'
#' @param processed_data A processed dataset, containing the gender columns
#' @param gender_group Which gender group you're interested in
#' @param column_pattern The pattern of the column you are processing (e.g. for "male_youth_crop_consumed_kg",
#' the pattern would be "crop_consumed_kg"
#'
#' @return
#' @export
#'
#' @examples
widen_gender_columns <- function(processed_data,
                                 gender_group,
                                 column_pattern) {
  combined_column <- paste0(gender_group, "_", column_pattern)
  number_of_loops <- find_number_of_loops(data = processed_data, combined_column)

  if (grepl("crop", combined_column)) {
    name_column <- "crop_name"
  }

  if (grepl("off_farm", combined_column)) {
    name_column <- "offfarm_income_name"
  }

  if (grepl("livestock", combined_column) |
    grepl("meat", combined_column) |
    grepl("milk", combined_column) |
    grepl("eggs", combined_column) |
    grepl("bees", combined_column)) {
    name_column <- "livestock_name"
  }

  if (number_of_loops > 0) {
    column_names <- paste0(combined_column, "_", c(1:number_of_loops))
    if (all(column_names %in% colnames(processed_data))) {
      return(map_to_wide_format(processed_data, name_column = name_column, column_prefixes = combined_column, types = "num")[[1]])
    }
  } else {
    return()
  }
}

#' Gender Control Summary
#'
#' Generate a summary of all of the gender controlled
#' indicators
#'
#' Rpackage file: GenderControlAll.R
#'
#' @param processed_data A processed RHoMIS dataset
#' @param indicator_data An indicator dataset, where you would like to append the result
#' @param gender_categories A vector of the gender categories in the dataset.
#'
#' @return
#' @export
#'
#' @examples
gender_control_summary <- function(processed_data,
                                   indicator_data,
                                   gender_categories = pkg.env$gender_categories) {
  extra_outputs <- list()


  # indicator_search_male_youth_control
  # indicator_search_male_adult_control
  # indicator_search_female_youth_control
  # indicator_search_female_adult_control


  amount_consumed_prefixes <- c(
    "crop_consumed_kg_per_year",
    "meat_consumed_kg_per_year",
    "milk_consumed_litres_per_year",
    "eggs_consumed_kg_per_year",
    "bees_honey_consumed_kg_per_year"
  )


  amount_sold_prefixes <- c(
    "crop_sold_kg_per_year",
    "meat_sold_kg_per_year",
    "milk_sold_litres_per_year",
    "eggs_sold_kg_per_year",
    "bees_honey_sold_kg_per_year"
  )

  value_prefixes <- c(
    "value_crop_consumed_lcu",
    "value_meat_consumed_lcu",
    "value_milk_consumed_lcu",
    "value_eggs_consumed_lcu",
    "value_bees_honey_consumed_lcu"
  )

  income_prefixes <- c(
    "crop_income_per_year",
    "livestock_sale_income",
    "meat_sold_income",
    "milk_sold_income_per_year",
    "eggs_income_per_year",
    "bees_honey_sold_income"
  )




  amounts_consumed <- sapply(gender_categories, function(gender) {
    sapply(amount_consumed_prefixes, function(column_pattern) {
      widen_gender_columns(
        processed_data = processed_data,
        gender_group = gender,
        column_pattern = column_pattern
      )
    }, simplify = F)
  }, simplify = F)

  extra_outputs$amounts_consumed <- amounts_consumed

  amounts_sold <- sapply(gender_categories, function(gender) {
    sapply(amount_sold_prefixes, function(column_pattern) {
      widen_gender_columns(
        processed_data = processed_data,
        gender_group = gender,
        column_pattern = column_pattern
      )
    }, simplify = F)
  }, simplify = F)

  extra_outputs$amounts_sold <- amounts_sold


  value_consumed <- sapply(gender_categories, function(gender) {
    sapply(value_prefixes, function(column_pattern) {
      widen_gender_columns(
        processed_data = processed_data,
        gender_group = gender,
        column_pattern = column_pattern
      )
    }, simplify = F)
  }, simplify = F)
  extra_outputs$value_consumed <- value_consumed


  income <- sapply(gender_categories, function(gender) {
    sapply(income_prefixes, function(column_pattern) {
      widen_gender_columns(
        processed_data = processed_data,
        gender_group = gender,
        column_pattern = column_pattern
      )
    }, simplify = F)
  }, simplify = F)
  extra_outputs$income <- income


  total_gender_incomes <- list()
  income_by_gender <- sapply(gender_categories, function(gender) {
    sapply(income[[gender]], function(gender_control_df) {
      if (any(class(gender_control_df) %in% c("tbl_df", "tbl", "data.frame"))) {
        rowSums(gender_control_df, na.rm = T)
      }
    }, simplify = F) %>%
      dplyr::bind_cols() %>%
      rowSums(na.rm = T)
  }, simplify = F) %>% dplyr::bind_cols()
  total_gender_incomes$farm_income <- income_by_gender



  value_consumed_by_gender <- sapply(gender_categories, function(gender) {
    sapply(value_consumed[[gender]], function(gender_control_df) {
      if (any(class(gender_control_df) %in% c("tbl_df", "tbl", "data.frame"))) {
        rowSums(gender_control_df, na.rm = T)
      }
    }, simplify = F) %>%
      dplyr::bind_cols() %>%
      rowSums(na.rm = T)
  }, simplify = F) %>% dplyr::bind_cols()
  total_gender_incomes$value_consumed <- value_consumed_by_gender


  if ("off_farm_income_lcu_per_year" %in% colnames(indicator_data)) {
    off_farm_gender_dfs <- sapply(gender_categories, function(gender) {
      widen_gender_columns(
        processed_data = processed_data,
        gender_group = gender,
        column_pattern = "off_farm_source_prop"
      )
    }, simplify = F)
    extra_outputs$off_farm_control_props <- off_farm_gender_dfs


    off_farm_gender_total <- sapply(off_farm_gender_dfs, function(off_farm_df) {
      if (any(class(off_farm_df) %in% c("tbl_df", "tbl", "data.frame"))) {
        rowSums(off_farm_df, na.rm = T)
      }
    }, simplify = F) %>% dplyr::bind_cols()

    off_farm_gender_total <- off_farm_gender_total  %>% dplyr::mutate_all(as.numeric)

    total_off_farm_incomes <- rowSums(off_farm_gender_total, na.rm = T)
    subset <- total_off_farm_incomes > 0 & !is.na(total_off_farm_incomes)

    off_farm_gender_total[subset, ] <- off_farm_gender_total[subset, ] / total_off_farm_incomes[subset]
    indicator_data[["off_farm_income_lcu_per_year"]] <-  as.numeric(indicator_data[["off_farm_income_lcu_per_year"]])
    off_farm_gender_control <- off_farm_gender_total * indicator_data[["off_farm_income_lcu_per_year"]]
    total_gender_incomes$off_farm <- off_farm_gender_control
  }

  total_gender_value_controls <- sapply(gender_categories, function(gender) {
    sapply(total_gender_incomes, function(gender_income_df) {
        if (nrow(gender_income_df)==nrow(processed_data)){
      gender_income_df[[gender]]
        }else{
            rep(NA, nrow(processed_data))
        }
    }, simplify = F) %>%
      dplyr::bind_cols() %>%
      rowSums(na.rm = T)
  }, simplify = F) %>% dplyr::bind_cols()

  gender_control_scores <- (total_gender_value_controls / rowSums(total_gender_value_controls, na.rm = T)) %>% tibble::as_tibble()
  colnames(gender_control_scores) <- paste0("proportion_of_value_controlled_", colnames(gender_control_scores))

  if(nrow(gender_control_scores)!=0){

  indicator_data <- dplyr::bind_cols(indicator_data, gender_control_scores)
  }else{
      warning("Gender outputs have zero rows, indicating no gender outputs generated")
  }

  result <- list()
  result$indicator_data <- indicator_data
  result$processed_data <- processed_data
  result$gender_outputs <- extra_outputs

  return(result)
}



gender_value_control_all <- function(processed_data, off_farm_income) {
  extra_outputs <- list()

  results$processed_data <- processed_data
  results$gender_control_outputs <- extra_outputs

  return(results)
}


# write_gender_outputs_to_folder <- function(
#         list_of_dfs,
#         folder="gender_control",
# ){
#
# }
