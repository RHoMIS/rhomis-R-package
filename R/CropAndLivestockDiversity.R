
#' Crop Diversity
#'
#' Calculating the number of crops kept by a household
#'
#' Rpackage file: CropAndLivestockDiversity.R
#'
#' @param data RHoMIS dataset
#' @param indicator_data A tibble to store indicators, if crop diversity can be calculated, then the indicator will be appended.
#'
#' @return
#' @export
#'
#' @examples
crop_diversity <- function(data, indicator_data) {
  missing_columns <- check_columns_in_data(data, individual_columns = "crops_all")

  if (length_missing_columns == 0) {
    crop_diversity_df <- split_string_categories_to_dummy(data$crops_all, seperator = " ")
    indicator_data$crop_diversity <- rowSums(crop_diversity_df, na.rm = T)
    return(indicator_data)
  }

  if (length_missing_columns > 0) {
    warning("Cannot calculate crop diversity, missing the crops_all column")
    return(indicator_data)
  }
}


#' Livestock Diversity
#'
#' Calculating the number of livestock kept by a household
#'
#' Rpackage file: CropAndLivestockDiversity.R
#'
#' @param data RHoMIS dataset
#' @param indicator_data A tibble to store indicators, if crop diversity can be calculated, then the indicator will be appended.
#'
#' @return
#' @export
#'
#' @examples
livestock_diversity <- function(data, indicator_data) {
  missing_columns <- check_columns_in_data(data, individual_columns = "livestock_all")

  if (length_missing_columns == 0) {
    livestock_diversity_df <- split_string_categories_to_dummy(data$livestock_all, seperator = " ")
    indicator_data$livestock_data <- rowSums(livestock_diversity_df, na.rm = T)
    return(indicator_data)
  }

  if (length_missing_columns > 0) {
    warning("Cannot calculate crop diversity, missing the crops_all column")
    return(indicator_data)
  }
}