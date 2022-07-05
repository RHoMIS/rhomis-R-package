


#' Land Size Calculation
#'
#' Calculate land sizes in hectares using
#' RHoMIS data
#'
#'   
#' Rpackage file: LandSize.R
#'
#' 
#' @param data RHoMIS data
#' @param unit_conv_tibble A tibble of land area units and
#'
#' @return
#' @export
#'
#' @examples
land_size_calculation <- function(data,
                                  unit_conv_tibble = NULL) {
  if ("id_rhomis_dataset" %in% colnames(data) == F) {
    stop("Missing the id_rhomis_dataset column in RHoMIS data")
  }

  if (is.null(unit_conv_tibble)) {
    unit_conv_tibble <- make_per_project_conversion_tibble(
      proj_id_vector = data[["id_rhomis_dataset"]],
      unit_conv_tibble = land_area_units
    )
  }

  land_df <- tibble::as_tibble(list(
    land_cultivated = rep(NA, nrow(data)),
    land_owned = rep(NA, nrow(data))
  ))

  missing_unit_land <- check_columns_in_data(data, individual_columns = "unitland")
  if (length(missing_unit_land) == 0) {
    converted_units <- switch_units(data["unitland"],
      unit_tibble = unit_conv_tibble,
      id_vector = data[["id_rhomis_dataset"]]
    )

    missing_land_cultivated <- check_columns_in_data(data, individual_columns = "landcultivated")
    if (length(missing_land_cultivated) == 0) {
      data[c("landcultivated")] <- data[c("landcultivated")] %>% dplyr::mutate_all(as.numeric)
      land_cultivated <- data["landcultivated"] * converted_units
      land_df$land_cultivated <- land_cultivated[[1]]
    }

    missing_land_owned <- check_columns_in_data(data, individual_columns = "landowned")
    if (length(missing_land_owned) == 0) {
      data[c("landowned")] <- data[c("landowned")] %>% dplyr::mutate_all(as.numeric)
      land_owned <- data["landowned"] * converted_units
      land_df$land_owned <- land_owned[[1]]
    }
  }




  return(land_df)
}
