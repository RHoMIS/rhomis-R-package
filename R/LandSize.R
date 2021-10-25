library(tibble)


#' Land Size Calculation
#'
#' Calculate land sizes in hectares using
#' RHoMIS data
#'
#' @param data RHoMIS data
#' @param units Land size units
#' @param unit_conversions Unit conversions for the land size units
#'
#' @return
#' @export
#'
#' @examples
land_size_calculation <- function(data,
                                  units=land_area_units$units,
                                  unit_conversions=land_area_units$conversions){


    converted_units <- switch_units(data["unitland"],
                                    units = units,
                                    conversion_factors = unit_conversions)

    data[c("landcultivated", "landowned")] <- data[c("landcultivated", "landowned")] %>% dplyr::mutate_all(as.numeric)

    land_cultivated <- data["landcultivated"]*converted_units
    land_owned <- data["landowned"]*converted_units

    land_df <- tibble::as_tibble(list(land_cultivated=land_cultivated[[1]],
                                      land_owned=land_owned[[1]]))


    return(land_df)

}
