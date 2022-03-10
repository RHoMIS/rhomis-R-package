


#' Land Size Calculation
#'
#' Calculate land sizes in hectares using
#' RHoMIS data
#'
#' @param data RHoMIS data
#' @param unit_conv_tibble A tibble of land area units and
#'
#' @return
#' @export
#'
#' @examples
land_size_calculation <- function(data,
                                  unit_conv_tibble=NULL){

    if ("id_rhomis_dataset"%in% colnames(data)==F){
        stop("Missing the id_rhomis_dataset column in RHoMIS data")
    }

    if (is.null(unit_conv_tibble)){
        unit_conv_tibble <- make_per_project_conversion_tibble(
            proj_id_vector = data[["id_rhomis_dataset"]],
            unit_conv_tibble = land_area_units
        )
    }


    converted_units <- switch_units(data["unitland"],
                                    unit_tibble = unit_conv_tibble,
                                    id_vector = data[["id_rhomis_dataset"]])

    data[c("landcultivated", "landowned")] <- data[c("landcultivated", "landowned")] %>% dplyr::mutate_all(as.numeric)

    land_cultivated <- data["landcultivated"]*converted_units
    land_owned <- data["landowned"]*converted_units

    land_df <- tibble::as_tibble(list(land_cultivated=land_cultivated[[1]],
                                      land_owned=land_owned[[1]]))


    return(land_df)

}
