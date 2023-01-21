


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

    # indicator_search_land_cultivated_ha
    # indicator_search_land_owned_ha

    if ("id_rhomis_dataset" %in% colnames(data) == F) {
        stop("Missing the id_rhomis_dataset column in RHoMIS data")
    }

    if (is.null(unit_conv_tibble)) {
        unit_conv_tibble <- make_per_project_conversion_tibble(
            proj_id_vector = data[["id_rhomis_dataset"]],
            unit_conv_tibble = land_area_to_ha
        )
    }

    land_df <- tibble::as_tibble(list(
        land_cultivated_ha = rep(NA, nrow(data)),
        land_owned_ha = rep(NA, nrow(data))
    ))

    missing_unit_land_cultivated <- check_columns_in_data(data, individual_columns = "unitland")
    if (length(missing_unit_land_cultivated) == 0) {

        if ("areaunits_other" %in% colnames(data)){
            data[["unitland"]] <- as.character(data[["unitland"]])
            data[["areaunits_other"]] <- as.character(data[["areaunits_other"]])

            other_index <- data[["unitland"]]=="other" & !is.na(data[["unitland"]])
            data[other_index,"unitland"] <- data[other_index,"areaunits_other"]
        }

        converted_cultivated_units <- switch_units(data["unitland"],
                                                   unit_tibble = unit_conv_tibble,
                                                   id_vector = data[["id_rhomis_dataset"]]
        )%>% dplyr::mutate_all(as.numeric)
    }

    missing_unit_land_owned <- check_columns_in_data(data, individual_columns = "unitland_owned")
    if (length(missing_unit_land_owned) == 0) {
        if ("areaunits_other_own" %in% colnames(data)){
            data[["unitland_owned"]] <- as.character(data[["unitland_owned"]])
            data[["areaunits_other_own"]] <- as.character(data[["areaunits_other_own"]])
            other_index <- data[["unitland_owned"]]=="other" & !is.na(data[["unitland_owned"]])
            data[other_index,"unitland_owned"] <- data[other_index,"areaunits_other_own"]
        }

        converted_owned_units <- switch_units(data["unitland_owned"],
                                              unit_tibble = unit_conv_tibble,
                                              id_vector = data[["id_rhomis_dataset"]]
        ) %>% dplyr::mutate_all(as.numeric)
    }
    if(length(missing_unit_land_owned)==1 & length(missing_unit_land_cultivated)==0){
        converted_owned_units <- converted_cultivated_units
    }





    missing_land_cultivated <- check_columns_in_data(data, individual_columns = c("landcultivated","unitland"))
    if (length(missing_land_cultivated) == 0) {
        data[c("landcultivated")] <- data[c("landcultivated")] %>% dplyr::mutate_all(as.numeric)
        land_cultivated <- data["landcultivated"] * converted_cultivated_units
        land_df$land_cultivated_ha <- land_cultivated[[1]]
    }

    missing_land_owned <- check_columns_in_data(data, individual_columns = "landowned")
    if (length(missing_land_owned) == 0) {
        if(exists("converted_owned_units")){
            data[c("landowned")] <- data[c("landowned")] %>% dplyr::mutate_all(as.numeric)
            land_owned <- data["landowned"] * converted_owned_units
            land_df$land_owned_ha <- land_owned[[1]]
        }

    }



    return(land_df)
}
