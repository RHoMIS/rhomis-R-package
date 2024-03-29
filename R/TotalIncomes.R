

#' Total Livestock Income
#'
#' Calculate total livestock income from
#' processed RHoMIS data
#'
#' Rpackage file: TotalIncomes.R
#'
#' @param data RHoMIS data that has been processed using
#' the "livestock_calculations_all" function
#'
#' @return
#' @export
#'
#' @examples
total_livestock_income <- function(data) {

    # indicator_search_livestock_income_lcu_per_year

    number_of_loops <- find_number_of_loops(data, name_column = "livestock_name")

    whole_livestock_income_columns <- paste0("livestock_sale_income", "_", c(1:number_of_loops))
    meat_income_columns <- paste0("meat_sold_income", "_", c(1:number_of_loops))
    milk_income_columns <- paste0("milk_sold_income_per_year", "_", c(1:number_of_loops))
    eggs_income_columns <- paste0("eggs_income_per_year", "_", c(1:number_of_loops))
    honey_income_columns <- paste0("bees_honey_sold_income", "_", c(1:number_of_loops))

    if (all(whole_livestock_income_columns %in% colnames(data))) {
        whole_livestock_income_data <- data[whole_livestock_income_columns] %>% dplyr::mutate_all(as.numeric)
    } else {
        whole_livestock_income_data <- tibble::as_tibble(list("livestock_income" = rep(NA, nrow(data))))
    }

    if (all(meat_income_columns %in% colnames(data))) {
        meat_income_data <- data[meat_income_columns] %>% dplyr::mutate_all(as.numeric)
    } else {
        meat_income_data <- tibble::as_tibble(list("meat_income" = rep(NA, nrow(data))))
    }

    if (all(milk_income_columns %in% colnames(data))) {
        milk_income_data <- data[milk_income_columns] %>% dplyr::mutate_all(as.numeric)
    } else {
        milk_income_data <- tibble::as_tibble(list("milk_income" = rep(NA, nrow(data))))
    }

    if (all(eggs_income_columns %in% colnames(data))) {
        eggs_income_data <- data[eggs_income_columns] %>% dplyr::mutate_all(as.numeric)
    } else {
        eggs_income_data <- tibble::as_tibble(list("egg_income" = rep(NA, nrow(data))))
    }

    if (all(honey_income_columns %in% colnames(data))) {
        honey_income_data <- data[honey_income_columns] %>% dplyr::mutate_all(as.numeric)
    } else {
        honey_income_data <- tibble::as_tibble(list("honey_income" = rep(NA, nrow(data))))
    }


    whole_livestock_income <- rowSums(whole_livestock_income_data, na.rm = T)
    meat_income <- rowSums(meat_income_data, na.rm = T)
    milk_income <- rowSums(milk_income_data, na.rm = T)
    eggs_income <- rowSums(eggs_income_data, na.rm = T)
    honey_income <- rowSums(honey_income_data, na.rm = T)

    whole_livestock_income[rowSums(!is.na(whole_livestock_income_data)) == 0] <- NA
    meat_income[rowSums(!is.na(meat_income_data)) == 0] <- NA
    milk_income[rowSums(!is.na(milk_income_data)) == 0] <- NA
    eggs_income[rowSums(!is.na(eggs_income_data)) == 0] <- NA
    honey_income[rowSums(!is.na(honey_income_data)) == 0] <- NA

    total_livestock_product_income_data <- tibble::as_tibble(list(
        meat_income = meat_income,
        milk_income = milk_income,
        eggs_income = eggs_income,
        honey_income = honey_income
    ))

    total_livestock_product_income <- rowSums(total_livestock_product_income_data, na.rm = T)
    total_livestock_product_income[rowSums(!is.na(total_livestock_product_income_data)) == 0] <- NA

    total_livestock_income_data <- tibble::as_tibble(list(
        whole_livestock_income = whole_livestock_income,
        total_livestock_product_income = total_livestock_product_income
    ))

    total_livestock_income <- rowSums(total_livestock_income_data, na.rm = T)
    total_livestock_income[rowSums(!is.na(total_livestock_income_data)) == 0] <- NA

    return(total_livestock_income)
}

#' Total Crop Income
#'
#' Calculate the total crop income from processed
#' RHoMIS data
#'
#' Rpackage file: TotalIncomes.R
#'
#' @param data RHoMIS data that has been processed using
#' the "crop_calculations_all" function
#'
#' @return
#' @export
#'
#' @examples
total_crop_income <- function(data) {
    # indicator_search_crop_income_lcu_per_year
    number_of_loops <- find_number_of_loops(data, name_column = "crop_name")

    crop_income_columns <- paste0("crop_income_per_year", "_", c(1:number_of_loops))

    crop_amount_columns <- paste0("crop_yield", "_", c(1:number_of_loops))
    crop_yield_units_columns <- paste0("crop_yield_units", "_", c(1:number_of_loops))
    crop_sold_units_columns <- paste0("crop_sold_price_quantityunits", "_", c(1:number_of_loops))
    crop_use_columns <- paste0()



    crop_income_data <- data[crop_income_columns]

    # na_rows <- rowSums(is.na(data[crop_amount_columns])) != number_of_loops &
    #   (
    #     rowSums(is.na(data[crop_yield_units_columns])) == number_of_loops |
    #       rowSums(is.na(data[crop_sold_units_columns])) == number_of_loops
    #   )
    na_rows <- rowSums(is.na(crop_income_data))==ncol(crop_income_data)


    crop_income_total <- rowSums(crop_income_data, na.rm = T)
    crop_income_total[na_rows] <- NA

    return(crop_income_total)
}


#' Calculating off-farm income
#'
#' Calculating off-farm income based on total income proportions
#'
#' Rpackage file: TotalIncomes.R
#'
#' @param data RHoMIS data, including column of offfarm income
#' @param total_crop_income Total income from crops list
#' @param total_livestock_income Total income from livestock list
#' proportions
#' @return
#' @export
#'
#' @examples
total_and_off_farm_incomes <- function(data, total_crop_income, total_livestock_income) {
    total_farm_income <- tibble::as_tibble(list(
        total_crop_income = total_crop_income,
        total_livestock_income = total_livestock_income
    )) %>%
        rowSums(na.rm = T)

    total_farm_income[is.na(total_crop_income) & is.na(total_livestock_income)] <- NA


    unit_conv_tibble <- proportion_conversions
    unit_conv_tibble$id_rhomis_dataset <- "x"

    id_vector <- rep("x", nrow(data))

    # indicator_search_off_farm_income_lcu_per_year

    off_farm_prop <- data["offfarm_income_proportion"]
    off_farm_incomes_any <- data["offfarm_incomes_any"]
    off_farm_prop <- switch_units(off_farm_prop, unit_tibble = unit_conv_tibble, id_vector = id_vector)

    off_farm_prop[off_farm_incomes_any=="n"] <- 0
    # DERIVING OFF-FARM INCOME
    # total_income = total_crop_income + total_livestock_income + off_farm_income
    # off_farm_income = prop_off_farm*total_income
    # off_farm_income = prop_off_farm(total_farm_income + off_farm_income)
    # off_farm_income = prop_off_farm(total_farm_income + off_farm_income)
    # off_farm_income = prop_off_farm(total_farm_income) +prop_off_farm*off_farm_income
    # off_farm_income - prop_off_farm*off_farm_income = prop_off_farm(total_farm_income)
    # (1-prop_off_farm)*off_farm_income = prop_off_farm(total_farm_income)
    # off_farm_income = prop_off_farm(total_farm_income)/(1-prop_off_farm)

    off_farm_income <- (off_farm_prop[[1]] * (total_farm_income)) / (1 - off_farm_prop[[1]])

    off_farm_income[off_farm_prop==0] <- 0

    # indicator_search_total_income_lcu_per_year
    total_income <- tibble::as_tibble(list(
        total_farm_income = total_farm_income,
        off_farm_income = off_farm_income
    ))


    total_income <- rowSums(total_income, na.rm = T)
    total_income[is.na(total_farm_income) & is.na(off_farm_income)] <- NA

    incomes_to_return <- tibble::as_tibble(list(
        off_farm_income = off_farm_income,
        total_income = total_income
    ))

    return(incomes_to_return)
}

#' Gendered Off-Farm Incomes Indicator
#'
#' Rpackage file: TotalIncomes.R
#'
#' @param data RHoMIS data including off-farm income loops
#' @param gender_categories The gender categories included in the survey
#' @return
#' @export
#'
#' @examples
gendered_off_farm_income_indicator <- function(data, gender_categories = pkg.env$gender_categories) {
    number_of_loops <- find_number_of_loops(data, "offfarm_income_name")

    offfarm_name_columns <- paste0("offfarm_income_name", "_", c(1:number_of_loops))
    gender_control_columns <- paste0("offfarm_who_control_revenue", "_", c(1:number_of_loops))

    # Finding out how many off-farm activities they engage in
    off_farm_income_data <- tibble::as_tibble(!is.na(data[offfarm_name_columns])) %>% dplyr::mutate_all(as.numeric)
    off_farm_income_data[off_farm_income_data == 0] <- NA
    off_farm_income_data <- tibble::as_tibble(off_farm_income_data / rowSums(off_farm_income_data, na.rm = T))
    colnames(off_farm_income_data) <- paste0("off_farm_source_prop", "_", c(1:number_of_loops))

    gender_control_data <- data[gender_control_columns]
    gender_control_data <- tibble::as_tibble(cbind(off_farm_income_data, gender_control_data))
    gender_control_data <- insert_gender_columns_in_core_data(
        data = gender_control_data,
        original_column = "off_farm_source_prop",
        control_column = "offfarm_who_control_revenue",
        loop_structure = T, gender_control_categories = gender_categories
    )

    male_youth_control <- gender_control_data[grep("^male_youth*", colnames(gender_control_data))]
    male_youth_control_total <- rowSums(male_youth_control, na.rm = T)
    male_youth_control_total[rowSums(!is.na(male_youth_control)) == 0] <- NA

    female_youth_control <- gender_control_data[grep("^female_youth*", colnames(gender_control_data))]
    female_youth_control_total <- rowSums(female_youth_control, na.rm = T)
    female_youth_control_total[rowSums(!is.na(female_youth_control)) == 0] <- NA

    male_adult_control <- gender_control_data[grep("^male_adult*", colnames(gender_control_data))]
    male_adult_total <- rowSums(male_adult_control, na.rm = T)
    male_adult_total[rowSums(!is.na(male_adult_control)) == 0] <- NA

    female_adult_control <- gender_control_data[grep("^female_adult*", colnames(gender_control_data))]
    female_adult_total <- rowSums(female_adult_control, na.rm = T)
    female_adult_total[rowSums(!is.na(female_adult_control)) == 0] <- NA

    off_farm_gender_control_totals <- tibble::as_tibble(list(
        female_youth_off_farm_control_total = female_youth_control_total,
        male_youth_off_farm_control_total = male_youth_control_total,
        female_adult_off_farm_control_total = female_adult_total,
        male_adult_off_farm_control_total = male_adult_total
    ))

    return(off_farm_gender_control_totals)
}

#' Gendered Off-Farm Income Split
#'
#' Adding gender control columns for each of the specific
#' sources of off-farm income
#'
#' Rpackage file: TotalIncomes.R
#'
#' @param data RHoMIS data including off-farm income loops
#' @param gender_categories Categories of gender we are interested in
#'
#' @return
#' @export
#'
#' @examples
gendered_off_farm_income_split <- function(data, gender_categories) {
    number_of_loops <- find_number_of_loops(data, "offfarm_income_name")
    if (number_of_loops>0){
        offfarm_name_columns <- paste0("offfarm_income_name", "_", c(1:number_of_loops))
        gender_control_columns <- paste0("offfarm_who_control_revenue", "_", c(1:number_of_loops))

        # Finding out how many off-farm activities they engage in
        missing_columns <- check_columns_in_data(data,
                                                 loop_columns = c("offfarm_income_name", "offfarm_who_control_revenue"),
                                                 warning_message = "Could not calculate off-farm income income"
        )
        if (length(missing_columns) == 0) {
            data[gender_control_columns] <- data[gender_control_columns] %>% dplyr::mutate_all(as.character)
            off_farm_income_data <- tibble::as_tibble(!is.na(data[offfarm_name_columns])) %>% dplyr::mutate_all(as.numeric)
            off_farm_income_data[off_farm_income_data == 0] <- NA
            off_farm_income_data_columns <- paste0("off_farm_source_prop", "_", c(1:number_of_loops))
            colnames(off_farm_income_data) <- off_farm_income_data_columns

            gender_control_data <- data[gender_control_columns]
            gender_control_data <- tibble::as_tibble(cbind(off_farm_income_data, gender_control_data))
            gender_control_data <- insert_gender_columns_in_core_data(
                data = gender_control_data,
                original_column = "off_farm_source_prop",
                control_column = "offfarm_who_control_revenue",
                loop_structure = T,
                gender_control_categories = gender_categories
            )
            columns_to_exclude <- unlist(c(offfarm_name_columns, gender_control_columns, off_farm_income_data_columns))
            gender_control_data <- gender_control_data[colnames(gender_control_data) %in% columns_to_exclude == F]

            # gender_control_data <- gender_control_data[grepl("male",colnames(gender_control_data))]
            for (gender_cat in gender_categories) {
                data <- add_column_after_specific_column(data,
                                                         new_data = gender_control_data,
                                                         new_column_name = paste0(gender_cat, "_off_farm_source_prop"),
                                                         old_column_name = "offfarm_who_control_revenue",
                                                         loop_structure = T
                )
            }
        }
    }

    return(data)
}
