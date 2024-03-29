


#' Get household Size Conversions
#'
#'
#' Rpackage file: HouseholdSize.R
#'
#' @return
#' @export
#'
#' @examples
get_household_size_conversion <- function() {
    MAE_coeff <- tibble::as_tibble(list(
      children_under4 = 0.5, #current
      children_under_4 = 0.5,
      children_4to10 = 0.75, #current
      males11to24 = 0.85,
      males_11to24 = 0.85, #current
      females11to24 = 0.75,
      females_11to24 = 0.75, #current
      males11to50=0.925,
      females11to50=0.805,
      males25to50 = 1,
      males_25to50 = 1, #current
      females25to50 = 0.86,
      female_25_to_50 = 0.86,
      females_25to50 = 0.86, #current
      male_50_plus = 0.73,
      malesover50 = 0.73,#current
      males_50plus = 0.73,
      female_50_plus = 0.6,
      femalesover50 = 0.6,#current
      females_50plus=0.6
    ))
    return(MAE_coeff)
}




#' Identify Person Category
#'
#' For the household roster, categories
#' each person based on the MAE conversion information
#'
#' Rpackage file: HouseholdSize.R
#'
#' @param age The age vector for the specific loop
#' @param gender The gender vector for the specific loop
#'
#' @return
#' @export
#'
#' @examples
identify_person_category <- function(age, gender) {
    vector <- rep(NA, length(gender))
    gender <- substr(tolower(gender),0,1)

    vector[age < 4] <- "children_under_4"
    vector[age >= 4 & age <= 10] <- "children_4to10"
    vector[age >= 11 & age <= 24 & gender == "m"] <- "males11to24"
    vector[age >= 11 & age <= 24 & gender == "f"] <- "females11to24"
    vector[age > 24 & age <= 50 & gender == "m"] <- "males25to50"
    vector[age > 24 & age <= 50 & gender == "f"] <- "females25to50"
    vector[age > 50 & gender == "m"] <- "malesover50"
    vector[age > 50 & gender == "f"] <- "femalesover50"

    return(vector)
}

#' Household roster to categories
#'
#' Rpackage file: HouseholdSize.R
#'
#' @param data The dataset containing the household roster information
#'
#' @return A data-frame with the correct household
#' @export
#'
#' @examples
household_roster_to_categories <- function(data) {
    number_of_loops <- find_number_of_loops(data, name_column = "hh_pop_rep_num")
    all_loops <- c(1:number_of_loops)
    column_names <- paste0("household_person_category_", all_loops)

    age_categories <- sapply(all_loops, function(x) identify_person_category(age = data[[paste0("person_age_", x)]], gender = data[[paste0("person_gender_", x)]]), simplify = F)
    names(age_categories) <- column_names

    age_categories <- tibble::as_tibble(age_categories)




    return(age_categories)
}


#' Household Roster to Wide
#'
#' Moving household roster size to wide
#'
#' Rpackage file: HouseholdSize.R
#'
#' @param data RHoMIS data with household Roster information
#'
#' @return
#' @export
#'
#' @examples
household_roster_to_wide <- function(data) {
    categorical_format <- household_roster_to_categories(data)
    # getting the categories used in identify_person_category()
    categories <- c("children_under_4",
                       "children_4to10",
                       "males11to24",
                       "females11to24",
                       "males25to50",
                       "females25to50",
                       "malesover50",
                       "femalesover50")
    # categories <- unique(identify_person_category(rep(1:100,2), rep(c("F", "M"), each=100)))
    categorical_format <- sapply(categories, function(x) rowSums(categorical_format == x, na.rm = T))
    categorical_format <- tibble::as_tibble(categorical_format)
    return(categorical_format)
}



#' Calculate MAE
#'
#' A function to calculate the male adult equivalent of household size
#'
#' Rpackage file: HouseholdSize.R
#'
#' @param data The data containing all rhomis information
#'
#' @return
#' @export
#'
#' @examples
calculate_MAE <- function(data) {

    conversion_factors <- get_household_size_conversion()



    if (find_number_of_loops(data, name_column = "hh_pop_rep_num") > 1) {
        data_hh_loop <- household_roster_to_wide(data)
        MAE_frame_loop <- sapply(names(conversion_factors), function(x) {
            if (x %in% colnames(data_hh_loop)) {
                return(as.numeric(conversion_factors[1, x]) * data_hh_loop[x])
            } else {
                return(rep(0, nrow(data_hh_loop)))
            }
        }, simplify = F) %>% dplyr::bind_cols()
        MAE_frame_loop <- tibble::as_tibble(MAE_frame_loop)
        MAE_loop <- rowSums(MAE_frame_loop, na.rm = T)
    }


    if (any(colnames(data) %in% names(conversion_factors))){

        columns_to_harmonise <- c("children_under4",

                                  "males_11to24",
                                  "females_11to24",

                                  "males11to50",
                                  "females11to50",


                                  "males_25to50",

                                  "females_25to50",
                                  "female_25_to_50",

                                  "male_50_plus",
                                  "males_50plus",

                                  "female_50_plus",
                                  "female_50plus")

        standard_cols <- c("children_under_4",
                             "children_4to10",
                             "males11to24",
                             "females11to24",
                             "males25to50",
                             "females25to50",
                             "malesover50",
                             "femalesover50")

        if (any(columns_to_harmonise %in% colnames(data))){
            columns_warn <- columns_to_harmonise[columns_to_harmonise %in% colnames(data)]

            temp_message <- paste0("The following columns were detected in your dataset:\n",
                   paste0(columns_warn,collapse="\n"),
                   "\nRHoMIS standard groups are named as follows:\n",
                   paste0(standard_cols,collapse="\n"),
                   "\nPlease consider renaming these columns if this data is to be combined with other RHoMIS datasets")
            warning(temp_message)
        }


        MAE_frame_group <- sapply(names(conversion_factors), function(x) {
            if (x %in% colnames(data)) {
                return(as.numeric(conversion_factors[1, x]) * data[x])
            } else {
                return(rep(0, nrow(data)))
            }
        }, simplify = F) %>% dplyr::bind_cols()
        MAE_frame_group <- tibble::as_tibble(MAE_frame_group)
        MAE_group <- rowSums(MAE_frame_group, na.rm = T)
    }

    if (exists("MAE_loop") & !exists("MAE_group")){
        return(MAE_loop)

    }

    if (exists("MAE_group") & !exists("MAE_loop")){
        return(MAE_group)
    }

    if (exists("MAE_group") & exists("MAE_loop")){
        na_rows <- is.na(MAE_group) & is.na(MAE_loop)
        mae <- pmax(MAE_group, MAE_loop,na.rm = T)
        mae[na_rows] <- NA
        return(mae)


    }


}


#' Calculate Household Size Members
#'
#' Calculate Household Size based on roster
#' questions
#'
#' Rpackage file: HouseholdSize.R
#'
#' @param data RHoMIS data with household roster
#'
#' @return
#' @export
#'
#' @examples
calculate_household_size_members <- function(data) {
    # calculate_household_size_members
    # indicator_search_hh_size_members
    conversion_factors <- get_household_size_conversion()
    if (find_number_of_loops(data, name_column = "hh_pop_rep_num") > 1) {
        household_size_data_hh_loop <- household_roster_to_wide(data)
        household_size_hh_loop <- rowSums(household_size_data_hh_loop, na.rm = T)

    }
    if (any(colnames(data) %in% names(conversion_factors))) {

        columns_to_harmonise <- c("children_under4",

                                  "males_11to24",
                                  "females_11to24",

                                  "males11to50",
                                  "females11to50",


                                  "males_25to50",

                                  "females_25to50",
                                  "female_25_to_50",

                                  "male_50_plus",
                                  "males_50plus",

                                  "female_50_plus",
                                  "female_50plus")

        standard_cols <- c("children_under_4",
                           "children_4to10",
                           "males11to24",
                           "females11to24",
                           "males25to50",
                           "females25to50",
                           "malesover50",
                           "femalesover50")

        if (any(columns_to_harmonise %in% colnames(data))){
            columns_warn <- columns_to_harmonise[columns_to_harmonise %in% colnames(data)]

            temp_message <- paste0("The following columns were detected in your dataset:\n",
                                   paste0(columns_warn,collapse="\n"),
                                   "\nRHoMIS standard groups are named as follows:\n",
                                   paste0(standard_cols,collapse="\n"),
                                   "\nPlease consider renaming these columns if this data is to be combined with other RHoMIS datasets")
            warning(temp_message)
        }


        household_size_data_groups <- data[colnames(data) %in% names(conversion_factors)]
        household_size_groups <- rowSums(household_size_data_groups, na.rm = T)

    }

    if (exists("household_size_hh_loop") & !exists("household_size_groups")){
        return(household_size_hh_loop)

    }

    if (exists("household_size_groups") & !exists("household_size_hh_loop")){
        return(household_size_groups)
    }

    if (exists("household_size_groups") & exists("household_size_hh_loop")){
        na_rows <- is.na(household_size_groups) & is.na(household_size_hh_loop)
        hh_size_member <- pmax(household_size_groups, household_size_hh_loop,na.rm = T)
        hh_size_member[na_rows] <- NA
        return(hh_size_member)


    }




    return(household_size)
}
