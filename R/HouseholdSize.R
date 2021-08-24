library(tibble)


get_household_size_conversion <- function(){
    MAE_coeff <- tibble::as_tibble(list(children_under_4=0.5,
                                children_4to10=0.75,
                                males11to24=0.85,
                                females11to24=0.75,
                                males25to50=1,
                                female_25_to_50=0.86,
                                male_50_plus=0.73,
                                female_50_plus=0.6))
    return(MAE_coeff)
}




#' Identify Person Category
#'
#' For the household roster, categories
#' each person based on the MAE conversion information
#'
#' @param age The age vector for the specific loop
#' @param gender The gender vector for the specific loop
#'
#' @return
#' @export
#'
#' @examples
identify_person_category<- function(age, gender){

    vector <- rep(NA, length(gender))
    gender <- tolower(gender)

    vector[age<=4] <-"children_under_4"
    vector[age>4 & age<=10] <-"children_4to10"
    vector[age>11 & age<=24 & gender=="m"] <-"males11to24"
    vector[age>11 & age<=24 & gender=="f"] <-"females11to24"
    vector[age>24 & age<=50 & gender=="m"] <-"males25to50"
    vector[age>24 & age<=50 & gender=="f"] <-"female_25_to_50"
    vector[age>=50& gender=="m"] <-"male_50_plus"
    vector[age>=50 & gender=="f"] <-"female_50_plus"

    return(vector)
}

#' Household roster to catgeries
#'
#' @param data The dataset containing the household roster information
#'
#' @return A data-frame with the correct household
#' @export
#'
#' @examples
household_roster_to_categories <- function(data){

    number_of_loops <- find_number_of_loops(data,name_column = "hh_pop_rep_num")
    all_loops <- c(1:number_of_loops)
    column_names <- paste0("household_person_category_",all_loops)

    age_categories <- sapply(all_loops, function(x) identify_person_category(age=data[[paste0("person_age_",x)]], gender = data[[paste0("person_gender_",x)]]),simplify=F)
    names(age_categories)<-column_names

    age_categories <-tibble::as_tibble(age_categories)




    return(age_categories)
}


#' Household Roster to Wide
#'
#' Moving household roster size to wide
#'
#' @param data RHoMIS data with household Roster information
#'
#' @return
#' @export
#'
#' @examples
household_roster_to_wide <- function(data){

    categorical_format <- household_roster_to_categories(data)
    categories <- colnames(get_household_size_conversion())

    categorical_format <- sapply(categories, function(x) rowSums(categorical_format==x,na.rm = T))
    categorical_format <- tibble::as_tibble(categorical_format)
    return(categorical_format)

}

#' Calculate MAE
#'
#' A function to calculate the male adult equivalent of household size
#'
#' @param data The data containing all rhomis information
#'
#' @return
#' @export
#'
#' @examples
calculate_MAE <- function(data){


    if (find_number_of_loops(data,name_column = "hh_pop_rep_num")>1)
    {
        data <- household_roster_to_wide(data)
    }

    conversion_factors <- get_household_size_conversion()

    MAE_frame <- sapply(names(conversion_factors), function(x) as.numeric(conversion_factors[1,x])*data[x])
    MAE_frame <- tibble::as_tibble(MAE_frame)
    MAE_frame <- rowSums(MAE_frame,na.rm=T)

    return(MAE_frame)

}


#' Calculate Household Size Members
#'
#' Calculate Household Size based on roster
#' questions
#'
#' @param data RHoMIS data with household roster
#'
#' @return
#' @export
#'
#' @examples
calculate_household_size_members <- function(data){
    if (find_number_of_loops(data,name_column = "hh_pop_rep_num")>1)
    {
        data <- household_roster_to_wide(data)
    }

    household_size <- rowSums(data,na.rm=T)

    return(household_size)


}
