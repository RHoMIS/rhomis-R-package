library(tibble)
library(dplyr)
library(purrr)
#unit_conversions <- crop_yield_units

#' Convert Crop Yield Units
#'
#' RHoMIS crop yields come in a character format. This function
#' converts these to numeric and adds the new numeric units in an
#' appropriate place for the dataset
#'
#' @param data The data containing the units to convert
#' @param unit_conversions A dataframe or tibble of unit conversions
#'
#' @return
#' @export
#'
#' @examples
convert_crop_yield_units <- function(data, unit_conversions=crop_yield_units){
    number_of_loops <- find_number_of_loops(data,name_column = "crop_name")
    columns_to_convert <- paste0("crop_yield_units","_",c(1:number_of_loops))
    new_column_names <- paste0("crop_yield_units_numeric","_",c(1:number_of_loops))
    numeric_crop_units <- switch_units(data[columns_to_convert], units =unit_conversions$unit, conversion_factors = unit_conversions$conversion)
    colnames(numeric_crop_units) <-new_column_names
    data <- add_column_after_specific_column(data=data,
                                             new_data=numeric_crop_units,
                                             new_column_name="crop_yield_units_numeric",
                                             old_column_name="crop_yield_units",
                                             loop_structure=T)
    return(data)
}

#' Crop yield single loop
#'
#' Calculate crop yield based off of a single loop
#'
#' @param data THe data containind crop yields, and numeric
#' crop yield units
#' @param loop_number which loop you are calculating crop yield for.
#'
#' @return
#' @export
#'
#' @examples
crop_harvest_single_loop<-function(data, loop_number){
    crop_yield <- data[[paste0("crop_yield","_",loop_number)]]*data[[paste0("crop_yield_units_numeric","_",loop_number)]]
    return(crop_yield)
}

#' Crop harvest calculations
#'
#' Calculating the amount of each crop harvested per year
#'
#' @param data RHoMIS data containing cropping information
#'
#' @return
#' @export
#'
#' @examples
crop_harvest_calculations <- function(data){
    number_of_loops <- find_number_of_loops(data,name_column = "crop_name")
    data <- convert_crop_yield_units(data)

    new_column_names <- paste0("crop_harvest_kg_per_year","_",1:number_of_loops)
    crop_harvest_per_year <- sapply(c(1:number_of_loops),function(x) crop_harvest_single_loop(data,x))
    colnames(crop_harvest_per_year) <-new_column_names
    crop_harvest_per_year <- tibble::as_tibble(crop_harvest_per_year)

    data <- add_column_after_specific_column(data=data,
                                             new_data=crop_harvest_per_year,
                                             new_column_name="crop_harvest_kg_per_year",
                                             old_column_name="crop_yield_units_numeric",
                                             loop_structure=T)

    return(data)

}


#' Crop Proportions for all
#'
#' A function for calculating the numeric proportions of crops which are sold or
#' consumed
#'
#' @param data A standard RHoMIS data set
#'
#' @return
#' @export
#'
#' @examples
crop_proportions_all <- function(data){

    number_of_loops <- find_number_of_loops(data, name_column = "crop_name")

    crop_consumed_proportions_numeric <- sapply(c(1:number_of_loops), function(x) proportions_calculation(data, use = "eat", use_column = "crop_use", prop_column = "crop_consumed_prop", loop_number = x))
    colnames(crop_consumed_proportions_numeric)<-paste0("crop_consumed_prop_numeric","_",c(1:number_of_loops))
    crop_consumed_proportions_numeric<-tibble::as_tibble(crop_consumed_proportions_numeric)
    data <- add_column_after_specific_column(data=data,
                                             new_data=crop_consumed_proportions_numeric,
                                             new_column_name="crop_consumed_prop_numeric",
                                             old_column_name="crop_consumed_prop",
                                             loop_structure=T)


    crop_sold_proportions_numeric <- sapply(c(1:number_of_loops), function(x) proportions_calculation(data, use = "sell", use_column = "crop_use", prop_column = "crop_sold_prop", loop_number = x))
    colnames(crop_sold_proportions_numeric) <- paste0("crop_sold_prop_numeric","_",c(1:number_of_loops))
    crop_sold_proportions_numeric<- tibble::as_tibble(crop_sold_proportions_numeric)
    data <- add_column_after_specific_column(data=data,
                                             new_data=crop_sold_proportions_numeric,
                                             new_column_name="crop_sold_prop_numeric",
                                             old_column_name="crop_sold_prop",
                                             loop_structure=T)

    return(data)
}



convert_crop_sold_units <- function(data, unit_conversions=crop_price_units){
    number_of_loops <- find_number_of_loops(data, name_column = "crop_name")
    crop_sold_units_column <- paste0("crop_sold_price_quantityunits","_",c(1:number_of_loops))

    crop_sold_units <- data[crop_sold_units_column]
    colnames(crop_sold_units)<-paste0("crop_sold_units_numeric","_", c(1:3))
    units_converted <- switch_units(crop_sold_units,
                                    units = unit_conversions$unit,
                                    conversion_factors = unit_conversions$conversion)

    data <- add_column_after_specific_column(data=data,
                                             new_data=units_converted,
                                             new_column_name="crop_sold_units_numeric",
                                             old_column_name="crop_sold_price_quantityunits",
                                             loop_structure=T)

    return(data)
}

#' Crop sold and consumed.
#'
#' Note that for this function to work,
#' previous functions have to have been run. The crops harvested has to have been
#' calculated, as well as the proportions (numeric) which have been sold
#' and consumed
#'
#' @param data RHoMIS data which has been processed to include
#' crops harvested (see above) and crop proportions sold/consumed.
#'
#' @return
#' @export
#'
#' @examples
crop_sold_and_consumed_calculation <- function(data){
    # Beginning with crops sold
    number_of_loops <- find_number_of_loops(data, name_column="crop_name")
    harvested_columns <- paste0("crop_harvest_kg_per_year","_",c(1:number_of_loops))
    sold_columns <- paste0("crop_sold_prop_numeric","_",c(1:number_of_loops))

    if (all(harvested_columns%in%colnames(data))==F)
    {
        stop("Have not calculated the amounts harvested in kg. Calculate amounts harvested before calculating amounts sold")
    }
    if (all(sold_columns%in%colnames(data))==F)
    {
        stop("Have not calculated the numeric proportions of amount of crops sold. Calculate proportions sold before calculating amounts sold")
    }

    harvest_data <- data[harvested_columns]
    sold_prop_data <- data[sold_columns]

    amount_sold_kg <- tibble::as_tibble(harvest_data*sold_prop_data)
    colnames(amount_sold_kg) <- paste0("crop_sold_kg_per_year", "_",c(1:number_of_loops))

    data <- add_column_after_specific_column(data=data,
                                             new_data=amount_sold_kg,
                                             new_column_name="crop_sold_kg_per_year",
                                             old_column_name="crop_sold_prop_numeric",
                                             loop_structure=T)

    # Moving on to crops consumed
    number_of_loops <- find_number_of_loops(data, name_column="crop_name")
    harvested_columns <- paste0("crop_harvest_kg_per_year","_",c(1:number_of_loops))
    consumed_columns <- paste0("crop_consumed_prop_numeric","_",c(1:number_of_loops))

    if (all(harvested_columns%in%colnames(data))==F)
    {
        stop("Have not calculated the amounts harvested in kg. Calculate amounts harvested before calculating amounts consumed")
    }
    if (all(consumed_columns%in%colnames(data))==F)
    {
        stop("Have not calculated the numeric proportions of amount of crops consumed Calculate proportions consumed before calculating amounts consumed")
    }

    harvest_data <- data[harvested_columns]
    consumed_prop_data <- data[consumed_columns]

    amount_consumed_kg <- tibble::as_tibble(harvest_data*consumed_prop_data)
    colnames(amount_consumed_kg) <- paste0("crop_consumed_kg_per_year", "_",c(1:number_of_loops))

    data <- add_column_after_specific_column(data=data,
                                             new_data=amount_consumed_kg,
                                             new_column_name="crop_consumed_kg_per_year",
                                             old_column_name="crop_consumed_prop_numeric",
                                             loop_structure=T)

    return(data)

}

#' Crop Income Calculations
#'
#' A function for calculating crop incomes. Please note
#' for this calculation to work. The amount of a crop harvested, and the amount
#' of a crop sold also needs to have been calculated
#'
#' @param data A RHoMIS dataset, including information on crop harvested,
#' and crop sold
#'
#' @return
#' @export
#'
#' @examples
crop_income_calculations <- function(data){
    number_of_loops <- find_number_of_loops(data, name_column="crop_name")

    crop_sold_columns <- paste0("crop_sold_kg_per_year","_",c(1:number_of_loops))
    crop_sold_unit_columns <- paste0("crop_sold_units_numeric","_",c(1:number_of_loops))
    crop_sold_income_columns <- paste0("crop_sold_income","_",c(1:number_of_loops))

    if (all(crop_sold_columns%in%colnames(data))==F)
    {
        stop("Have not calculated the amounts sold in kg. Calculate amounts sold before calculating income")
    }
    if (all(crop_sold_unit_columns%in%colnames(data))==F)
    {
        stop("Have not converted the crop price quantity units yet. Convert these units before calculating incomes sold")
    }

    crop_sold_amount <- data[crop_sold_columns]
    crop_sold_units <- data[crop_sold_unit_columns]
    crop_sold_income <- data[crop_sold_income_columns]

    crop_sold_units_numeric<-crop_sold_units
    crop_sold_units_numeric[crop_sold_units=="total_income_per_year"] <- NA
    crop_sold_units_numeric <- crop_sold_units_numeric %>% dplyr::mutate_all(as.numeric)


    crop_sold_income_per_year <- crop_sold_income
    # Multiplying values which do not have "total_income_per_year_unit
    crop_sold_income_per_year <- crop_sold_income_per_year*crop_sold_units_numeric*crop_sold_amount

    crop_sold_income_per_year[crop_sold_units=="total_income_per_year"]<-crop_sold_income[crop_sold_units=="total_income_per_year"]
    crop_sold_income_per_year <- tibble::as_tibble(crop_sold_income_per_year)

    colnames(crop_sold_income_per_year) <- paste0("crop_income_per_year","_",c(1:number_of_loops))
    data <- add_column_after_specific_column(data=data,
                                             new_data=crop_sold_income_per_year,
                                             new_column_name="crop_income_per_year",
                                             old_column_name="crop_sold_units_numeric",
                                             loop_structure=T)

    crop_price <- crop_sold_income_per_year/crop_sold_amount
    colnames(crop_price) <- paste0("crop_price","_",c(1:number_of_loops))

    data <- add_column_after_specific_column(data=data,
                                             new_data=crop_price,
                                             new_column_name="crop_price",
                                             old_column_name="crop_income_per_year",
                                             loop_structure=T)


}

crop_gender_calculations <- function(data){



    # Need to include merging for products sold
    crop_sold_kg_per_year


return(data)

}

data <- tibble::as_tibble(list(
    "crop_name_1"=c("maize","cassava","wheat"),
    "crop_yield_1"=c(50,20,10),
    "crop_yield_units_1"=c("kg","sacks_100kg","tonnes"),
    "crop_use_1"=c("eat","eat sell","eat sell feed_lvstk"),
    "crop_consumed_1"=c(NA,"most","half"),
    "crop_sold_1"=c(NA,"little","half"),
    "crop_sold_income_1"=c(200,500,800),
    "crop_sold_price_quantityunits_1"=c("price_per_kg","price_per_bag_50kg","price_per_quintal"),
    "crop_who_control_revenue_1"=c("male_adult","male_adult female_adult",NA),
    "crop_consume_control_1"=c(NA,"male_adult female_adult female_youth","male_youth"),

    "crop_name_2"=c("wheat","maize","millet"),
    "crop_yield_2"=c(12,8,NA),
    "crop_yield_units_2"=c("kg","other_randome_unit","unit_not_registered"),
    "crop_use_2"=c("eat sell","eat sell","eat sell"),
    "crop_consumed_2"=c("most","half","underhalf"),
    "crop_sold_2"=c("little","half","most"),
    "crop_sold_income_2"=c(800,1000,800),
    "crop_sold_price_quantityunits_2"=c("total_income_per_year","price_per_kg","price_per_bag_100kg"),
    "crop_who_control_revenue_2"=c("male_adult","female_adult","male_adult"),
    "crop_consume_control_2"=c("male_youth male_adult","female_youth female_adult","male_adult"),

    "crop_name_3"=c("rice","other_vegetables",NA),
    "crop_yield_3"=c(25,100,NA),
    "crop_yield_units_3"=c("kg","cart_225kg",NA),
    "crop_use_3"=c("sell","eat",NA),
    "crop_consumed_3"=c(NA,NA,NA),
    "crop_sold_3"=c(NA,NA,NA),
    "crop_sold_income_3"=c(800,600,NA),
    "crop_sold_price_quantityunits_3"=c("total_income_per_year","price_per_bag_45kg",NA),
    "crop_who_control_revenue_3"=c("male_adult","female_adult",NA),
    "crop_consume_control_3"=c("male_adult male_adult","female_adult female_adult",NA)
))
