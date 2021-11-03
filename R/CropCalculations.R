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
#' @param units Units for crop yield
#' @param unit_conversions Conversions to convert these units
#' to kg
#'
#'
#' @return
#' @export
#'
#' @examples
convert_crop_yield_units <- function(data, units=crop_yield_units$unit, unit_conversions=crop_yield_units$conversion){

    missing_columns <- check_columns_in_data(data,
                                             loop_columns = c("crop_name",
                                                              "crop_yield_units"),
                                             warning_message = "Tried to convert crop yield units, will not be able to calculate crop yields. Other indicators will also be affected")

    if (length(missing_columns)==0)
    {
        number_of_loops <- find_number_of_loops(data,name_column = "crop_name")
        columns_to_convert <- paste0("crop_yield_units","_",c(1:number_of_loops))
        new_column_names <- paste0("crop_yield_units_numeric","_",c(1:number_of_loops))
        numeric_crop_units <- switch_units(data[columns_to_convert], units =units, conversion_factors = unit_conversions)
        colnames(numeric_crop_units) <-new_column_names
        data <- add_column_after_specific_column(data=data,
                                                 new_data=numeric_crop_units,
                                                 new_column_name="crop_yield_units_numeric",
                                                 old_column_name="crop_yield_units",
                                                 loop_structure=T)
    }
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
    missing_columns <- check_columns_in_data(data,
                                             individual_columns=c(paste0("crop_yield","_",loop_number),paste0("crop_yield_units_numeric","_",loop_number)),
                                             warning_message = "Tried to calculate crop yield, but some information was missing.")

    if (length(missing_columns)==0)
    {
        crop_yield <- as.numeric(data[[paste0("crop_yield","_",loop_number)]])*as.numeric(data[[paste0("crop_yield_units_numeric","_",loop_number)]])
    }
    return(crop_yield)
}

#' Crop harvest calculations
#'
#' Calculating the amount of each crop harvested per year
#'
#' @param data RHoMIS data containing cropping information
#' @param units Units to convert crop yields to kg
#' @param unit_conversions Converstion factors to convert
#' crop yields to kg
#'
#' @return
#' @export
#'
#' @examples
crop_harvest_calculations <- function(data, units=crop_yield_units$unit, unit_conversions=crop_yield_units$conversion){
     missing_columns <- check_columns_in_data(data,
                                              loop_columns=c("crop_name"),
                                              warning_message = "Tried to calculate crop yield, but some information was missing.")

     if (length(missing_columns)==0)
     {
    number_of_loops <- find_number_of_loops(data,name_column = "crop_name")
    data <- convert_crop_yield_units(data, units, unit_conversions)

    new_column_names <- paste0("crop_harvest_kg_per_year","_",1:number_of_loops)
    crop_harvest_per_year <- sapply(c(1:number_of_loops),function(x) crop_harvest_single_loop(data,x))
    colnames(crop_harvest_per_year) <-new_column_names
    crop_harvest_per_year <- tibble::as_tibble(crop_harvest_per_year)

    data <- add_column_after_specific_column(data=data,
                                             new_data=crop_harvest_per_year,
                                             new_column_name="crop_harvest_kg_per_year",
                                             old_column_name="crop_yield_units_numeric",
                                             loop_structure=T)
    }

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



#' Convert Crop Sold Units
#'
#' Convert crop income units into a numeric conversion factor
#'
#' @param data RHoMIS data containing crop-looping information
#' @param units A list of units to be converted
#' @param unit_conversions The numeric conversion factors for the units listed above
#'
#' @return
#' @export
#'
#' @examples
convert_crop_sold_units <- function(data, units = crop_price_units$unit,unit_conversions=crop_price_units$conversion){
    number_of_loops <- find_number_of_loops(data, name_column = "crop_name")
    crop_sold_units_column <- paste0("crop_sold_price_quantityunits","_",c(1:number_of_loops))

    crop_sold_units <- data[crop_sold_units_column]
    colnames(crop_sold_units)<-paste0("crop_sold_units_numeric","_", c(1:number_of_loops))
    units_converted <- switch_units(crop_sold_units,
                                    units = units,
                                    conversion_factors = unit_conversions)

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
    data <- crop_proportions_all(data)
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
#' @param units Units for crop income calculations which
#' need to be converted
#' @param unit_conversions Conversions for these crop income
#' units
#'
#' @return
#' @export
#'
#' @examples
crop_income_calculations <- function(data, units=crop_price_units$unit, unit_conversions=crop_price_units$conversion){

    data <- convert_crop_sold_units(data,units =units ,unit_conversions = unit_conversions)

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
    crop_sold_income_per_year <- crop_sold_income_per_year %>% dplyr::mutate_all(as.numeric)
    crop_sold_amount <- crop_sold_amount %>% dplyr::mutate_all(as.numeric)

    crop_sold_income_per_year <- crop_sold_income_per_year*crop_sold_units_numeric*crop_sold_amount

    subscript_frame <- crop_sold_units=="total_income_per_year"
    subscript_frame[is.na(subscript_frame)] <- FALSE
    crop_sold_income_per_year[subscript_frame]<-as.numeric(crop_sold_income[subscript_frame])
    crop_sold_income_per_year <- tibble::as_tibble(crop_sold_income_per_year)
    crop_sold_income_per_year <- crop_sold_income_per_year %>% dplyr::mutate_all(as.numeric)


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

#' Crop Gender Calculations
#'
#' Adding the gendered information for crop consumed
#' crop sold, and crop income. These have to already have been
#' calculated in the dataset being used
#'
#' @param data RHoMIS data set conatining processed values for
#' crop consumed, crop sold, and crop income
#'
#' @return
#' @export
#'
#' @examples
crop_gender_calculations <- function(data){
    # crop consumed calculations
    data<-insert_gender_columns_in_core_data(data=data,
                                             original_column="crop_consumed_kg_per_year",
                                             control_column="crop_consume_control",
                                             loop_structure=T)

    # crop sold calculations
    data<-insert_gender_columns_in_core_data(data,
                                             original_column="crop_sold_kg_per_year",
                                             control_column="crop_who_control_revenue",
                                             loop_structure=T)

    # crop income calculations
    data<-insert_gender_columns_in_core_data(data,
                                             original_column="crop_income_per_year",
                                             control_column="crop_who_control_revenue",
                                             loop_structure=T)

    return(data)

}

#' Crop Calculations All
#'
#' A single function for conducting all of the crop calculations
#'
#'
#' @param data RHoMIS crop loop data
#' @param crop_yield_units_all Units of crop yield
#' @param crop_yield_unit_conversions_all Conversion factors to convert crop yield units
#' to kg
#' @param crop_income_units_all Units of crop income
#' @param crop_income_unit_conversions_all Conversion factors for crop
#' income units
#'
#' @return
#' @export
#'
#' @examples
crop_calculations_all <- function(data,
                                  crop_yield_units_all=crop_yield_units$unit,
                                  crop_yield_unit_conversions_all=crop_yield_units$conversion,
                                  crop_income_units_all=crop_price_units$unit,
                                  crop_income_unit_conversions_all=crop_price_units$conversion){

    # Calculating the amount of crops harvested in kg
    crop_columns_in_data <- check_columns_in_data(data,
                                                  loop_columns = c("crop_name",
                                                                   "crop_yield",
                                                                   "crop_yield_units"),
                                                  warning_message = "Cannot calculate amounts of crops harvested")
    if(length(crop_columns_in_data)==0){
    data <- crop_harvest_calculations(data,
                                      units = crop_yield_units_all,
                                      unit_conversions = crop_yield_unit_conversions_all)
    }



    # Calculating amounts sold and consumed in kg
    crop_columns_in_data <- check_columns_in_data(data,
                                                  loop_columns = c("crop_harvest_kg_per_year",
                                                                   "crop_sold_prop",
                                                                   "crop_consumed_prop"),
                                                  warning_message = "Cannot calculate amounts of crops crops sold and consumed")
    if(length(crop_columns_in_data)==0){
    data <- crop_sold_and_consumed_calculation(data)
    }
    if(length(crop_columns_in_data)>0){
        warning(paste0("Cannot calculate the amounts of crops sold and consumed, missing the following columns: \n", crop_columns_in_data, collapse = "\n"))
    }

    # Crop income calculations
    crop_columns_in_data <- check_columns_in_data(data,
                                                  loop_columns = c("crop_sold_kg_per_year",
                                                                   "crop_sold_price_quantityunits",
                                                                   "crop_sold_income"),
                                                  warning_message = "Cannot calculate amounts of crop incomes")
    if(length(crop_columns_in_data)==0){
    data <- crop_income_calculations(data,
                                     units = crop_income_units_all,
                                     unit_conversions = crop_income_unit_conversions_all)

    }


    crop_columns_in_data <- check_columns_in_data(data,
                                                  loop_columns = c("crop_consumed_kg_per_year",
                                                                   "crop_consume_control",
                                                                   "crop_sold_kg_per_year",
                                                                   "crop_who_control_revenue",
                                                                   "crop_income_per_year"
                                                                   ),
                                                  warning_message = "Cannot calculate gendered crop splits")
    if(length(crop_columns_in_data)==0){
    data <- crop_gender_calculations(data)
    }
    return(data)
}




