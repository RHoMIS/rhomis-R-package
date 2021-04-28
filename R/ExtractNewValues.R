library(tidyverse)



#' Finding Unique Values
#'
#' A function to find the unique values for all of a given tibble.
#'
#' @param data A tibble for which we hope to find all of the unique values
#'
#' @return A list of the unique values, NAs excluded
#' @export
#'
#' @examples
#' data <- tibble(crop_name_1=c("maize","cassava","millet"),
#' crop_name_2=c("potato",NA,"cucumber"))
#' # Expect the result c("maize", "cassava", "millet","potato", "cucumber")
find_unique_values <- function(data){


    # Finding all of the unique values
    all_values <- as.character(unlist(lapply(data, function(x) unique(x))))
    # Removing NA column
    all_values <- all_values[!is.na(all_values)]
    # Finding unique values from all the columns
    unique_values <- unique(all_values)
    return(unique_values)
}


#' Extract New Units
#'
#' A function for extracting any new values from a survey
#'
#' @param data The whole data-set from which the units are to be extracted from
#' @param loop_or_individual_column Options are: "loop", "column". Specifying whether you want to extract new values from a loop or a column.
#' @param column_pattern The pattern which is used to identify the columns. For example if trying to extract new crop names, these come in the form "crop_name_1", "crop_name_2", "crop_name_3". The pattern here would be "crop_name"
#' @param number_of_loops If loop_or_individual_column=="loop", then here we specify how many loops to search for.
#' @param column_name If loop_or_individual_column=="column", then here we specify which column to search for
#'
#' @return A list of the individual values for that value (excluding NA values)
#' @export
#'
#' @examples
#'
#' # Example code for a crop loop
#' column_pattern <- "crop_name"
#' number_of_loops <- 2
#' data <- tibble(crop_name_1=c("maize","cassava","millet"),
#'                random_columns=c("maize","cassava","millet"),
#'                crop_name_2=c("potato",NA,"cucumber"))
#' result <- extract_new_values(data, loop_or_individual_column="loop",column_pattern="crop_name", number_of_loops=2)
#' # expected_result is: c("maize","cassava","millet","potato","cucumber")
#'
#' # Example code for an individual column

extract_new_values <- function(data, loop_or_individual_column="loop",column_name=NA,column_pattern=NA, number_of_loops=NA){
    # Extracting new values for a RHoMIS tibble
    if (loop_or_individual_column=="loop")
    {
        if (is.na(column_pattern)){
            stop("You have selected to extract new values from loops. Please specify a 'column_pattern' for those loop.")
        }
        if (is.na(number_of_loops)){
            stop("You have selected to extract new values from loops. Please specify a 'number_of_loops' for those loops.")
        }
        relevant_data <- data[,paste0(column_pattern,"_",1:number_of_loops)]
        unique_values <- find_unique_values(relevant_data)
        return(unique_values)
    }

    if (loop_or_individual_column=="column")
    {
        if (is.na(column_name)){
            stop("You have selected to extract new values from a specific column. Please specify a 'column_name'.")
        }


        relevant_data <- data[,column_name]
        unique_values <- find_unique_values(relevant_data)
        return(unique_values)
    }
}

find_loop_number_and_extract_values <- function(data, column_pattern){

    # Creating a pattern to search through the column names of the data
    regex_pattern <- paste0("^",column_pattern,"_[[:digit:]]") # Finding columns which start with "column_pattern_Integer"

    # Finding the relevant column names
    relevant_columns <- grep(regex_pattern, colnames(data), value=T)
    # The number of loops for this column
    number_of_loops <- length(relevant_columns)

    new_values <- extract_new_values(data, loop_or_individual_column="loop",column_pattern=column_pattern, number_of_loops=number_of_loops)

    return(new_values)


}





#' Extract New Core Units
#'
#' A function to extract the new values from a core RHoMIS survey. Note that this function will not work on any dataset which has modified the core questions
#'
#' @param data The RHoMIS core dataset for which we are hoping to extract the new values
#'
#' @return A nested list of all of the unique values for core RHoMIS values
#' @export
#'
#' @examples
#'
#'
#' data <- tibble(crop_name_1=c("maize","cassava"),
#' crop_name_2=c(NA,"cucumber"),
#' random_crop_name_2=c("blue","green"),
#' livestock_name_1=c("cow","chicken"),
#' livestock_name_2=c("pig",NA),
#' crop_yield_units_1=c("kg", "sacks"),
#' crop_yield_units_2=c("wheelbarrow", NA),
#' crop_yield_units_other_1=c("other1", NA),
#' crop_yield_units_other_2=c("other2", "other3"),
#' crop_sold_price_quantityunits_1=c(NA,"price1"),
#' crop_sold_price_quantityunits_2=c("price2","price3"),
#' crop_price_quantityunits_other_1=c(NA, "crop_price_1"),
#' crop_price_quantityunits_other_2=c(NA, "crop_price_2"),
#' crop_price_quantityunits_other_3=c(NA, "crop_price_3"),
#' crop_price_quantityunits_other_4=c(NA, NA),
#' unitland_1=c("acre", NA),
#' unitland_2=c("hectare", "m2"),
#' areaunits_other_1=c("area1", "area2"),
#' areaunits_other_2=c(NA, "area3"),
#' unitland_owned_1=c("unitland1", "unitland2"),
#' unitland_owned_2=c(NA, "unitland3"),
#' unitland_rentin_1=c("renty1", "renty2"),
#' unitland_rentin_2=c("renty3", NA),
#' unitland_rentin_3=c(NA, NA),
#' unitland_rentout_1=c("rent1", NA),
#' unitland_rentout_2=c(NA, NA),
#' unitland_rentout_3=c("rent2", "rent5"),
#' milk_units_1=c("milk1", NA),
#' milk_units_2=c("milk4", "milk5"),
#' milk_amount_units_other_1=c("milkoth1", "milkoth2"),
#' milk_amount_units_other_2=c(NA, "milkoth3"),
#' milk_sold_price_timeunits_1=c("mspt1", "mspt3"),
#' milk_sold_price_timeunits_2=c(NA, "mspt5"),
#' milk_amount_time_units_other_1=c("mspto1", NA),
#' milk_amount_time_units_other_2=c("mspto2", "mspto3"),
#' bees_honey_production_units_1=c("hnyprod1", "hnyprod2"),
#' bees_honey_production_units_2=c(NA, "hnyprod3"),
#' bees_honey_production_units_other_1=c("hnyprodoth1", NA),
#' bees_honey_production_units_other_2=c("hnyprodoth2", "hnyprodoth3"),
#' eggs_units_1=c("egg1", NA),
#' eggs_units_2=c("egg2", "egg3"),
#' eggs_amount_units_other_1=c("eggoth1", NA),
#' eggs_amount_units_other_2=c("eggoth2", "eggoth3"),
#' eggs_sold_price_timeunits_1=c("eggtime1", "eggtime2"),
#' eggs_sold_price_timeunits_2=c("eggtime3", NA),
#' eggs_sold_price_timeunits_3=c(NA, NA),
#' eggs_sold_price_timeunits_other_1=c("eggtimeoth1", NA),
#' eggs_sold_price_timeunits_other_2=c("eggtimeoth2", "eggtimeoth3"),
#' fertiliser_units=c("fert1", "fert2"),
#' fertiliser_units_other=c("fertoth1","fertoth2"))
#'
#'
#' result <- extract_new_core_units(data)
#'
#' expected_result <- c(list("crop_name"=c("maize","cassava", "cucumber"),
#'                           "livestock_name"=c("cow","chicken","pig"),
#'                           "crop_yield_units"=c("kg","sacks","wheelbarrow","other1","other2","other3"),
#'                           "crop_sold_price_quantityunits"=c("price1","price2","price3","crop_price_1","crop_price_2","crop_price_3"),
#'                           "unitland"=c("acre","hectare","m2","area1","area2","area3","unitland1","unitland2","unitland3","renty1","renty2","renty3","rent1","rent2","rent5"),
#'                           "milk_units"=c("milk1","milk4","milk5","milkoth1","milkoth2","milkoth3"),
#'                           "milk_sold_price_timeunits"=c("mspt1","mspt3","mspt5","mspto1","mspto2","mspto3"),
#'                           "bees_honey_production_units"=c("hnyprod1","hnyprod2","hnyprod3","hnyprodoth1","hnyprodoth2","hnyprodoth3"),
#'                           "eggs_units"=c("egg1","egg2","egg3", "eggoth1","eggoth2","eggoth3"),
#'                           "eggs_sold_price_timeunits"=c("eggtime1","eggtime2","eggtime3","eggtimeoth1","eggtimeoth2","eggtimeoth3"),
#'                           "fertiliser_units"=c("fert1","fert2","fertoth1", "fertoth2")))
#'
#'


extract_new_core_units <- function(data)
{
    loop_values_to_extract <- c("crop_name",
                                "livestock_name",
                                "crop_yield_units",
                                "crop_yield_units_other",
                                "crop_sold_price_quantityunits",
                                "crop_price_quantityunits_other",
                                "unitland",
                                "areaunits_other",
                                "unitland_owned",
                                "unitland_rentin",
                                "unitland_rentout",
                                "milk_units",
                                "milk_amount_units_other",
                                "milk_sold_price_timeunits",
                                "milk_amount_time_units_other",
                                "bees_honey_production_units",
                                "bees_honey_production_units_other",
                                "eggs_units",
                                "eggs_amount_units_other",
                                "eggs_sold_price_timeunits",
                                "eggs_sold_price_timeunits_other")


    # Return a named list when applying the function
    loop_results <- sapply(loop_values_to_extract, function(x) find_loop_number_and_extract_values(data,x), simplify = F)

    individual_columns_to_extract <- c("fertiliser_units","fertiliser_units_other")
    column_results <- sapply(individual_columns_to_extract, function(x) extract_new_values(data,loop_or_individual_column="column", column_name=x), simplify = F)



    categories_to_merge <- list(crop_name=c("crop_name"),
                                livestock_name=c("livestock_name"),
                                crop_yield_units=c("crop_yield_units_other"),
                                crop_sold_price_quantityunits=c("crop_price_quantityunits_other"),
                                unitland= c("areaunits_other","unitland_owned","unitland_rentin","unitland_rentout"),
                                milk_units=c("milk_amount_units_other"),
                                milk_sold_price_timeunits=c("milk_amount_time_units_other"),
                                bees_honey_production_units=c("bees_honey_production_units_other"),
                                eggs_units=c("eggs_amount_units_other"),
                                eggs_sold_price_timeunits=c("eggs_sold_price_timeunits_other"),
                                fertiliser_units=c("fertiliser_units_other"))



    final_result <- c(loop_results,column_results)

    final_result<-sapply(names(categories_to_merge),function(x) merge_and_simplify_core_values(final_result,main_item=x,categories_to_merge), simplify = F)


    return(final_result)


}


#' Merge and Simplify Core Values
#'
#' When extracting new values from the RHoMIS dataset, some variables appear in multiple columns.
#' For example with units, we can have "crop_yield_units" and "crop_yield_units_other". These are all crop_yield units
#' and we would like to ensure they are aggregated appropriately.
#'
#' This is heavily linked to the "extract_new_core_units" function.
#'
#'
#' @param list_of_unique_core_values A nested list of new values from a RHoMIS survey
#' @param main_item The primary item that you want to merge. In "crop_yield_units" and "crop_yield_units_other", the main item would be "crop_yield_units"
#' @param categories_to_merge A named list of the categories which go together, please see example
#'
#' @return
#' @export
#'
#' @examples
#'
#' categories_to_merge <- list(crop_name=c("crop_name"),
#' livestock_name=c("livestock_name"),
#' crop_yield_units=c("crop_yield_units_other"),
#' crop_sold_price_quantityunits=c("crop_price_quantityunits_other"),
#' unitland= c("areaunits_other","unitland_owned","unitland_rentin","unitland_rentout"),
#' milk_units=c("milk_amount_units_other"),
#' milk_sold_price_timeunits=c("milk_amount_time_units_other"),
#' bees_honey_production_units=c("bees_honey_production_units_other"),
#' eggs_units=c("eggs_amount_units_other"),
#' eggs_sold_price_timeunits=c("eggs_sold_price_timeunits_other"),
#' fertiliser_units=c("fertiliser_units_other"))
#'
#' list_of_unique_core_values <- c(list(crop_sold_price_quantityunits=c("cropPriceA","cropPriceB", "cropPriceC"),
#'                                      crop_price_quantityunits_other=c("cropPriceD","cropPriceE", "cropPriceF"),
#'                                      random_unimportant_column=c("bla", "bla", "bla")))
#'
#' main_item <- "crop_sold_price_quantityunits"
#' categories_to_merge <- list(crop_name=c("crop_name"),
#'                             livestock_name=c("livestock_name"),
#'                             crop_yield_units=c("crop_yield_units_other"),
#'                             crop_sold_price_quantityunits=c("crop_price_quantityunits_other"),
#'                             unitland= c("areaunits_other","unitland_owned","unitland_rentin","unitland_rentout"),
#'                             milk_units=c("milk_amount_units_other"),
#'                             milk_sold_price_timeunits=c("milk_amount_time_units_other"),
#'                             bees_honey_production_units=c("bees_honey_production_units_other"),
#'                             eggs_units=c("eggs_amount_units_other"),
#'                             eggs_sold_price_timeunits=c("eggs_sold_price_timeunits_other"),
#'                             fertiliser_units=c("fertiliser_units_other"))
#'
#' expected_result <- c("cropPriceA","cropPriceB","cropPriceC","cropPriceD","cropPriceE","cropPriceF")
#' merge_and_simplify_core_values(list_of_unique_core_values, main_item,categories_to_merge)


merge_and_simplify_core_values <- function(list_of_unique_core_values, main_item,categories_to_merge){


    extras_categories_merge<-categories_to_merge[[main_item]]
    extra_values_to_merge <- list_of_unique_core_values[extras_categories_merge]
    extra_values_to_merge <- unlist(extra_values_to_merge)
    extra_values_to_merge <- unique(extra_values_to_merge)

    list_of_unique_core_values[[main_item]] <- c(list_of_unique_core_values[[main_item]],extra_values_to_merge)
    list_of_unique_core_values[[main_item]] <- unique(list_of_unique_core_values[[main_item]])

    return(list_of_unique_core_values[[main_item]])
}



