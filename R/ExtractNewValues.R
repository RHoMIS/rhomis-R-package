library(tibble)
library(readr)



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
#' data <- tibble::tibble(crop_name_1=c("maize","cassava","millet"),
#' crop_name_2=c("potato",NA,"cucumber"))
#' # Expect the result c("maize", "cassava", "millet","potato", "cucumber")
find_unique_values <- function(data){


    # Finding all of the unique values
    all_values <- as.character(unlist(lapply(data, function(x) unique(x))))
    # Removing NA column
    all_values <- all_values[!is.na(all_values)]
    all_values <- all_values[all_values!="other"]
    all_values <- all_values[all_values!="other1"]
    all_values <- all_values[all_values!="other2"]
    all_values <- all_values[all_values!="other3"]
    # Finding unique values from all the columns
    unique_values <- unique(all_values)
    return(unique_values)
}


#' Extract New Units
#'
#' A function for extracting any new values from a survey
#'
#' @param data The whole data-set from which the units are to
#' be extracted from
#' @param loop_or_individual_column Options are: "loop", "column".
#' Specifying whether you want to extract new
#' values from a loop or a column.
#' @param column_pattern The pattern which is used to
#' identify the columns. For example if trying to
#' extract new crop names, these come in the
#' form "crop_name_1", "crop_name_2", "crop_name_3".
#' The pattern here would be "crop_name"
#' @param number_of_loops If loop_or_individual_column=="loop",
#' then here we specify how many loops to search for.
#' @param column_name If loop_or_individual_column=="column",
#' then here we specify which column to search for
#'
#' @return A list of the individual values for that value (excluding NA values)
#' @export
#'
#' @examples
#'
#' # Example code for a crop loop
#' column_pattern <- "crop_name"
#' number_of_loops <- 2
#' data <- tibble::tibble(crop_name_1=c("maize","cassava","millet"),
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

        if(column_name %in% colnames(data))
        {
            relevant_data <- data[,column_name]
            unique_values <- find_unique_values(relevant_data)
        }
        if(column_name %in% colnames(data)==F)
        {
            return()
        }
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


    if(number_of_loops==0){
        return()
    }

    if(number_of_loops>0)
    {
        new_values <- extract_new_values(data, loop_or_individual_column="loop",column_pattern=column_pattern, number_of_loops=number_of_loops)
    }
    return(new_values)


}



#' Extract New Core Units
#'
#' A function to extract the new values from a core
#' RHoMIS survey. Note that this function will not
#' work on any dataset which has modified the core
#' questions.
#'
#' @param data The RHoMIS core dataset for
#' which we are hoping to extract the new values
#' @return A nested list of all of the unique
#' values for core RHoMIS values
#' @export
#'
#' @examples
#'
#'
#' data <- tibble::tibble(crop_name_1=c("maize","cassava"),
#'                crop_name_2=c(NA,"cucumber"),
#'                random_crop_name_2=c("blue","green"),
#'                livestock_name_1=c("cow","chicken"),
#'                livestock_name_2=c("pig",NA),
#'                crop_yield_units_1=c("kg", "sacks"),
#'                crop_yield_units_2=c("wheelbarrow", NA),
#'                crop_yield_units_other_1=c("other1", NA),
#'                crop_yield_units_other_2=c("other2", "other3"),
#'                crop_sold_price_quantityunits_1=c(NA,"price1"),
#'                crop_sold_price_quantityunits_2=c("price2","price3"),
#'                crop_price_quantityunits_other_1=c(NA, "crop_price_1"),
#'                crop_price_quantityunits_other_2=c(NA, "crop_price_2"),
#'                crop_price_quantityunits_other_3=c(NA, "crop_price_3"),
#'                crop_price_quantityunits_other_4=c(NA, NA),
#'                unitland=c("acre", NA),
#'                areaunits_other=c("area1", "area2"),
#'                unitland_owned=c("unitland1", "unitland2"),
#'                unitland_rentin=c("renty1", "renty2"),
#'                unitland_rentout=c("rent1", "rent5"),
#'                milk_units_1=c("milk1", NA),
#'                milk_units_2=c("milk4", "milk5"),
#'                milk_amount_units_other_1=c("milkoth1", "milkoth2"),
#'                milk_amount_units_other_2=c(NA, "milkoth3"),
#'                milk_sold_price_timeunits_1=c("mspt1", "mspt3"),
#'                milk_sold_price_timeunits_2=c(NA, "mspt5"),
#'                milk_amount_time_units_other_1=c("mspto1", NA),
#'                milk_amount_time_units_other_2=c("mspto2", "mspto3"),
#'                bees_honey_production_units_1=c("hnyprod1", "hnyprod2"),
#'                bees_honey_production_units_2=c(NA, "hnyprod3"),
#'                bees_honey_production_units_other_1=c("hnyprodoth1", NA),
#'                bees_honey_production_units_other_2=c("hnyprodoth2", "hnyprodoth3"),
#'                eggs_units_1=c("egg1", NA),
#'                eggs_units_2=c("egg2", "egg3"),
#'                eggs_amount_units_other_1=c("eggoth1", NA),
#'                eggs_amount_units_other_2=c("eggoth2", "eggoth3"),
#'                eggs_sold_price_timeunits_1=c("eggtime1", "eggtime2"),
#'                eggs_sold_price_timeunits_2=c("eggtime3", NA),
#'                eggs_sold_price_timeunits_3=c(NA, NA),
#'                eggs_sold_price_timeunits_other_1=c("eggtimeoth1", NA),
#'                eggs_sold_price_timeunits_other_2=c("eggtimeoth2", "eggtimeoth3"),
#'                fertiliser_units=c("fert1", "fert2"),
#'                fertiliser_units_other=c("fertoth1","fertoth2"))
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
    loop_results <- sapply(loop_values_to_extract, function(x) find_loop_number_and_extract_values(data,x), simplify = FALSE)

    individual_columns_to_extract <- c("country",
                                       "crops_other1",
                                       "crops_other2",
                                       "crops_other3",
                                       "livestock_other1",
                                       "livestock_other2",
                                       "livestock_other3",
                                       "unitland",
                                       "areaunits_other",
                                       "areaunits_other_own",
                                       "areaunits_other_rent",
                                       "unitland_owned",
                                       "unitland_rentin",
                                       "unitland_rentout",
                                       "fertiliser_units",
                                       "fertiliser_units_other")
    column_results <- sapply(individual_columns_to_extract, function(x) extract_new_values(data,loop_or_individual_column="column", column_name=x), simplify = FALSE)



    categories_to_merge <- list(
        country=c("country"),
        crop_name=c("crop_name","crops_other1","crops_other2", "crops_other3"),
        livestock_name=c("livestock_name", "livestock_other1","livestock_other2","livestock_other3"),
        crop_yield_units=c("crop_yield_units_other"),
        crop_sold_price_quantityunits=c("crop_price_quantityunits_other"),
        unitland= c("areaunits_other","unitland_owned","unitland_rentin","unitland_rentout", "areaunits_other_own", "areaunits_other_rent"),
        milk_units=c("milk_amount_units_other"),
        milk_sold_price_timeunits=c("milk_amount_time_units_other"),
        bees_honey_production_units=c("bees_honey_production_units_other"),
        eggs_units=c("eggs_amount_units_other"),
        eggs_sold_price_timeunits=c("eggs_sold_price_timeunits_other"),
        fertiliser_units=c("fertiliser_units_other"))



    final_result <- c(loop_results,column_results)



    final_result<-sapply(names(categories_to_merge),function(x) merge_and_simplify_core_values(final_result,main_item=x,categories_to_merge), simplify = FALSE)




    return(final_result)


}


#' Merge and Simplify Core Values
#'
#' When extracting new values from the
#' RHoMIS dataset, some variables appear
#' in multiple columns. For example with
#' units, we can have "crop_yield_units"
#' and "crop_yield_units_other". These are
#' all crop_yield units and we would like
#' to ensure they are aggregated appropriately.
#'
#' This is heavily linked to the "extract_new_core_units"
#' function.
#'
#'
#' @param list_of_unique_core_values A nested list of new values
#' from a RHoMIS survey
#' @param main_item The primary item that you want to merge.
#' In "crop_yield_units" and "crop_yield_units_other",
#' the main item would be "crop_yield_units"
#' @param categories_to_merge A named list of the categories
#' which go together, please see example
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

#' Write Core Values to File
#'
#' This function extracts all of the new units and
#' names in a RHoMIS survey and writes them to a folder
#' You can select the folder that you would like
#' to write all of the files to.
#'
#' @param data The data for which you want to
#' extract all of the new files.
#' @param folder The folder where you want to
#' store all of the values you need to convert.
#' Make sure the folder is correct and does not end in "/"
#'
#' @return
#' @export
#'
#' @examples
#' data <- tibble::tibble(crop_name_1=c("maize","cassava"),
#'                crop_name_2=c(NA,"cucumber"),
#'                random_crop_name_2=c("blue","green"),
#'                livestock_name_1=c("cow","chicken"),
#'                livestock_name_2=c("pig",NA),
#'                crop_yield_units_1=c("kg", "sacks"),
#'                crop_yield_units_2=c("wheelbarrow", NA),
#'                crop_yield_units_other_1=c("other1", NA),
#'                crop_yield_units_other_2=c("other2", "other3"),
#'                crop_sold_price_quantityunits_1=c(NA,"price1"),
#'                crop_sold_price_quantityunits_2=c("price2","price3"),
#'                crop_price_quantityunits_other_1=c(NA, "crop_price_1"),
#'                crop_price_quantityunits_other_2=c(NA, "crop_price_2"),
#'                crop_price_quantityunits_other_3=c(NA, "crop_price_3"),
#'                crop_price_quantityunits_other_4=c(NA, NA),
#'                unitland=c("acre", NA),
#'                areaunits_other=c("area1", "area2"),
#'                unitland_owned=c("unitland1", "unitland2"),
#'                unitland_rentin=c("renty1", "renty2"),
#'                unitland_rentout=c("rent1", "rent5"),
#'                milk_units_1=c("milk1", NA),
#'                milk_units_2=c("milk4", "milk5"),
#'                milk_amount_units_other_1=c("milkoth1", "milkoth2"),
#'                milk_amount_units_other_2=c(NA, "milkoth3"),
#'                milk_sold_price_timeunits_1=c("mspt1", "mspt3"),
#'                milk_sold_price_timeunits_2=c(NA, "mspt5"),
#'                milk_amount_time_units_other_1=c("mspto1", NA),
#'                milk_amount_time_units_other_2=c("mspto2", "mspto3"),
#'                bees_honey_production_units_1=c("hnyprod1", "hnyprod2"),
#'                bees_honey_production_units_2=c(NA, "hnyprod3"),
#'                bees_honey_production_units_other_1=c("hnyprodoth1", NA),
#'                bees_honey_production_units_other_2=c("hnyprodoth2", "hnyprodoth3"),
#'                eggs_units_1=c("egg1", NA),
#'                eggs_units_2=c("egg2", "egg3"),
#'                eggs_amount_units_other_1=c("eggoth1", NA),
#'                eggs_amount_units_other_2=c("eggoth2", "eggoth3"),
#'                eggs_sold_price_timeunits_1=c("eggtime1", "eggtime2"),
#'                eggs_sold_price_timeunits_2=c("eggtime3", NA),
#'                eggs_sold_price_timeunits_3=c(NA, NA),
#'                eggs_sold_price_timeunits_other_1=c("eggtimeoth1", NA),
#'                eggs_sold_price_timeunits_other_2=c("eggtimeoth2", "eggtimeoth3"),
#'                fertiliser_units=c("fert1", "fert2"),
#'                fertiliser_units_other=c("fertoth1","fertoth2"))
#'
#' # write_core_values_to_convert_to_file(data, "My/Folder)
#'
write_core_values_to_convert_to_file <- function(data, folder){

    new_units <- extract_units_data_frames(data)
    sapply(names(new_units), function(x) write_new_values_to_file(specific_value=x,list_of_tibbles=new_units, folder=folder),simplify = FALSE)
}

#' Extract Units to DataFrame
#'
#' @param data The core RHoMIS dataset which we are extracting units from
#'
#' @return A list of data frames
#' @export
#'
#' @examples
extract_units_data_frames <- function(data){
    new_units <- extract_new_core_units(data)
    new_units<- sapply(new_units, function(x) convert_new_values_to_tibble(x),simplify = FALSE)

    return(new_units)

}

#' Convert New Values to Tibble
#'
#' The output of the "extract_new_core_units" function
#' is a list of lists. With the names of the lists
#' are the variables to be converted and the lists
#' contain all of the new values.
#' This function converts all of these lists into tibbles.
#' With a new column which has to be filled
#' in by the user
#'
#' @param new_values A list of lists containing the values to convert
#'
#' @return
#' @export
#'
#' @examples
#'
#' new_values <- c("egg1","egg2","egg3" )
#' expected_result <- tibble::as_tibble(list("survey_value"=c("egg1","egg2","egg3" ), "conversion"=c(NA,NA,NA)))
#'
#' actual_result <- convert_new_values_to_tibble(new_values)
#'
convert_new_values_to_tibble <- function(new_values){


    #new_values <- c("x","y","z")
    data_to_return <- tibble::as_tibble(list("survey_value"=new_values, "conversion"=rep(NA,length(new_values))))

    return(data_to_return)

}



#' Write units tibble to csv
#'
#' Write the specific tibble to a csv.
#'
#' @param specific_value Which attribute are you
#' hoping to write to csv (e.g. "crop_yield_units")
#' @param list_of_tibbles What are all of the units
#' which you have extracted from the RHoMIS project.
#' See function "extract_new_core_units"
#' @param folder What is the folder you hope to write this to.
#' Do not include "/" at the end of the file path
#'
#' @return No return
#' @export
#'
#' @examples
#'
#' # Please make sure the correct folder exists for you
#' specific_value <- "egg_units"
#' new_values <- list(egg_units = c("egg1","egg2","egg3" ),
#'                    other_units = c("other1","other2","other3" ))
#' list_of_tibbles <- sapply(new_values, function(x) convert_new_values_to_tibble(x),simplify = FALSE)
#' folder <- "data/test_outputs/write_new_values_to_file"
#'
#'
#' #write_new_values_to_file(specific_value,list_of_tibbles,folder)
#'
write_new_values_to_file <- function(specific_value,list_of_tibbles, folder){


    data <- list_of_tibbles[[specific_value]]
    readr::write_csv(data,paste0(folder,"/",specific_value,".csv"))

}



