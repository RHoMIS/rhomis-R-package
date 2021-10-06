library(mongolite)
library(tibble)
library(jsonlite)

#' Saving Set of Conversions
#'
#' Save a singly type of conversion factor into the conversion factor database
#'
#' @param database The database which will store the units
#' @param url The url of the database storing the units
#' @param projectID The name of the project the units are related to
#' @param formID The name of the form the projects are related to
#' @param conversions A table of the conversion factors to be used
#' @param conversion_type Which type of conversion is being saves (e.g crop yield, PPI, HDDS etc...)
#'
#' @return
#' @export
#'
#' @examples
save_set_of_conversions <- function(database="rhomis",  url="mongodb://localhost",
                                    projectID="core_unit",
                                    formID="core_unit",
                                    conversions,
                                    conversion_type){

    # conversions <- crop_price_units
    #conversion_type <- "crop_price_units"
    connection <- connect_to_db("units_and_conversions",database,url)

    conversion_data <- jsonlite::toJSON(conversions) %>% clean_json_string()

    insert_query <- paste0('{"projectID":"',projectID,'","formID":"',formID,'","conversionType":"',conversion_type,'"}')
    set_query <- paste0('{"$set":{','"data":',conversion_data,'}}')
    connection$update(insert_query,set_query,upsert = T)

    connection$disconnect()
}


#' Title
#'
#' @param database The database which will store the units
#' @param url The url of the database storing the units
#' @param projectID The name of the project the units are related to
#' @param formID The name of the form the projects are related to
#' @param conversion_data A list of tibbles of the conversion factors to be used
#' @param conversion_types Which types of conversion is being saves (e.g crop yield, PPI, HDDS etc...)
#'
#'
#' @return
#' @export
#'
#' @examples
save_multiple_conversions <- function(database="rhomis",  url="mongodb://localhost",
                                      projectID="core_units",
                                      formID="core_units",
                                      conversion_data,
                                      conversion_types){


    if (length(conversion_data)!=length(conversion_types)){
        stop("Length of conversion data argument not the same as the names for conversion data")
    }

    for (index in 1:length(conversion_data)){
        save_set_of_conversions(database, url,
                                projectID,
                                formID,
                                conversion_data[[index]],
                                conversion_types[index])
    }

}


#' Save Initial Units
#'
#' @param database The database where units are to be saved
#' @param url The URL of the database where units are to be saved, the default mongoDB url is set as default argument.
#' @param projectID The ID of the project for saving the units
#' @param units_and_conversions A list of all of the unit and conversions (list of tibbles)
#' @param formID The ID of the form for saving project units
#'
#' @return
#' @export
#'
#' @examples
save_initial_units <- function(database="rhomis",  url="mongodb://localhost",
                               projectID="core_units",
                               formID="core_units",
                               units_and_conversions = list("crop_price_units"=crop_price_units,
                                                            "crop_yield_units"=crop_yield_units,
                                                            "eggs_amount_units"=eggs_amount_units,
                                                            "eggs_price_time_units"=eggs_price_time_units,
                                                            "honey_amount_units"=honey_amount_units,
                                                            "land_area_units"=land_area_units,
                                                            "livestock_weights"=livestock_weights,
                                                            "milk_amount_units"=milk_amount_units,
                                                            "milk_price_time_units"=milk_price_time_units,
                                                            "ppi_limits"=ppi_limits,
                                                            "ppi_score_card"=ppi_score_card,
                                                            "proportion_conversions"=proportion_conversions)){


    save_multiple_conversions(database,
                              url,
                              projectID,
                              formID,
                              units_and_conversions,
                              names(units_and_conversions))

}

extract_units_from_db <- function(database="rhomis",
                                  url="mongodb://localhost",
                                  projectID="core_units",
                                  formID="core_units",
                                  conversion_type,
                                  collection="units_and_conversions"){

    connection <- connect_to_db(collection,database,url)



    # Arguments to identify the relevant project
    match_arguments = jsonlite::toJSON(list('projectID'=projectID,
                                            'formID'=formID,
                                            'conversionType'=conversion_type), na = "null")
    match_arguments <- gsub("[","",match_arguments, fixed=T)
    match_arguments <- gsub("]","",match_arguments, fixed=T)
    match_arguments <- paste0('{"$match":',match_arguments,'}')
    match_arguments <- clean_json_string(match_arguments)

    # Unwind the subarray into the value of interest
    unwind_arguments <- paste0('{"$unwind": "$data"}')

    # Manage the projection (which values are included or not)
    project_arguments <- jsonlite::toJSON(list("_id"=0,
                                               "formID"=0,
                                               "projectID"=0,
                                               "conversionType"=0),
                                          na = "null")
    project_arguments <- gsub("[","",project_arguments, fixed=T)
    project_arguments <- gsub("]","",project_arguments, fixed=T)
    project_arguments <- paste0('{"$project":',project_arguments,'}')


    # Unwind the subarray into the value of interest
    unwind_data <- paste0('{"$unwind": {"path":"$data", "preserveNullAndEmptyArrays":true}}')

    # Group data

   # e.g
    # [{"$match":{"projectID":"core_unit","formID":"core_unit","conversionType":"ppi_score_card"}},{"$unwind": "$data"},{"$project":{"_id":0,"formID":0,"projectID":0,"conversionType":0}},{"$unwind": {"path":"$data", "preserveNullAndEmptyArrays":true}}]
     pipeline <- paste0('[',match_arguments,',',unwind_arguments,',',project_arguments,',',unwind_data,']')

    # Conducting the final query and reshaping
     result <- connection$aggregate(pipeline = pipeline)
     result <- result$data
     result <- tibble::as_tibble(result)

     connection$disconnect()
    return(result)


}


