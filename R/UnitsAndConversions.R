library(mongolite)
library(tibble)
library(jsonlite)
library(tibble)
library(dplyr)
library(readr)

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

    conversion_data <- jsonlite::toJSON(conversions, na = "null") %>% clean_json_string()

    insert_query <- paste0('{"projectID":"',projectID,'","formID":"',formID,'","conversionType":"',conversion_type,'"}')
    set_query <- paste0('{"$set":{','"data":',conversion_data,'}}')
    connection$update(insert_query,set_query,upsert = T)

    connection$disconnect()

    connection <- connect_to_db("projectData",database,url)
    insert_query <- paste0('{"projectID":"',projectID,'","formID":"',formID,'"}')
    set_query <- paste0('{"$addToSet":{"units":','"',conversion_type,'"}}')

    connection$update(insert_query,set_query,upsert = T)


    connection$disconnect()



}


#' Save Multiple conversions
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


#' Load all db units
#'
#' Load all units from a local MongoDB databases
#'
#' @param database The database you are querying units from
#' @param projectID The Name of the project
#' @param formID The name of the form
#' @param unit_list The list of units which are to be queried and loaded into the global environment
#'
#' @return
#' @export
#'
#' @examples
load_all_db_units <- function(unit_list,database="rhomis", projectID="core_units", formID="core_units"){



    if ("country" %in% unit_list){
        country_conversions <- extract_units_from_db(database,
                              url="mongodb://localhost",
                              projectID=projectID,
                              formID=formID,
                              conversion_type="country",
                              collection="units_and_conversions")
        assign("country_conversions", country_conversions, envir = .GlobalEnv)
    }
    if ("country" %in% unit_list==F){
        warning('Tried to find country name conversions, but could not find records in projectData collection')
        country_conversions <- country
        assign("country_conversions", country_conversions, envir = .GlobalEnv)
    }


    if ("crop_name" %in% unit_list){
        crop_name_conversions <- extract_units_from_db(database,
                                                            url="mongodb://localhost",
                                                            projectID=projectID,
                                                            formID=formID,
                                                            conversion_type="crop_name",
                                                            collection="units_and_conversions")
        assign("crop_name_conversions", crop_name_conversions, envir = .GlobalEnv)

    }
    if ("crop_name" %in% unit_list==F){
        warning('Tried to find crop name conversions, but could not find records in projectData collection')
        crop_name_conversions <- tibble::as_tibble(list("survey_value"=crop_name, "conversion"=crop_name))
        assign("crop_name_conversions", crop_name_conversions, envir = .GlobalEnv)

    }


    if ("livestock_name" %in% unit_list){
        livestock_name_conversions <- extract_units_from_db(database,
                                                     url="mongodb://localhost",
                                                     projectID=projectID,
                                                     formID=formID,
                                                     conversion_type="livestock_name",
                                                     collection="units_and_conversions")
        assign("livestock_name_conversions", livestock_name_conversions, envir = .GlobalEnv)

    }
    if ("livestock_name" %in% unit_list==F){
        warning('Tried to find livestock name conversions, but could not find records in projectData collection')
        livestock_name_conversions <- tibble::as_tibble(list("survey_value"=livestock_name, "conversion"=livestock_name))
        assign("livestock_name_conversions", livestock_name_conversions, envir = .GlobalEnv)
    }


    if ("crop_yield_units" %in% unit_list){
        crop_yield_unit_conversions <- extract_units_from_db(database,
                                                       url="mongodb://localhost",
                                                       projectID=projectID,
                                                       formID=formID,
                                                       conversion_type="crop_yield_units",
                                                       collection="units_and_conversions")
        assign("crop_yield_unit_conversions", crop_yield_unit_conversions, envir = .GlobalEnv)

    }
    if ("crop_yield_units" %in% unit_list==F){
        warning('Tried to find crop yield unit conversions, but could not find records in projectData collection')
        crop_yield_unit_conversions <- crop_yield_units
        colnames(crop_yield_unit_conversions) <- c("survey_value", "conversion")

        assign("crop_yield_unit_conversions", crop_yield_unit_conversions, envir = .GlobalEnv)
    }



    if ("crop_sold_price_quantityunits" %in% unit_list){
        crop_price_unit_conversions <- extract_units_from_db(database,
                                                             url="mongodb://localhost",
                                                             projectID=projectID,
                                                             formID=formID,
                                                             conversion_type="crop_sold_price_quantityunits",
                                                             collection="units_and_conversions")
        assign("crop_price_unit_conversions", crop_price_unit_conversions, envir = .GlobalEnv)

    }
    if ("crop_sold_price_quantityunits" %in% unit_list==F){
        warning('Tried to find crop price unit conversions, but could not find records in projectData collection')
        crop_price_unit_conversions <- crop_price_units
        colnames(crop_price_unit_conversions) <- c("survey_value", "conversion")

        assign("crop_price_unit_conversions", crop_price_unit_conversions, envir = .GlobalEnv)
    }



    if ("unitland" %in% unit_list){
        land_unit_conversion <- extract_units_from_db(database,
                                                      url="mongodb://localhost",
                                                      projectID=projectID,
                                                      formID=formID,
                                                      conversion_type="unitland",
                                                      collection="units_and_conversions")

        assign("land_unit_conversion", land_unit_conversion, envir = .GlobalEnv)

    }
    if ("unitland" %in% unit_list==F){
        warning('Tried to find land unit conversions, but could not find records in projectData collection')
        land_unit_conversion <- land_area_units
        colnames(land_unit_conversion) <- c("survey_value", "conversion")

        assign("land_unit_conversion", land_unit_conversion, envir = .GlobalEnv)
    }





    if ("milk_units" %in% unit_list){
        milk_unit_conversion <- extract_units_from_db(database,
                                                      url="mongodb://localhost",
                                                      projectID=projectID,
                                                      formID=formID,
                                                      conversion_type="milk_units",
                                                      collection="units_and_conversions")
        assign("milk_unit_conversion", milk_unit_conversion, envir = .GlobalEnv)

    }
    if ("milk_units" %in% unit_list==F){
        warning('Tried to find land unit conversions, but could not find records in projectData collection')
        milk_unit_conversion <- milk_amount_units
        colnames(milk_unit_conversion) <- c("survey_value", "conversion")

        assign("milk_unit_conversion", milk_unit_conversion, envir = .GlobalEnv)
    }


    if ("milk_sold_price_timeunits" %in% unit_list){
        milk_price_unit_conversion <- extract_units_from_db(database,
                                                            url="mongodb://localhost",
                                                            projectID=projectID,
                                                            formID=formID,
                                                            conversion_type="milk_sold_price_timeunits",
                                                            collection="units_and_conversions")
        assign("milk_price_unit_conversion", milk_price_unit_conversion, envir = .GlobalEnv)

    }
    if ("milk_sold_price_timeunits" %in% unit_list==F){
        warning('Tried to find milk price conversion, but could not find records in projectData collection')
        milk_price_unit_conversion <- milk_price_time_units
        colnames(milk_price_unit_conversion) <- c("survey_value", "conversion")

        assign("milk_price_unit_conversion", milk_price_unit_conversion, envir = .GlobalEnv)
    }



    if ("bees_honey_production_units" %in% unit_list){
        honey_unit_conversion <-  extract_units_from_db(database,
                                                        url="mongodb://localhost",
                                                        projectID=projectID,
                                                        formID=formID,
                                                        conversion_type="bees_honey_production_units",
                                                        collection="units_and_conversions")
        assign("honey_unit_conversion", honey_unit_conversion, envir = .GlobalEnv)

    }
    if ("bees_honey_production_units" %in% unit_list==F){
        warning('Tried to find honey amount conversion, but could not find records in projectData collection')
        honey_unit_conversion <- honey_amount_units
        colnames(honey_unit_conversion) <- c("survey_value", "conversion")

        assign("honey_unit_conversion", honey_unit_conversion, envir = .GlobalEnv)
    }



    if ("eggs_units" %in% unit_list){
        eggs_unit_conversion <- extract_units_from_db(database,
                                                      url="mongodb://localhost",
                                                      projectID=projectID,
                                                      formID=formID,
                                                      conversion_type="eggs_units",
                                                      collection="units_and_conversions")
        assign("eggs_unit_conversion", eggs_unit_conversion, envir = .GlobalEnv)

    }
    if ("eggs_units" %in% unit_list==F){
        warning('Tried to find eggs amount conversion, but could not find records in projectData collection')
        eggs_unit_conversion <- eggs_amount_units
        colnames(eggs_unit_conversion) <- c("survey_value", "conversion")

        assign("eggs_unit_conversion", eggs_unit_conversion, envir = .GlobalEnv)
    }



    if ("eggs_sold_price_timeunits" %in% unit_list){
        eggs_price_unit_conversion <- extract_units_from_db(database,
                                                            url="mongodb://localhost",
                                                            projectID=projectID,
                                                            formID=formID,
                                                            conversion_type="eggs_sold_price_timeunits",
                                                            collection="units_and_conversions")
        assign("eggs_price_unit_conversion", eggs_price_unit_conversion, envir = .GlobalEnv)

    }
    if ("eggs_sold_price_timeunits" %in% unit_list==F){
        warning('Tried to find eggs price conversion, but could not find records in projectData collection')
        eggs_price_unit_conversion <- eggs_price_time_units
        colnames(eggs_price_unit_conversion) <- c("survey_value", "conversion")

        assign("eggs_price_unit_conversion", eggs_price_unit_conversion, envir = .GlobalEnv)
    }




    if ("fertiliser_units" %in% unit_list){
        fertiliser_unit_conversion <- extract_units_from_db(database,
                                                            url="mongodb://localhost",
                                                            projectID=projectID,
                                                            formID=formID,
                                                            conversion_type="fertiliser_units",
                                                            collection="units_and_conversions")
        assign("fertiliser_unit_conversion", fertiliser_unit_conversion, envir = .GlobalEnv)

    }
    if ("fertiliser_units" %in% unit_list==F){
        warning('Tried to find fertiliser amount conversion, but could not find records in projectData collection')
        fertiliser_unit_conversion <- fertiliser_units
        colnames(fertiliser_unit_conversion) <- c("survey_value", "conversion")

        assign("fertiliser_unit_conversion", fertiliser_unit_conversion, envir = .GlobalEnv)
    }

}

#' Extract Units from database
#'
#' Load all of the units from a local mongoDB
#'
#' @param database The name of the database
#' @param url The url of the database
#' @param projectID The id of the project you are interested in
#' @param formID The id of the form you are interested in
#' @param conversion_type The type of conversion factor you are extracting
#' @param collection The collection where these units are found
#'
#' @return
#' @export
#'
#' @examples
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


#' Check Existing Conversion
#'
#' Go through the common conversions stored in the R-package
#'
#' @param list_of_df A list of dataframes, containing all
#' of the units and conversions
#'
#' @return
#' @export
#'
#' @examples
check_existing_conversions <- function(list_of_df){
    list_of_df <- sapply(names(list_of_df), function(x) {

        if (x=="country" & "country" %in% names(list_of_df))
        {
            df_with_existing_conversions <- dplyr::left_join(list_of_df[["country"]],
                                                             country,
                                                             by=("survey_value"="survey_value")) %>%
                dplyr::select("survey_value", "conversion.y") %>%
                dplyr::rename("conversion"="conversion.y")
            return(df_with_existing_conversions)
        }
        if (x=="crop_name")
        {
            crop_name_conv <- tibble::as_tibble(list(
                "survey_value"=crop_name,
                "conversion"=crop_name))

            df_with_existing_conversions <- dplyr::left_join(list_of_df[["crop_name"]],
                                                             crop_name_conv,
                                                             by=("survey_value"="survey_value")) %>%
                dplyr::select("survey_value", "conversion.y") %>%
                dplyr::rename("conversion"="conversion.y")
            return(df_with_existing_conversions)

        }
        if (x=="livestock_name")
        {
            livestock_name_conv <- tibble::as_tibble(list(
                "survey_value"=livestock_name,
                "conversion"=livestock_name))

            df_with_existing_conversions <- dplyr::left_join(list_of_df[["livestock_name"]],
                                                             livestock_name_conv,
                                                             by=("survey_value"="survey_value")) %>%
                dplyr::select("survey_value", "conversion.y") %>%
                dplyr::rename("conversion"="conversion.y")
            return(df_with_existing_conversions)

        }
        if (x=="crop_yield_units")
        {
            crop_yield_units_conv <- tibble::as_tibble(list(
                "survey_value"=crop_yield_units$unit,
                "conversion"=crop_yield_units$conversion))

            df_with_existing_conversions <- dplyr::left_join(list_of_df[["crop_yield_units"]],
                                                             crop_yield_units_conv,
                                                             by=("survey_value"="survey_value")) %>%
                dplyr::select("survey_value", "conversion.y") %>%
                dplyr::rename("conversion"="conversion.y")

            return(df_with_existing_conversions)


        }
        if (x=="crop_sold_price_quantityunits")
        {
            crop_sold_price_quantityunits_conv <- tibble::as_tibble(list(
                "survey_value"=crop_price_units$unit,
                "conversion"=crop_price_units$conversion))

            df_with_existing_conversions <- dplyr::left_join(list_of_df[["crop_sold_price_quantityunits"]],
                                                             crop_sold_price_quantityunits_conv,
                                                             by=("survey_value"="survey_value")) %>%
                dplyr::select("survey_value", "conversion.y") %>%
                dplyr::rename("conversion"="conversion.y")

            return(df_with_existing_conversions)

        }
        if(x=="unitland")
        {
            land_units_conv <- tibble::as_tibble(list(
                "survey_value"=land_area_units$units,
                "conversion"=land_area_units$conversions))

            df_with_existing_conversions <- dplyr::left_join(list_of_df[["unitland"]],
                                                             land_units_conv,
                                                             by=("survey_value"="survey_value")) %>%
                dplyr::select("survey_value", "conversion.y") %>%
                dplyr::rename("conversion"="conversion.y")

            return(df_with_existing_conversions)
        }
        if (x=="milk_units")
        {

            milk_units_conv <- tibble::as_tibble(list(
                "survey_value"=milk_amount_units$unit,
                "conversion"=milk_amount_units$conversion_factor))

            df_with_existing_conversions <- dplyr::left_join(list_of_df[["milk_units"]],
                                                             milk_units_conv,
                                                             by=("survey_value"="survey_value")) %>%
                dplyr::select("survey_value", "conversion.y") %>%
                dplyr::rename("conversion"="conversion.y")

            return(df_with_existing_conversions)

        }
        if (x=="milk_sold_price_timeunits")
        {

            milk_price_units_conv <- tibble::as_tibble(list(
                "survey_value"=milk_price_time_units$unit,
                "conversion"=milk_price_time_units$conversion_factor))

            df_with_existing_conversions <- dplyr::left_join(list_of_df[["milk_sold_price_timeunits"]],
                                                             milk_price_units_conv,
                                                             by=("survey_value"="survey_value")) %>%
                dplyr::select("survey_value", "conversion.y") %>%
                dplyr::rename("conversion"="conversion.y")

            return(df_with_existing_conversions)

        }
        if (x=="bees_honey_production_units")
        {
            bees_honey_units_conv <- tibble::as_tibble(list(
                "survey_value"=honey_amount_units$units,
                "conversion"=honey_amount_units$conversion_factors))

            df_with_existing_conversions <- dplyr::left_join(list_of_df[["bees_honey_production_units"]],
                                                             bees_honey_units_conv,
                                                             by=("survey_value"="survey_value")) %>%
                dplyr::select("survey_value", "conversion.y") %>%
                dplyr::rename("conversion"="conversion.y")

            return(df_with_existing_conversions)

        }
        if (x=="eggs_units")
        {
            eggs_amount_units_conv <- tibble::as_tibble(list(
                "survey_value"=eggs_amount_units$unit,
                "conversion"=eggs_amount_units$conversion_factor))

            df_with_existing_conversions <- dplyr::left_join(list_of_df[["eggs_units"]],
                                                             eggs_amount_units_conv,
                                                             by=("survey_value"="survey_value")) %>%
                dplyr::select("survey_value", "conversion.y") %>%
                dplyr::rename("conversion"="conversion.y")

            return(df_with_existing_conversions)

        }
        if (x=="eggs_sold_price_timeunits")
        {
            eggs_sold_units_conv <- tibble::as_tibble(list(
                "survey_value"=eggs_price_time_units$unit,
                "conversion"=eggs_price_time_units$conversion_factor))

            df_with_existing_conversions <- dplyr::left_join(list_of_df[["eggs_sold_price_timeunits"]],
                                                             eggs_sold_units_conv,
                                                             by=("survey_value"="survey_value")) %>%
                dplyr::select("survey_value", "conversion.y") %>%
                dplyr::rename("conversion"="conversion.y")

            return(df_with_existing_conversions)

        }
        if (x=="fertiliser_units")
        {

            fertiliser_units_conv <- tibble::as_tibble(list(
                "survey_value"=fertiliser_units$unit,
                "conversion"=fertiliser_units$conversion))

            df_with_existing_conversions <- dplyr::left_join(list_of_df[["fertiliser_units"]],
                                                             fertiliser_units_conv,
                                                             by=("survey_value"="survey_value")) %>%
                dplyr::select("survey_value", "conversion.y") %>%
                dplyr::rename("conversion"="conversion.y")

            return(df_with_existing_conversions)

        }
    }, simplify = F)

    return(list_of_df)
}


#' Write Units to Folder
#'
#' Write all of the units and new names to a
#' folder where they can be checked and converted locally
#'
#' @param list_of_df A list of dataframes, containing all
#' of the units and conversions
#'
#' @return
#' @export
#'
#' @examples
write_units_to_folder <- function(list_of_df){
    dir.create("./unit_conversions", showWarnings = F)

    sapply(names(list_of_df), function(x) {
        file_path <- paste0("./unit_conversions/",x,".csv")
        readr::write_csv(list_of_df[[x]], file_path)
    })


}


#' Load local units
#'
#' Load units for a particular project from csv and
#' load them into the global environment
#'
#' @param file_names A list of file names to load
#'
#' @return
#' @export
#'
#' @examples
load_local_units <- function(file_names){


    if ("country.csv" %in% file_names){
        country_conversions <- readr::read_csv("./unit_conversions/country.csv", col_types = readr::cols())
        assign("country_conversions", country_conversions, envir = .GlobalEnv)
    }
    if ("country.csv" %in% file_names==F){
        warning('Tried to find country name conversions, but could not find file in "./unit_conversions" folder')
        country_conversions <- country

        assign("country_conversions", country_conversions, envir = .GlobalEnv)
    }


    if ("crop_name.csv" %in% file_names){
        crop_name_conversions <- readr::read_csv("./unit_conversions/crop_name.csv", col_types = readr::cols())
        assign("crop_name_conversions", crop_name_conversions, envir = .GlobalEnv)

    }
    if ("crop_name.csv" %in% file_names==F){
        warning('Tried to find crop name conversions, but could not find file in "./unit_conversions" folder')
        crop_name_conversions <- tibble::as_tibble(list("survey_value"=crop_name, "conversion"=crop_name))
        assign("crop_name_conversions", crop_name_conversions, envir = .GlobalEnv)

    }


    if ("livestock_name.csv" %in% file_names){
        livestock_name_conversions <- readr::read_csv("./unit_conversions/livestock_name.csv", col_types = readr::cols())
        assign("livestock_name_conversions", livestock_name_conversions, envir = .GlobalEnv)

    }
    if ("livestock_name.csv" %in% file_names==F){
        warning('Tried to find livestock name conversions, but could not find file in "./unit_conversions" folder')
        livestock_name_conversions <- tibble::as_tibble(list("survey_value"=livestock_name, "conversion"=livestock_name))

        assign("livestock_name_conversions", livestock_name_conversions, envir = .GlobalEnv)
    }


    if ("crop_yield_units.csv" %in% file_names){
        crop_yield_unit_conversions <- readr::read_csv("./unit_conversions/crop_yield_units.csv", col_types = readr::cols())
        assign("crop_yield_unit_conversions", crop_yield_unit_conversions, envir = .GlobalEnv)

    }
    if ("crop_yield_units.csv" %in% file_names==F){
        warning('Tried to find crop yield unit conversions, but could not find file in "./unit_conversions" folder')
        crop_yield_unit_conversions <- crop_yield_units
        colnames(crop_yield_unit_conversions) <- c("survey_value", "conversion")

        assign("crop_yield_unit_conversions", crop_yield_unit_conversions, envir = .GlobalEnv)
    }



    if ("crop_sold_price_quantityunits.csv" %in% file_names){
        crop_price_unit_conversions <- readr::read_csv("./unit_conversions/crop_sold_price_quantityunits.csv", col_types = readr::cols())
        assign("crop_price_unit_conversions", crop_price_unit_conversions, envir = .GlobalEnv)

    }
    if ("crop_sold_price_quantityunits.csv" %in% file_names==F){
        warning('Tried to find crop price unit conversions, but could not find file in "./unit_conversions" folder')
        crop_price_unit_conversions <- crop_price_units
        colnames(crop_price_unit_conversions) <- c("survey_value", "conversion")

        assign("crop_price_unit_conversions", crop_price_unit_conversions, envir = .GlobalEnv)
    }



    if ("unitland.csv" %in% file_names){
        land_unit_conversion <- readr::read_csv("./unit_conversions/unitland.csv", col_types = readr::cols())
        assign("land_unit_conversion", land_unit_conversion, envir = .GlobalEnv)

    }
    if ("unitland.csv" %in% file_names==F){
        warning('Tried to find land unit conversions, but could not find file in "./unit_conversions" folder')
        land_unit_conversion <- land_area_units
        colnames(land_unit_conversion) <- c("survey_value", "conversion")

        assign("land_unit_conversion", land_unit_conversion, envir = .GlobalEnv)
    }





    if ("milk_units.csv" %in% file_names){
        milk_unit_conversion <- readr::read_csv("./unit_conversions/milk_units.csv", col_types = readr::cols())
        assign("milk_unit_conversion", milk_unit_conversion, envir = .GlobalEnv)

    }
    if ("milk_units.csv" %in% file_names==F){
        warning('Tried to find land unit conversions, but could not find file in "./unit_conversions" folder')
        milk_unit_conversion <- milk_amount_units
        colnames(milk_unit_conversion) <- c("survey_value", "conversion")

        assign("milk_unit_conversion", milk_unit_conversion, envir = .GlobalEnv)
    }


    if ("milk_sold_price_timeunits.csv" %in% file_names){
        milk_price_unit_conversion <- readr::read_csv("./unit_conversions/milk_sold_price_timeunits.csv", col_types = readr::cols())
        assign("milk_price_unit_conversion", milk_price_unit_conversion, envir = .GlobalEnv)

    }
    if ("milk_sold_price_timeunits.csv" %in% file_names==F){
        warning('Tried to find milk price conversion, but could not find file in "./unit_conversions" folder')
        milk_price_unit_conversion <- milk_price_time_units
        colnames(milk_price_unit_conversion) <- c("survey_value", "conversion")

        assign("milk_price_unit_conversion", milk_price_unit_conversion, envir = .GlobalEnv)
    }



    if ("bees_honey_production_units.csv" %in% file_names){
        honey_unit_conversion <- readr::read_csv("./unit_conversions/bees_honey_production_units.csv", col_types = readr::cols())
        assign("honey_unit_conversion", honey_unit_conversion, envir = .GlobalEnv)

    }
    if ("bees_honey_production_units.csv" %in% file_names==F){
        warning('Tried to find honey amount conversion, but could not find file in "./unit_conversions" folder')
        honey_unit_conversion <- honey_amount_units
        colnames(honey_unit_conversion) <- c("survey_value", "conversion")

        assign("honey_unit_conversion", honey_unit_conversion, envir = .GlobalEnv)
    }



    if ("eggs_units.csv" %in% file_names){
        eggs_unit_conversion <- readr::read_csv("./unit_conversions/eggs_units.csv", col_types = readr::cols())
        assign("eggs_unit_conversion", eggs_unit_conversion, envir = .GlobalEnv)

    }
    if ("eggs_units.csv" %in% file_names==F){
        warning('Tried to find eggs amount conversion, but could not find file in "./unit_conversions" folder')
        eggs_unit_conversion <- eggs_amount_units
        colnames(eggs_unit_conversion) <- c("survey_value", "conversion")

        assign("eggs_unit_conversion", eggs_unit_conversion, envir = .GlobalEnv)
    }



    if ("eggs_sold_price_timeunits.csv" %in% file_names){
        eggs_price_unit_conversion <- readr::read_csv("./unit_conversions/eggs_sold_price_timeunits.csv", col_types = readr::cols())
        assign("eggs_price_unit_conversion", eggs_price_unit_conversion, envir = .GlobalEnv)

    }
    if ("eggs_sold_price_timeunits.csv" %in% file_names==F){
        warning('Tried to find eggs price conversion, but could not find file in "./unit_conversions" folder')
        eggs_price_unit_conversion <- eggs_price_time_units
        colnames(eggs_price_unit_conversion) <- c("survey_value", "conversion")

        assign("eggs_price_unit_conversion", eggs_price_unit_conversion, envir = .GlobalEnv)
    }



    if ("fertiliser_units.csv" %in% file_names){
        fertiliser_unit_conversion <- readr::read_csv("./unit_conversions/fertiliser_units.csv", col_types = readr::cols())
        assign("fertiliser_unit_conversion", fertiliser_unit_conversion, envir = .GlobalEnv)

    }
    if ("fertiliser_units.csv" %in% file_names==F){
        warning('Tried to find fertiliser amount conversion, but could not find file in "./unit_conversions" folder')
        fertiliser_unit_conversion <- fertiliser_units
        colnames(fertiliser_unit_conversion) <- c("survey_value", "conversion")

        assign("fertiliser_unit_conversion", fertiliser_unit_conversion, envir = .GlobalEnv)
    }




}


#' Find db Units
#'
#' The names of the units stored in the RHoMIS data base
#' are stored within the "projectData" collection. This
#' function lets you query which types of units were found
#' for a particular project
#'
#'
#' @param url The URL of the mongoDB you are querying
#' @param collection The collection storing administrative project information
#' @param database The name of the database containing the information
#' @param projectID The name of the project
#' @param formID The name of the form
#'
#' @return
#' @export
#'
#' @examples
find_db_units <- function(projectID,
                          formID,
                          url,
                          collection,
                          database){


        connection <- mongolite::mongo(collection="projectData",
                                       db=database,
                                       url=url)

        units <- connection$find(query = paste0('{"projectID":"',projectID,'", "formID":"',formID,'"}'),
                                 fields = '{"units":1, "_id":0}')

        units <- unlist(units)
        units <- unname(units)


        connection$disconnect()

        return(units)

}



