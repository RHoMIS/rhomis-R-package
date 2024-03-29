


#' Extract Secondary Units
#'
#' In the second processing step
#' of RHoMIS, there are secondary units
#' which need to be verified (e.g. livestock_tlu),
#' this function is used to extract these types
#' of units and search for existing conversions
#' within the package (see R/config_package_vars.R "set_secondary_units()").
#'
#' file: R/UnitsAndConversions.R
#'
#' @param units_and_conversions A list of unit and conversion tibbles
#'
#' @return
#' @export
#'
#' @examples
extract_secondary_units <- function(units_and_conversions){



    secondary_units_conversions_and_prices <- lapply(c(1:length(pkg.env$secondary_units)), function(secondary_unit_index){
        unit_type <- names(pkg.env$secondary_units)[secondary_unit_index]
        name_conversions <- as.character(pkg.env$secondary_units[secondary_unit_index])

        # paste0("Unit type is: ", unit_type, "\n Name conversion is: ", name_conversions)

        result <- extract_secondary_unit_set(
            units_and_conversions=units_and_conversions,
            name_conversions=name_conversions,
            unit_type=unit_type
        )

        result <- result[duplicated(result)==F,]
        return(result)

    })

    names(secondary_units_conversions_and_prices) <- names(pkg.env$secondary_units)
    return(secondary_units_conversions_and_prices)
}

#' Extract Secondary Unit Set
#'
#' In the second processing step
#' of RHoMIS, there are secondary units
#' which need to be verified (e.g. livestock_tlu),
#' this function is used to extract these types
#' of units and search for existing conversions
#' within the package (see R/config_package_vars.R "set_secondary_units()").
#'
#' file: R/UnitsAndConversions.R
#'
#' @param units_and_conversions A list of units conversion tibbles
#' @param name_conversions The names for the conversions (e.g. livestock_name)
#' @param unit_type The type of unit we are preparing to right to conversion
#'
#' @return
#' @export
#'
#' @examples
extract_secondary_unit_set <- function(
        units_and_conversions,
        name_conversions,
        unit_type


){

    # units_and_conversions <- units_and_conversions
    # name_conversions <- "livestock_name_to_std"
    # unit_type <- "livestock_count_to_tlu"




    # Adding Livestock Related Conversion Tables
    if (name_conversions %in% names(units_and_conversions)){
        temp_df <- units_and_conversions[[name_conversions]]

        temp_df <- temp_df[c("id_rhomis_dataset", "conversion")]
        if (nrow(temp_df[!is.na(units_and_conversions[[name_conversions]][["conversion"]]), c("id_rhomis_dataset", "conversion")])!=0){
            temp_df <- temp_df[!is.na(units_and_conversions[[name_conversions]][["conversion"]]), c("id_rhomis_dataset", "conversion")]
        }

        temp_df$index <- c(1:nrow(temp_df))
        temp_df <- temp_df %>%
            merge(get(unit_type), by.x ="conversion", by.y = "survey_value", sort=F, all.x = T) %>%
            dplyr::arrange(index)
        temp_df$unit_type <- unit_type
        temp_df <- temp_df %>%
            dplyr::rename(survey_value=conversion) %>%
            dplyr::rename(conversion=conversion.y) %>%
            dplyr::select(-index)

        temp_df <- temp_df[c("unit_type","id_rhomis_dataset", "survey_value", "conversion")]
        return(temp_df)
    }






}



#' Saving Set of Conversions
#'
#' Save a single type of conversion factor into the conversion factor database
#'
#' Rpackage file: UnitsAndConversions.R
#'
#' @param database The database which will store the units
#' @param url The url of the database storing the units
#' @param projectID The name of the project the units are related to
#' @param formID The name of the form the projects are related to
#' @param conversions A table of the conversion factors to be used
#' @param conversion_type Which type of conversion is being saves (e.g crop yield, PPI, HDDS etc...)
#' @param collection The mongodb collection to save the units
#' @param converted_values Whether saving values which user has already converted
#'
#' @return
#' @export
#'
#' @examples
save_set_of_conversions <- function(database = "rhomis", url = "mongodb://localhost",
                                    projectID = "core_unit",
                                    formID = "core_unit",
                                    conversions,
                                    conversion_type,
                                    collection,
                                    converted_values=F) {

    # conversions <- crop_calories
    # conversion_type <- "crop_calories"
    # database <- "rhomis-data-dev"

    # formID <- 'form1'
    # projectID <- 'project xy leo laptop'
    connection <- connect_to_db(collection, database, url)

    if (converted_values==T){


        old_conversions <- extract_units_from_db(database,
                                                 url = url,
                                                 projectID = projectID,
                                                 formID = formID,
                                                 conversion_type = conversion_type,
                                                 collection = collection
        )

        if (nrow(old_conversions)>0){
            if(all(c("unit_type", "id_rhomis_dataset", "survey_value", "conversion") %in% colnames(old_conversions))){

                conversions <- dplyr::left_join(conversions,
                                                old_conversions,
                                                by = c("survey_value" = "survey_value",
                                                       "id_rhomis_dataset"="id_rhomis_dataset")) %>%
                    dplyr::select("unit_type.x", "id_rhomis_dataset", "survey_value", "conversion.y") %>%
                    dplyr::rename("conversion" = "conversion.y") %>%
                    dplyr::rename("unit_type" = "unit_type.x")

            }
        }
    }


    conversion_data <- jsonlite::toJSON(conversions, na = "null") %>% clean_json_string()

    insert_query <- paste0('{"projectID":"', projectID, '","formID":"', formID, '","conversionType":"', conversion_type, '"}')
    set_query <- paste0('{"$set":{', '"data":', conversion_data, "}}")
    connection$update(insert_query, set_query, upsert = T)

    connection$disconnect()

    connection <- connect_to_db("projectData", database, url)
    insert_query <- paste0('{"projectID":"', projectID, '","formID":"', formID, '"}')
    set_query <- paste0('{"$addToSet":{"units":', '"', conversion_type, '"}}')

    connection$update(insert_query, set_query, upsert = T)


    connection$disconnect()
}


#' Save Multiple conversions
#'
#' Rpackage file: UnitsAndConversions.R
#'
#' @param database The database which will store the units
#' @param url The url of the database storing the units
#' @param projectID The name of the project the units are related to
#' @param formID The name of the form the projects are related to
#' @param conversion_data A list of tibbles of the conversion factors to be used
#' @param conversion_types Which types of conversion is being saves (e.g crop yield, PPI, HDDS etc...)
#' @param collection The mongodb collection to save the units
#' @param converted_values Whether saving values which user has already converted
#'
#'
#'
#' @return
#' @export
#'
#' @examples
save_multiple_conversions <- function(database = "rhomis", url = "mongodb://localhost",
                                      projectID = "core_units",
                                      formID = "core_units",
                                      conversion_data,
                                      conversion_types,
                                      collection,
                                      converted_values=F) {
    if (length(conversion_data) != length(conversion_types)) {
        stop("Length of conversion data argument not the same as the names for conversion data")
    }

    for (index in 1:length(conversion_data)) {
        save_set_of_conversions(
            database, url,
            projectID,
            formID,
            conversion_data[[index]],
            conversion_types[index],
            collection,
            converted_values
        )
    }
}


#' Save Initial Units
#'
#' Rpackage file: UnitsAndConversions.R
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
save_initial_units <- function(database = "rhomis", url = "mongodb://localhost",
                               projectID = "core_units",
                               formID = "core_units",
                               units_and_conversions = list(
                                   "crop_price_units" = crop_price_units,
                                   "crop_yield_units" = crop_yield_units,
                                   "eggs_amount_units" = eggs_amount_units,
                                   "eggs_price_time_units" = eggs_price_time_units,
                                   "honey_amount_units" = honey_amount_units,
                                   "land_area_units" = land_area_units,
                                   "livestock_weights" = livestock_weights,
                                   "milk_amount_units" = milk_amount_units,
                                   "milk_price_time_units" = milk_price_time_units,
                                   "ppi_limits" = ppi_limits,
                                   "ppi_score_card" = ppi_score_card,
                                   "proportion_conversions" = proportion_conversions
                               )) {
    save_multiple_conversions(
        database,
        url,
        projectID,
        formID,
        units_and_conversions,
        names(units_and_conversions)
    )
}


#' Load all db units
#'
#' Load all units from a local MongoDB databases
#'
#' Rpackage file: UnitsAndConversions.R
#'
#' @param database The database you are querying units from
#' @param projectID The Name of the project
#' @param formID The name of the form
#' @param unit_list The list of units which are to be queried and loaded into the global environment
#' @param id_rhomis_dataset A vector of rhomis IDs for the project being processed.
#' @return
#' @export
#'
#' @examples
load_all_db_units <- function(unit_list, database = "rhomis", projectID = "core_units", formID = "core_units", id_rhomis_dataset) {


    units_and_conversions <- list()
    # loop over the possible list of unit conversion file names



    for (unit_name in pkg.env$unit_file_names) {

        if (as.character(unit_name) %in% unit_list) {
            conversions <- extract_units_from_db(database,
                                                 url = "mongodb://localhost",
                                                 projectID = projectID,
                                                 formID = formID,
                                                 conversion_type = as.character(unit_name),
                                                 collection = "units_and_conversions"
            )

            conversions[conversions$conversion=="NA" & !is.na(conversions$conversion),"conversion"] <- NA

        } else {
            warning(paste("Tried to find ", unit_name, " conversions, but could not find records in projectData collection"))

            if (unit_name %in% c("crop_name_to_std", "livestock_name_to_std")) {
                # evaluate the string denoting the variable name to be used
                var <- eval(parse(text = paste0("rhomis::",unit_name)))

                # make dummy tibble
                conversions <- tibble::as_tibble(list("survey_value" = var, "conversion" = var))
            } else {
                conversions <- make_per_project_conversion_tibble(
                    proj_id_vector = id_rhomis_dataset,
                    unit_conv_tibble = eval(parse(text = paste0("rhomis::",unit_name)))
                )
                conversions$unit_type <- unit_name

                if (!(unit_name == "country_to_iso2")) {
                    colnames(conversions) <- c("survey_value", "conversion", "id_rhomis_dataset", "unit_type")
                }
            }
        }

        if (all(c("id_rhomis_dataset","survey_value") %in% colnames(conversions))){
        conversions <- conversions[!duplicated(conversions[c( "id_rhomis_dataset","survey_value")]),]
        }
        units_and_conversions[[unit_name]] <- conversions
        # assign(, conversions, envir = pkg.env)
    }


    return(units_and_conversions)

}

#' Extract Units from database
#'
#' Load all of the units from a local mongoDB
#'
#' Rpackage file: UnitsAndConversions.R
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
extract_units_from_db <- function(database = "rhomis",
                                  url = "mongodb://localhost",
                                  projectID = "core_units",
                                  formID = "core_units",
                                  conversion_type,
                                  collection = "units_and_conversions") {
    connection <- connect_to_db(collection, database, url)



    # Arguments to identify the relevant project
    match_arguments <- jsonlite::toJSON(list(
        "projectID" = projectID,
        "formID" = formID,
        "conversionType" = conversion_type
    ), na = "null")
    match_arguments <- gsub("[", "", match_arguments, fixed = T)
    match_arguments <- gsub("]", "", match_arguments, fixed = T)
    match_arguments <- paste0('{"$match":', match_arguments, "}")
    match_arguments <- clean_json_string(match_arguments)

    # Unwind the subarray into the value of interest
    unwind_arguments <- paste0('{"$unwind": "$data"}')

    # Manage the projection (which values are included or not)
    project_arguments <- jsonlite::toJSON(list(
        "_id" = 0,
        "formID" = 0,
        "projectID" = 0,
        "conversionType" = 0
    ),
    na = "null"
    )
    project_arguments <- gsub("[", "", project_arguments, fixed = T)
    project_arguments <- gsub("]", "", project_arguments, fixed = T)
    project_arguments <- paste0('{"$project":', project_arguments, "}")


    # Unwind the subarray into the value of interest
    unwind_data <- paste0('{"$unwind": {"path":"$data", "preserveNullAndEmptyArrays":true}}')

    # Group data

    # e.g
    # [{"$match":{"projectID":"core_unit","formID":"core_unit","conversionType":"ppi_score_card"}},{"$unwind": "$data"},{"$project":{"_id":0,"formID":0,"projectID":0,"conversionType":0}},{"$unwind": {"path":"$data", "preserveNullAndEmptyArrays":true}}]
    pipeline <- paste0("[", match_arguments, ",", unwind_arguments, ",", project_arguments, ",", unwind_data, "]")

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
#' Rpackage file: UnitsAndConversions.R
#'
#' @param list_of_df A list of dataframes, containing all
#' of the units and conversions
#'
#' @return
#' @export
#'
#' @examples
check_existing_conversions <- function(list_of_df) {
    new_list <- sapply(names(list_of_df), function(x) {
        if (x %in% c("crop_name_to_std", "livestock_name_to_std")) {
            conversion <- tibble::as_tibble(list(
                "survey_value" = eval(parse(text = paste0("rhomis::",x))),
                "conversion" = eval(parse(text = paste0("rhomis::",x)))
            ))

            df_with_existing_conversions <- dplyr::left_join(list_of_df[[x]],
                                                             conversion,
                                                             by = ("survey_value" = "survey_value")
            ) %>%
                dplyr::select("unit_type", "id_rhomis_dataset", "survey_value", "conversion.y") %>%
                dplyr::rename("conversion" = "conversion.y")
        } else {
            df_with_existing_conversions <- dplyr::left_join(list_of_df[[x]],
                                                             eval(parse(text = paste0("rhomis::",x))),
                                                             by = ("survey_value" = "survey_value")
            ) %>%
                dplyr::select("unit_type", "id_rhomis_dataset", "survey_value", "conversion.y") %>%
                dplyr::rename("conversion" = "conversion.y")
        }

        return(df_with_existing_conversions)
    }, simplify = F)

    return(new_list)
}




#' Check Existing Crop Calorie Conversion
#'
#' Go through the common conversions stored in the R-package
#'
#' Rpackage file: UnitsAndConversions.R
#'
#' @param data RHoMIS Dataset
#'
#' @return
#' @export
#'
#' @examples
check_existing_calorie_conversions <- function(data) {
    list_of_dfs <- extract_calorie_values_by_project(data)

    new_list <- sapply(names(list_of_dfs), function(x) {
        df_with_existing_conversions <- dplyr::left_join(list_of_dfs[[x]],
                                                         eval(parse(text = paste0("rhomis::",x))),
                                                         by = ("survey_value" <- "survey_value")
        ) %>%
            dplyr::select("unit_type", "id_rhomis_dataset", "survey_value", "conversion.y") %>%
            dplyr::rename("conversion" = "conversion.y")
        return(df_with_existing_conversions)
    }, simplify = F)

    return(new_list)
}



#' Write Units to Folder
#'
#' Write all of the units and new names to a
#' folder where they can be checked and converted locally
#'
#' Rpackage file: UnitsAndConversions.R
#'
#' @param list_of_df A list of dataframes, containing all
#' of the units and conversions
#' @param folder The folder for writing the units
#' @param converted_folder A flag indicating whether the
#' units being over written have already been converted
#' by the user. In which case, preserve their previously
#' verified values.
#'
#'
#'
#' @return
#' @export
#'
#' @examples
write_units_to_folder <- function(list_of_df,
                                  folder = "./unit_conversions",
                                  converted_folder = F) {
    dir.create(folder, showWarnings = F)

    sapply(names(list_of_df), function(x) {
        file_path <- paste0(folder, "/", x, ".csv")

        data_to_write <- list_of_df[[x]]

        if(converted_folder){
            if (file.exists(file_path)){
                old_conversion_file <- readr::read_csv(file_path,
                                                       col_types = readr::cols(),
                                                       na = c("n/a", "-999", "NA"),
                                                       locale = readr::locale(encoding = "UTF8"),
                                                       show_col_types=F
                )

                data_to_write <- data_to_write %>% dplyr::mutate_all(as.character)
                old_conversion_file<- old_conversion_file %>% dplyr::mutate_all(as.character)

                data_to_write <- dplyr::left_join(data_to_write,
                                                  old_conversion_file,
                                                  by = c("survey_value" = "survey_value",
                                                         "id_rhomis_dataset"="id_rhomis_dataset")) %>%
                    dplyr::select("unit_type.x", "id_rhomis_dataset", "survey_value", "conversion.y") %>%
                    dplyr::rename("conversion" = "conversion.y") %>%
                    dplyr::rename("unit_type" = "unit_type.x")

            }
        }
        readr::write_excel_csv(data_to_write, file_path)
    })
}


#' Load local units
#'
#' Load units for a particular project from csv and
#' load them into the global environment
#'
#' Rpackage file: UnitsAndConversions.R
#'
#' @param id_rhomis_dataset A vector including the ID of the RHoMIS datasets being processed
#' @param units_folder The path to the folder containing the units to load
#' @param unit_type Whether you are reading "primary" or "secondary" units.
#'
#' @return
#' @export
#'
#' @examples
load_local_units <- function(units_folder, id_rhomis_dataset, unit_type="primary") {

    units_and_conversions <- list()

    # get list of files stored in base_folder
    file_names <- list.files(units_folder)

    # loop over the possible list of unit conversion csv file names
    # not needed anymore (all included in pkg.env$unit_file_names)
    # if (unit_type=="primary"){
    #     unit_list <- as.character(pkg.env$unit_file_names)
    # }
    #
    # if (unit_type=="secondary"){
    #     unit_list <- names(pkg.env$secondary_units)
    # }

    for (unit_file in pkg.env$unit_file_names) {

        # check that this list of files exists in the base_folder
        if (paste0(unit_file, ".csv") %in% file_names) {
            conversions <- readr::read_csv(paste0(units_folder, unit_file, ".csv"),
                                           col_types = readr::cols(),
                                           na = c("n/a", "-999", "NA"),
                                           locale = readr::locale(encoding = "UTF8"),
                                           show_col_types=F
            )
        } else {

            # print a warning if the file isn't where it should be
            warning(paste0("Could not locate  ", unit_file, " in ", units_folder))

            # need a catch for these two files, because of the extra step in creating a dummy table
            if (unit_file %in% c("crop_name_to_std", "livestock_name_to_std")) {

                # evaluate the string denoting the variable name to be used
                var <- eval(parse(text = paste0("rhomis::",unit_file)))

                # make dummy tibble
                conversions <- tibble::as_tibble(list("survey_value" = var, "conversion" = var))
                conversions <- make_per_project_conversion_tibble(
                    proj_id_vector = id_rhomis_dataset,
                    unit_conv_tibble = conversions
                )
            } else {
                var <- eval(parse( text = paste0("rhomis::",unit_file)))

                # make dummy tibble
                conversions <- make_per_project_conversion_tibble(
                    proj_id_vector = id_rhomis_dataset,
                    unit_conv_tibble = var
                )
            }
        }

        if (all(c("id_rhomis_dataset","survey_value") %in% colnames(conversions))){
            conversions <- conversions[!duplicated(conversions[c( "id_rhomis_dataset","survey_value")]),]
        }

        # assign conversion to package env
        units_and_conversions[[unit_file]] <- conversions

        # assign(pkg.env$local_units_file_list[[unit_file]], conversions, envir = pkg.env)
    }

    return(units_and_conversions)
}





#' Load Calorie Conversions
#'
#' Load units for a particular project from csv and
#' load them into the global environment
#'
#' Rpackage file: UnitsAndConversions.R
#'
#' @param id_rhomis_dataset A vector including the ID of the RHoMIS datasets being processed
#' @param base_folder The path to the folder containing the units to load
#'
#' @return
#' @export
#'
#' @examples
load_calorie_conversions <- function(base_folder, id_rhomis_dataset) {

    # get list of files stored in base_folder
    file_names <- list.files(base_folder)

    for (produce in produce_group_list) {

        # create file name string to search base_folder file list
        csv_filename <- paste0(produce, "_calories.csv")

        # check if the file exists in list from folder
        if (csv_filename %in% file_names) {

            # load the conversion csv file
            calorie_conversion <- readr::read_csv(paste0(base_folder, csv_filename),
                                                  col_types = readr::cols(),
                                                  na = c("n/a", "-999", "NA"),
                                                  locale = readr::locale(encoding = "UTF8"),
                                                  show_col_types=F
            )
        } else {

            # print a warning if the file isn't where expected
            warning(paste0("Could not locate  ", cvs_filename, " in ", base_folder))

            # create conversion tibble
            calorie_conversion <- make_per_project_conversion_tibble(
                proj_id_vector = id_rhomis_dataset,
                unit_conv_tibble = eval(parse(text = paste0("rhomis::",produce, "_calories")))
            )
        }

        # assign to the global environment
        assign(paste0(produce, "_calorie_conversion"), calorie_conversion, envir = .GlobalEnv)
    }

    return()
}




#' Find db Units
#'
#' The names of the units stored in the RHoMIS data base
#' are stored within the "projectData" collection. This
#' function lets you query which types of units were found
#' for a particular project
#'
#' Rpackage file: UnitsAndConversions.R
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
                          database) {


    connection <- mongolite::mongo(
        collection = "projectData",
        db = database,
        url = url
    )

    units <- connection$find(
        query = paste0('{"projectID":"', projectID, '", "formID":"', formID, '"}'),
        fields = '{"units":1, "_id":0}'
    )

    units <- unlist(units)
    units <- unname(units)


    connection$disconnect()

    return(units)
}
