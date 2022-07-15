




#' Connect to db
#'
#' A simple function for connecting to a mongo database
#'   
#' Rpackage file: utils-mongodb.R
#'
#' @param collection Which collection you want to connect to
#' @param database Which database you want to connect to
#' @param url The URL of the database you want to connect to
#'
#' @return Returns a connection object
#' @export
#'
#' @examples
connect_to_db <- function(collection, database = "rhomis", url = "mongodb://localhost") {
  mongodb <- mongolite::mongo(
    collection = collection,
    db = database,
    url = "mongodb://localhost"
  )
  return(mongodb)
}


#' Data frame to JSON
#'
#' To write objects to a mongoDB, they need to be in JSON format.
#' This function converts data from a data frame to a JSON
#'   
#' Rpackage file: utils-mongodb.R
#'
#' @param data_to_change The data frame that needs to be converted
#'
#' @return A JSON string
#' @export
#'
#' @examples
#' sample_data_frame <- tibble::as_tibble(list(
#'   "original_spelling" = c("benana", "maz", "wetermalon", "cokonut"),
#'   "standardised_spelling" = c("benana", NA, "wetermalon", "cokonut")
#' ))
#' data_frame_to_json(sample_data_frame)
#'
data_frame_to_json <- function(data_to_change) {
  json_copy <- jsonlite::toJSON(data_to_change, na = "null")
  return(as.character(json_copy))
}


#' Querying whole collection
#'
#' Returning all of the information from a mongoDB query in a tabular format
#'   
#' Rpackage file: utils-mongodb.R
#'
#' @param collection The collection you want to collect the information from
#' @param database The database you are querying
#' @param url The url of the database you are querying
#'
#' @return A data frame of the data you are querying
#' @export
#'
#' @examples
find_collection <- function(collection, database = "rhomis", url = "mongodb://localhost") {
  connection <- connect_to_db(collection, database = database, url = url)
  data <- connection$find("{}")
  connection$disconnect()
  return(data)
}

#' Count collection
#'
#' Count the number of documents in a collection
#'   
#' Rpackage file: utils-mongodb.R
#'
#' @param collection The collection you want to collect the information from
#' @param database The database you are querying
#' @param url The url of the database you are querying
#'
#' @return
#' @export
#'
#' @examples
count_collection <- function(collection, database = "rhomis", url = "mongodb://localhost") {
  connection <- connect_to_db(collection, database = database, url = url)
  count <- connection$count("{}")
  connection$disconnect()
  return(count)
}



#' Write a New Collection
#'
#' Write a new table to a RHoMIS db database
#'   
#' Rpackage file: utils-mongodb.R
#'
#' @param data_to_write The dataframe that you want to write to file
#' @param collection The collection you want to collect the information from
#' @param database The database you are querying
#' @param url The url of the database you are querying
#'
#' @return
#' @export
#'
#' @examples
write_new_collection <- function(data_to_write, collection, database = "rhomis", url = "mongodb://localhost") {
  previous_data <- find_collection(collection, database, url)
  if (nrow(previous_data) > 0) {
    stop(paste0("Collection '", collection, "' exists already"))
  }

  connection <- connect_to_db(collection, database, url)

  # Insert the data frame
  connection$insert(data_to_write)
  print("Success in creating new table")
}


update_collection <- function(data_to_write, collection, database = "rhomis", url = "mongodb://localhost") {
  previous_data <- find_collection(collection, database, url)
  if (nrow(previous_data) == 0) {
    stop(paste0("Collection '", collection, "'does not exist yet"))
  }

  connection <- connect_to_db(collection, database, url)

  # Insert the data frame
  connection$insert(data_to_write)
  print("Success in updating table")
  connection$disconnect()
}

#' Add Data to Project List
#'
#' A method for submitting another project
#' to a list of projects.
#'   
#' Rpackage file: utils-mongodb.R
#'
#' @param data Data to add
#' @param collection Collection to add it to
#' @param database Database to add it to
#' @param url URL of the database
#' @param projectID ID of the project you are adding
#' @param formID The id of the form being you are adding
#' @param overwrite Whether or not to overwrite the project
#'
#' @return
#' @export
#'
#' @examples
add_data_to_project_list <- function(data, collection, database = "rhomis", url = "mongodb://localhost", projectID, formID, overwrite = F) {
  data_string <- jsonlite::toJSON(data, pretty = T, na = "null")
  connection <- connect_to_db(collection, database, url)

  if (overwrite == F) {
    data_string <- paste0('{"projectID":"', projectID, '","formID":', formID, ', "data"', ":", data_string, "}")
    data_string <- gsub("\n", "", data_string, fixed = T)
    data_string <- gsub('\\"', '"', data_string, fixed = T)
    data_string <- gsub('"\\', '"', data_string, fixed = T)

    connection$insert(data_string)
  }

  if (overwrite == T) {
    connection$update(paste0('{"projectID":"', projectID, '","formID":"', formID, '"}'),
      paste0('{"$set":{"data": ', data_string, "}}"),
      upsert = TRUE
    )
  }

  connection$disconnect()
}


#' Add data to db
#'   
#' Rpackage file: utils-mongodb.R
#'
#' @param data The tibble dataset that you would like to write
#' @param collection The collection which you would like to add the data to
#' @param database The database where the data is going to be saved
#' @param url The url of the database you are saving the database to. The default mongoDB url is used as the default here
#' @param projectID The ID of the project you would like to save
#' @param formID The ID of the form you would like to save
#' @param overwrite Whether or not to overwrite a previously saved version of the project
#' @param data_type The type of data which is being added (e.g. indicatorData, processedData,metaData...)
#'
#' @return
#' @export
#'
#' @examples
add_data_to_db <- function(data, collection = "data", data_type, database = "rhomis", url = "mongodb://localhost", projectID, formID, overwrite = F) {
  data_string <- jsonlite::toJSON(data, pretty = T, na = "null")
  connection <- connect_to_db(collection, database, url)


  if (overwrite == F) {
    data_string <- paste0('{"projectID":"', projectID, '","formID":', formID, '"dataType":', data_type, ', "data"', ":", data_string, "}")
    data_string <- gsub("\n", "", data_string, fixed = T)
    data_string <- gsub('\\"', '"', data_string, fixed = T)
    data_string <- gsub('"\\', '"', data_string, fixed = T)

    connection$insert(data_string)
  }

  if (overwrite == T) {
    connection$update(paste0('{"projectID":"', projectID, '","formID":"', formID, '"}'),
      paste0('{"$set":{"data": ', data_string, "}}"),
      upsert = TRUE
    )
  }

  connection$disconnect()
}


#' Add Project to List
#'
#' After conducting the main RHoMIS calculations, it is important to add the
#' form ID and project ID to the list of projects in the database, to keep track of the
#' projects using RHoMIS. This function allows us to do this.
#'   
#' Rpackage file: utils-mongodb.R
#'
#' @param database Database to add it to
#' @param url URL of the database
#' @param projectID ID of the project you are adding
#' @param formID The id of the form being you are adding
#'
#' @return
#' @export
#'
#' @examples
adding_project_to_list <- function(database = "rhomis", url = "mongodb://localhost", projectID, formID) {

  # collection, database="rhomis", url="mongodb://localhost",projectID,formID

  connection <- connect_to_db("projectData", database, url)

  connection$update(paste0('{"projectID":"', projectID, '","formID":"', formID, '"}'),
    paste0('{"$set":{"projectID": ', '"', projectID, '", "formID":"', formID, '"}}'),
    upsert = TRUE
  )
  connection$disconnect()
}

#' Set Project Tag to True
#' 
#' RHoMIS data goes through several
#' processing stages. Where units are
#' extracted, then verified, then 
#' prices verified. This function allows
#' us to tick off these stages, letting
#' the system know what has been verified and what has not.
#'    
#' Rpackage file: utils-mongodb.R
#'
#' @param database Database to add it to
#' @param url URL of the database
#' @param projectID ID of the project you are adding
#' @param formID The id of the form being you are adding
#' @param project_tag Which project tag needs to be set to true
#'
#' @return
#' @export
#'
#' @examples

set_project_tag_to_true <- function(database = "rhomis", url = "mongodb://localhost", projectID, formID, project_tag) {


  connection <- connect_to_db("projectData", database, url)

  connection$update(paste0('{"projectID":"', projectID, '","formID":"', formID, '"}'),
    paste0('{"$set":{"', project_tag, '" : true }}'),
    upsert = TRUE
  )
  connection$disconnect()
}


#' Save Dataset to DB
#'
#' Save a dataset to the MongoDB database
#'   
#' Rpackage file: utils-mongodb.R
#'
#' @param data The data to save
#' @param data_type The type of data to save (e.g. cropData, indicatorData...)
#' @param database The name of the database to save it to
#' @param url The url of the database
#' @param projectID The name of the project containing the data
#' @param formID The ID of the form
#'
#' @return
#' @export
#'
#' @examples
save_data_set_to_db <- function(data,
                                data_type,
                                database = "rhomis",
                                url = "mongodb://localhost",
                                projectID,
                                formID) {


#   data <- tibble::as_tibble(list(
#     "hhid"=c("hh1", "hh2", "hh3"),
#     "crop"=c("crop_1", "crop_2","crop_4")

#   )) 
#   data_type <- "crop_name"
#   projectID <-  "proj_test"
#   formID <- "form_test"
#   database <- "rhomis-test"
# url = "mongodb://localhost"


  data_string <- unlist(apply(data, FUN=function(row){
    item <- list()
    item$data <- as.list(row)
    item$data <- lapply(item$data, function(x){
      jsonlite::unbox(x)
    })
    item$projectID <- jsonlite::unbox(projectID)
    item$formID <- jsonlite::unbox(formID)
    item$dataType <- jsonlite::unbox(data_type)
    json_string <- jsonlite::toJSON(item, pretty = T, na = "null")

    return(json_string)
    # row
                                }, 
                                MARGIN=1))     

                                

  
  connection <- connect_to_db("data", database, url)
  connection$remove(paste0('{"projectID":"', projectID, '","formID":"', formID, '", "dataType":"', data_type, '"}'))
  lapply(data_string, function(x){
    connection$insert(x)
  })
  # connection$insert()

  # connection$update(paste0('{"projectID":"', projectID, '","formID":"', formID, '", "dataType":"', data_type, '"}'),
  #   paste0('{"$set":{"data": ', data_string, "}}"),
  #   upsert = TRUE
  # )


  connection$disconnect()

  connection <- connect_to_db("projectData", database, url)
  connection$update(paste0('{"projectID":"', projectID, '","formID":"', formID, '"}'),
    paste0('{"$addToSet":{"dataSets":', '"', data_type, '"}}'),
    upsert = TRUE
  )
  connection$disconnect()
}


#' Save List of Data Frames to the RHoMIS Database
#'
#' Take a list of output datasets and save them all to the
#' RHoMIS mongoDB
#'   
#' Rpackage file: utils-mongodb.R
#'
#' @param list_of_df The list of dataframes
#' @param projectID The name of the project
#' @param formID The name of the form
#' @param database The name of the database4
#' @param url The url of the database
#'
#' @return
#' @export
#'
#' @examples
save_list_of_df_to_db <- function(list_of_df,
                                  projectID,
                                  formID,
                                  database = "rhomis",
                                  url = "mongodb://localhost") {
  data_set_names <- names(list_of_df)

  sapply(data_set_names, function(x) {
    data_to_write <- list_of_df[[x]]
    if(length(data_to_write)==0){
                    return()
                }
    if (any(class(data_to_write) == "tbl_df") | any(class(data_to_write) == "tbl") | any(class(data_to_write) == "data.frame")) {
      save_data_set_to_db(
        data = data_to_write,
        data_type = x,
        database = database,
        url = url,
        projectID = projectID,
        formID = formID
      )
      return()
    }
    if (class(data_to_write) == "list") {
      names(data_to_write) <- paste0(x, "_", names(data_to_write))

      save_list_of_df_to_db(
        list_of_df = data_to_write,
        projectID = projectID,
        formID = formID,
        database = database,
        url = url
      )
      return()
    }

    return()
  })
}


#' Clean JSON String
#'
#' The strings produced from "json::stringify", when
#' processed, can contain some unnecessary characters.
#'
#' This function can be useful for removing these unnecessary
#' characters.
#'   
#' Rpackage file: utils-mongodb.R
#'
#' @param json_string The json string which needs to be cleaned
#'
#' @return
#' @export
#'
#' @examples
clean_json_string <- function(json_string) {
  json_string <- gsub("\n", "", json_string, fixed = T)
  json_string <- gsub('\\"', '"', json_string, fixed = T)
  json_string <- gsub('"\\', '"', json_string, fixed = T)
  json_string <- gsub("\\\\", "", json_string)


  return(json_string)
}


#' Read in DB Dataset
#'
#' A function for reading and unwinding
#' data in the RHoMIS database.
#'   
#' Rpackage file: utils-mongodb.R
#'
#' @param collection The collection containing the data needed
#' @param database The name of the database
#' @param project_name The name of the project which you are searching for
#' @param form_name The name of the form you are searching for
#' @param data_set_name The name of the dataset.
#'
#' @return
#' @export
#'
#' @examples
read_in_db_dataset <- function(collection = "data",
                               database,
                               project_name,
                               form_name,
                               data_set_name) {
  connection <- connect_to_db(collection, database, url)

#   data_set_name <- "crop_name"
#   project_name <-  "proj_test"
#   form_name <- "form_test"
#   database <- "rhomis-test"
# url = "mongodb://localhost"

  # Arguments to identify the relevant project
  match_arguments <- jsonlite::toJSON(list(
    "projectID" = jsonlite::unbox(project_name),
    "formID" = jsonlite::unbox(form_name),
    "dataType" = jsonlite::unbox(data_set_name)
  ), na = "null")

    result  <- connection$find(match_arguments)  
    result <- tibble::as_tibble(result$data)


  connection$disconnect()
  return(result)
}
