library(mongolite)
library(jsonlite)
library(tidyverse)
#' Connect to db
#'
#' A simple function for connecting to a mongo database
#'
#' @param collection Which collection you want to connect to
#' @param database Which database you want to connect to
#' @param url The URL of the database you want to connect to
#'
#' @return Returns a connection object
#' @export
#'
#' @examples
#'
connect_to_db <- function(collection,database="rhomis", url="mongodb://localhost"){
    mongodb <- mongo(collection = collection,
                     db=database,
                     url="mongodb://localhost")
    return(mongodb)
}


#' Data frame to JSON
#'
#' To write objects to a mongoDB, they need to be in JSON format.
#' This function converts data from a data frame to a JSON
#'
#' @param data_to_change The data frame that needs to be converted
#'
#' @return A JSON string
#' @export
#'
#' @examples
#' sample_data_frame <- as_tibble(list("original_spelling"=c("benana","maz","wetermalon","cokonut"),
#' "standardised_spelling"=c("benana",NA,"wetermalon","cokonut")))
#' data_frame_to_json(sample_data_frame)
#'
data_frame_to_json <- function(data_to_change){
    json_copy <- jsonlite::toJSON(data_to_change)
    return(as.character(json_copy))
}


#' Querying whole collection
#'
#' Returning all of the information from a mongoDB query in a tabular format
#'
#' @param collection The collection you want to collect the information from
#' @param database The database you are querying
#' @param url The url of the database you are querying
#'
#' @return A data frame of the data you are querying
#' @export
#'
#' @examples
find_collection <- function(collection,database="rhomis", url="mongodb://localhost"){
    connection <-connect_to_db(collection,database=database, url=url)
    data <- connection$find("{}")
    return(data)
}

#' Count collection
#'
#' Count the number of documents in a collection
#'
#' @param collection The collection you want to collect the information from
#' @param database The database you are querying
#' @param url The url of the database you are querying
#'
#' @return
#' @export
#'
#' @examples
count_collection <- function(collection,database="rhomis", url="mongodb://localhost"){
    connection <-connect_to_db(collection,database=database, url=url)
    count <- connection$count("{}")
    return(count)
}



#' Write a New Collection
#'
#' Write a new table to a RHoMIS db database
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
write_new_collection <- function(data_to_write, collection,database="rhomis", url="mongodb://localhost"){


    previous_data <- find_collection(collection,database,url)
    if (nrow(previous_data)>0)
    {
        stop(paste0("Collection '",collection,"' exists already"))
    }

    connection <- connect_to_db(collection,database,url)

    # Insert the data frame
    connection$insert(data_to_write)
    print("Success in creating new table")
}


update_collection <- function(data_to_write, collection,database="rhomis", url="mongodb://localhost")
{


    previous_data <- find_collection(collection,database,url)
    if (nrow(previous_data)==0)
    {
        stop(paste0("Collection '",collection,"'does not exist yet"))
    }

    connection <- connect_to_db(collection,database,url)

    # Insert the data frame
    connection$insert(data_to_write)
    print("Success in updating table")

}



