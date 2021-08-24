library(mongolite)
library(jsonlite)
library(tibble)
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
    mongodb <- mongolite::mongo(collection = collection,
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
#' sample_data_frame <- tibble::as_tibble(list("original_spelling"=c("benana","maz","wetermalon","cokonut"),
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

#' Add Data to Project List
#'
#' A method for submitting another project
#' to a list of projects.
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
add_data_to_project_list <- function(data,collection,database="rhomis", url="mongodb://localhost",projectID,formID,overwrite=F){

    data_string <- jsonlite::toJSON(data,pretty=T, na = "null")
    connection <- connect_to_db(collection,database,url)

    if(overwrite==F){

        data_string <- paste0('{"projectID":"',projectID,'","formID":',formID,', "data"',":",data_string,"}")
        data_string <- gsub("\n","",data_string, fixed=T)
        data_string <- gsub('\\"','"',data_string, fixed=T)
        data_string <- gsub('"\\','"',data_string, fixed=T)

        connection$insert(data_string)
    }

    if(overwrite==T){

        connection$update(paste0('{"projectID":"',projectID,'","formID":"',formID,'"}'),
                          paste0('{"$set":{"data": ',data_string,'}}')
                          ,upsert = TRUE)
    }


}

#' Add Project to List
#'
#' After conducting the main RHoMIS calculations, it is important to add the
#' form ID and project ID to the list of projects in the database, to keep track of the
#' projects using RHoMIS. This function allows us to do this.
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
adding_project_to_list <- function(database="rhomis", url="mongodb://localhost",projectID,formID){

    #collection, database="rhomis", url="mongodb://localhost",projectID,formID

    connection <- connect_to_db("projectData",database,url)

    connection$update(paste0('{"projectID":"',projectID,'","formID":"',formID,'"}'),
                      paste0('{"$set":{"projectID": ','"',projectID,'", "formID":"',formID,'"}}')
                      ,upsert = TRUE)

}

