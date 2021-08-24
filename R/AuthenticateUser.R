library(httr)
library(jsonlite)
library(tibble)

#' Register User
#'
#' Register a user on the RHoMIS 2.0
#' authentication server
#'
#' @param url The url of the server we are using for authentication
#' @param username The username of the user to be registered
#' @param email The email of the user to be registered
#' @param password The password of the user to be registered
#'
#' @return Will return the user ID from the database
#' @export
#'
#' @examples
register_user <- function(url, username, email, password){
    # url <- "http://localhost:3002/"
    # username <- "test_user_1"
    # email <- "test_user_1@domain.com"
    # password <- "testpassword1"

    data <- tibble::as_tibble(list("username"=username,
                                   "email"=email,
                                   "password"=password))



    json_body <- jsonlite::toJSON(unbox(data),pretty = T)


    response <- httr::POST(url = paste0(url, "api/user/register"),
                                   body=json_body,
                                   encode = "raw",
                                   httr::add_headers("Content-Type" = "application/json")
                                   #httr::add_headers("Authorization" = paste0(token))
    )

    content <- httr::content(response)

    if (grepl("Email already exists", content))
    {
        warning("Email already exists")
        return()

    }

    return(content$userID)


}

#' Login
#'
#' A function for logging in to the RHoMIS 2.0
#' token based authenticator
#'
#' @param url The URL of the authentication server
#' @param email The email of the account being authenticated
#' @param password The password of the account being authenticated
#'
#' @return A token
#' @export
#'
#' @examples
login <- function(url, email, password){

    # url <- "http://localhost:3002/"
    # email <- "test_ser_1@domain.com"
    # password <- "testpassword1"

    data <- tibble::as_tibble(list("email"=email,
                                   "password"=password))



    json_body <- jsonlite::toJSON(unbox(data),pretty = T)


    response <- httr::POST(url = paste0(url, "api/user/login"),
                           body=json_body,
                           encode = "raw",
                           httr::add_headers("Content-Type" = "application/json")
                           #httr::add_headers("Authorization" = paste0(token))
    )

    content <- httr::content(response)

    if (grepl("Incorrect password", content))
    {
        warning("Incorrect password")
        return()

    }
    if (grepl("Email not found", content))
    {
        warning("Email not found")
        return()

    }

        content<- gsub(".*<p>","",content)
        content<- gsub("</p>.*","",content)
       return(content)


}

#' Delete User
#'
#' A function to delete a user from the RHoMIS authentication server
#'
#' @param url The URL of the authentication server
#' @param email The email of the account being deleted
#' @param password The password of the account being deleted
#'
#'
#' @return
#' @export
#'
#' @examples
delete_user <- function(url, email, password){

}
