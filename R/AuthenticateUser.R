#' These functions are helpful for managing
#' users onf the RHoMIS 2.0 authentication server.
#' Found here: https://github.com/l-gorman/rhomis-authenticator


#' Register User
#'
#' Register a user on the RHoMIS 2.0
#' authentication server
#' 
#' Rpackage file: AuthenticateUser.R
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
register_user <- function(url, username, email, password) {
  data <- tibble::as_tibble(list(
    "username" = username,
    "email" = email,
    "password" = password
  ))

  json_body <- jsonlite::toJSON(unbox(data), pretty = T)

  response <- httr::POST(
    url = paste0(url, "api/user/register"),
    body = json_body,
    encode = "raw",
    httr::add_headers("Content-Type" = "application/json")
  )

  content <- httr::content(response)

  if (grepl("Email already exists", content)) {
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
#' Rpackage file: AuthenticateUser.R
#' 
#' @param url The URL of the authentication server
#' @param email The email of the account being authenticated
#' @param password The password of the account being authenticated
#'
#' @return A token
#' @export
#'
#' @examples
login <- function(url, email, password) {
  data <- tibble::as_tibble(list(
    "email" = email,
    "password" = password
  ))

  json_body <- jsonlite::toJSON(unbox(data), pretty = T)

  response <- httr::POST(
    url = paste0(url, "api/user/login"),
    body = json_body,
    encode = "raw",
    httr::add_headers("Content-Type" = "application/json")
    # httr::add_headers("Authorization" = paste0(token))
  )

  content <- httr::content(response)

  if (grepl("Incorrect password", content)) {
    warning("Incorrect password")
    return()
  }
  if (grepl("Email not found", content)) {
    warning("Email not found")
    return()
  }

  content <- gsub(".*<p>", "", content)
  content <- gsub("</p>.*", "", content)
  return(content)
}