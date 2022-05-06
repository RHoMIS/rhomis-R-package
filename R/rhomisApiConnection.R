#' RHoMIS Authenticate
#' 
#' Authenticate user for interacting with the
#' RHoMIS data api and authentication AP
#' 
#' @param base_url The URL for the api you are interacting with
#' @param email The email address of your account
#' @param password The password for your account
#' 
#' @return
#' @export
#'
#' @examples
rhomis_authenticate <- function(base_url, email, password){

  data_for_request <- list(email = email, password = password)
  # Passing a handle with the request allows for
  # the use of cookies. Facilitating multiple requests.
  h <- httr::handle(base_url)
  response <- httr::POST(
    url = paste0(base_url, "/api/user/login"),
    body = data_for_request,
    encode = "json",
    httr::add_headers("Content-Type" = "application/json"),
    handle = h )

    response_content <- as.character(httr::content(response))
    response_content <- response_content  %>% gsub(".*<p>","", .)  %>% gsub("</p>.*","",.) 

     if (response$status==400){


         stop(paste0("Error authenticating user:\n",response_content))
     }

    return(response_content)
}   

#' Create Project
#' 
#' Create a project in the RHoMIS database,
create_project <- function(api_url, auth_url, email, password, name){

    base_url <- "http://localhost:3002"
    email <- "test1@domain.com"
    password <- "testpass"

    token <- rhomis_authenticate(auth_url, email, password, name)

    response <- httr::POST(
    url = paste0(auth_url, "/api/projects/create-external"),
    body = data_for_request,
    encode = "json",
    httr::add_headers(
        "Content-Type" = "application/json",
        "Authorization"= token
    ),
    handle = h )

    response_content <- as.character(httr::content(response))

}

