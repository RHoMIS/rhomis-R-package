library(httr)
library(tibble)
library(tidyr)
library(dplyr)
library(readr)
library(magrittr)
#' Get Email
#'
#' A function used to get the email token required to connext to ODK central
#'
#' @param central_url The url which points to an ODK Central sever
#' @param central_email The email linked to your ODK central account
#' @param central_password The password used for your central account
#'
#' @return This function will return a token. This token is useful for
#' @examples
#' central_url <- "https://central.rhomis.cgiar.org"
#' central_email <- "my_email"
#' central_password <- "my_password"
#' #get_email_token(central_url,central_email,central_password)
get_email_token <- function(central_url, central_email, central_password){

    data_for_request<-list(email = central_email, password = central_password)
    h<-httr::handle(central_url) # Passing a handle with the request allows for the use of cookies. Facilitating multiple requests.
    central_response <- httr::POST(url = paste0(central_url, "/v1/sessions"),
                                   body = data_for_request,
                                   encode = "json",
                                   httr::add_headers("Content-Type" = "application/json"),
                                   handle=h
    )

    central_response_content<-httr::content(central_response)
    token<-central_response_content$token

    if (is.null(token)){
        stop("Unable to return token.\n
         Please check the credentials you provided are correct by going to your URL and logging in."
        )
    }

    return(token)
}



#' Get Users
#'
#' Get a table of users from the ODK central database
#'
#' @param central_url The url of the ODK central server
#' @param central_email The email of your ODK central account
#' @param central_password The password to your ODK central account
#'
#' @return A tibble of all of the users in the central database
#' @export
#'
#' @examples
#' central_url <- "https://central.rhomis.cgiar.org"
#' central_email <- "my_email"
#' central_password <- "my_password"
#' #get_users(central_url,central_email,central_password)
get_users <- function(central_url, central_email, central_password){

    email_token <- get_email_token(central_url,central_email,central_password)
    central_response <- httr::GET(url = paste0(central_url, "/v1/users"),
                                  encode = "json",
                                  httr::add_headers("Authorization" = paste0("Bearer ",email_token))
    )
    central_users<-httr::content(central_response)

    central_users <- central_results_to_df(central_users)
    return(central_users)
}

#' Get Projects
#'
#' Get a table of all of the projects on the ODK central server
#'
#' @param central_url The url of the ODK central server
#' @param central_email The email of your ODK central account
#' @param central_password The password to your ODK central account
#'
#' @return A table of all of the projects and their details
#' @export
#'
#' @examples
#' central_url <- "https://central.rhomis.cgiar.org"
#' central_email <- "my_email"
#' central_password <- "my_password"
#' #get_projects(central_url,central_email,central_password)
get_projects <- function(central_url, central_email, central_password){


    email_token <- get_email_token(central_url,central_email,central_password)
    central_response <- httr::GET(url = paste0(central_url, "/v1/projects"),
                                  encode = "json",
                                  httr::add_headers("Authorization" = paste0("Bearer ",email_token))
    )
    central_projects <- httr::content(central_response)
    central_projects <- central_results_to_df(central_projects)
    return(central_projects)
}

#' Get forms
#'
#' Get the forms for a specific ODK central project
#'
#' @param central_url The url of the ODK central server
#' @param central_email The email of your ODK central account
#' @param central_password The password to your ODK central account
#' @param projectID The ID of the project you are looking at. To get a list of project, see the
#' "get_projects" function
#'
#' @return A table of the forms available for a specific project
#' @export
#'
#' @examples
#' central_url <- "https://central.rhomis.cgiar.org"
#' central_email <- "my_email"
#' central_password <- "my_password"
#' #projects <- get_projects(central_url,central_email,central_password)
#' #get_forms(central_url,central_email,central_password, projectID$id[3])
get_forms <- function(central_url, central_email, central_password, projectID){

    email_token <- get_email_token(central_url,central_email,central_password)
    central_response <- httr::GET(url = paste0(central_url, "/v1/projects/",projectID,"/forms"),
                                  encode = "json",
                                  httr::add_headers("Authorization" = paste0("Bearer ",email_token))
    )
    central_forms <- httr::content(central_response)
    central_forms <- central_results_to_df(central_forms)
    return(central_forms)
}


#' Get Submissions List
#'
#' A function for seeing which submissions have been made for
#' a specific form, from a specific project
#'
#' @param central_url The url of the ODK central server
#' @param central_email The email of your ODK central account
#' @param central_password The password to your ODK central account
#' @param projectID The ID of the project you are looking at. To get a list of project, see the
#' "get_projects" function
#' @param formID The ID of the form containing the submissions you are looking at.
#' To get the list of forms see the "get_forms" function.
#'
#' @return
#' @export
#'
#' @examples
get_submissions_list <- function(central_url, central_email, central_password, projectID, formID){
    email_token <- get_email_token(central_url,central_email,central_password)
    central_response <- httr::GET(url = paste0(central_url, "/v1/projects/",projectID,"/forms/",formID,"/submissions"),
                                  encode = "json",
                                  httr::add_headers("Authorization" = paste0("Bearer ",email_token))
    )
    central_submissions <- httr::content(central_response)
    central_submissions <- central_results_to_df(central_submissions)
    return(central_submissions)
}

#' Get Submission Data
#'
#' A function for retrieving all of the ODK data as a zip file
#'
#' @param central_url The url of the ODK central server
#' @param central_email The email of your ODK central account
#' @param central_password The password to your ODK central account
#' @param projectID The ID of the project you are looking at. To get a list of project, see the
#' "get_projects" function
#' @param formID The ID of the form containing the submissions you are looking at.
#' To get the list of forms see the "get_forms" function.
#'
#' @return
#' @export
#'
#' @examples
get_submission_data <- function(central_url, central_email, central_password, projectID, formID){
    email_token <- get_email_token(central_url,central_email,central_password)
    file_destination <- tempfile(fileext=".zip")
    central_response <- httr::GET(url = paste0(central_url, "/v1/projects/",projectID,"/forms/",formID,"/submissions.csv.zip?attachments=false"),
                                  encode = "json",
                                  httr::add_headers("Authorization" = paste0("Bearer ",email_token)),
                                  httr::write_disk(file_destination, overwrite = TRUE))

    files <- unzip(file_destination)

    core_data_file_name <- files[grepl("repeat",files)==F]
    crop_repeat_file_name <- files[grepl("crop_repeat",files)==T]
    livestock_repeat_file_name <- files[grepl("livestock_repeat",files)==T]
    household_roster_repeat_file_name <- files[grepl("hh_pop_repeat",files)==T]
    offfarm_income_repeat_file_name <- files[grepl("offfarm_income_repeat",files)==T]


    #core_data <- suppressWarnings(readr::read_csv(files[1], col_types = cols()))
    core <- readr::read_csv(core_data_file_name)
    crop_repeat <- readr::read_csv(crop_repeat_file_name)
    livestock_repeat <- readr::read_csv(livestock_repeat_file_name)
    household_roster_repeat <- readr::read_csv(household_roster_repeat_file_name)
    offfarm_income_repeat <- readr::read_csv(offfarm_income_repeat_file_name)

    combined_data <- list("core"=core,
                          "crop_repeat"=crop_repeat,
                          "livestock_repeat"=livestock_repeat,
                          "household_roster_repeat"=household_roster_repeat,
                          "offfarm_income_repeat"=offfarm_income_repeat)

    main_data_set <- combined_data$core
    main_data_set <- central_loops_to_rhomis_loops(main_data_set,combined_data$household_roster_repeat)
    main_data_set <- central_loops_to_rhomis_loops(main_data_set,combined_data$crop_repeat)
    main_data_set <- central_loops_to_rhomis_loops(main_data_set,combined_data$livestock_repeat)
    main_data_set <- central_loops_to_rhomis_loops(main_data_set,combined_data$offfarm_income_repeat)


    colnames(main_data_set) <- tolower(clean_column_names(colnames(main_data_set), seperator = "-", repeat_columns = c("")))


    file.remove(file_destination)

    file.remove(core_data_file_name)
    file.remove(crop_repeat_file_name)
    file.remove(livestock_repeat_file_name)
    file.remove(household_roster_repeat_file_name)
    file.remove(offfarm_income_repeat_file_name)



    return(main_data_set)

}


#----------------------------------------------------------
#' Convert the nested list of central results into a tibble
#'
#' ODK central returns context in the form of a nested list. This converts
#' this nested list into a more accessible tibble format
#'
#' @param central_results The nested list returned from a central query
#'
#' @return A table of central results
#' @export
#'
#' @examples
#' central_results <- list(list("id"=1,"name"="name1","email"="email1"),
#' list("id"=2,"name"="name2","email"="email2"),
#' list("id"=3,"name"="name3","email"="email3"))
#' central_results_to_df(list(list("id"=1,"name"="name1","email"="email1"),
#' list("id"=2,"name"="name2","email"="email2"),
#' list("id"=3,"name"="name3","email"="email3")))
central_results_to_df <- function(central_results){
    column_headers <- unique(names(unlist(central_results)))
    all_tibbles <- sapply(central_results, function(x) widen_individual_result(x), simplify = F)
    final_df <- dplyr::bind_rows(all_tibbles)
    return (final_df)
}

#' Widen Individual Central Results
#'
#' Widening a single central result into a simple tibble
#'
#' @param individual_central_item The individual list item subsetted from a central query
#' @param column_headers The desired headers for the new table
#'
#' @return
#' @export
#'
#' @examples
#'central_results <- list(list("id"=1,"name"="name1","email"="email1"),
#'                        list("id"=2,"name"="name2","email"="email2"),
#'                        list("id"=3,"name"="name3","email"="email3"))
#'column_headers <- unique(names(unlist(central_results)))
#'individual_central_result <- central_results[2]
#'widen_individual_result(individual_central_result,column_headers)
widen_individual_result <- function(individual_central_item, column_headers){
    item_to_tibble <- stack(unlist(individual_central_item)[column_headers]) %>%
        tibble::as_tibble() %>%
        tidyr::pivot_wider(names_from = "ind", values_from="values") %>%
        dplyr::mutate_all(as.character)

    return(item_to_tibble)

}
