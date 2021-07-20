library(httr)


#' Get Survey Builder Project
#'
#' The survey builder contains meta-data about the people
#' using RHoMIS 2.0. This function connects to the survey
#' builder API to get the meta-data information from
#' multiple projects
#'
#' @param survey_builder_url The URL of the survey builder application
#' @param survey_builder_access_token The access toke to access the
#' survey builder API
#'
#' @return
#' @export
#'
#' @examples
get_survey_builder_projects <- function(survey_builder_url, survey_builder_access_token){

    survey_builder_response <- httr::GET(url = paste0(survey_builder_url, "/api/project"),
                                         encode = "json",
                                         httr::add_headers("Authorization" = paste0("Bearer ",survey_builder_access_token)))

    survey_projects<-httr::content(survey_builder_response)

    survey_projects<- central_results_to_df(survey_projects)

    return(survey_projects)
}

#' Get Individual Survey Builder Project
#'
#' The survey builder contains meta-data about the people
#' using RHoMIS 2.0. This function connects to the survey
#' builder API to get the meta-data information from the
#' individual projects
#'
#' @param survey_builder_url The URL of the survey builder application
#' @param survey_builder_access_token The access toke to access the
#' survey builder API
#' @param projectID The ID of the project you want to get
#' individual information for
#'
#' @return
#' @export
#'
#' @examples
get_individual_survey_builder_project <- function(survey_builder_url, survey_builder_access_token, projectID){
    survey_builder_response <- httr::GET(url = paste0(survey_builder_url, "/api/project/",projectID),
                                         encode = "json",
                                         httr::add_headers("Authorization" = paste0("Bearer ",survey_builder_access_token)))

    survey_projects<-httr::content(survey_builder_response)
    survey_projects[sapply(survey_projects, is.null)] <- NA
    survey_projects<- as_tibble(survey_projects)

    return(survey_projects)
}
