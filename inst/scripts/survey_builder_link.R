library(rhomis)

#' Example script for showing that we can link to the survey
#' builder database

#load_environment_variables
readRenviron(".env")

survey_builder_url <- Sys.getenv("RHOMIS_SURVEY_BUILDER_URL")
survey_builder_access_token <- Sys.getenv("RHOMIS_SURVEY_BUILDER_ACCESS_TOKEN")

get_survey_builder_projects(survey_builder_url,survey_builder_access_token)
get_individual_survey_builder_project(survey_builder_url,survey_builder_access_token,1)


