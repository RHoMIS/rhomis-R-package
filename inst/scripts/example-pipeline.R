# Loading environemnt variables from .env file
readRenviron(".env")



central_url <- "https://central.rhomis.cgiar.org"
# Accessing the environemnt variables
central_email <- Sys.getenv("RHOMIS_CENTRAL_EMAIL")
central_password <- Sys.getenv("RHOMIS_CENTRAL_PASSWORD")

# The name of the project we are interested in
project_name <- "Leo Test 1"

# Get data on the different central users
users <- get_users(central_url,
                   central_email,
                   central_password)

# Get a list of the different central projects
projects <-get_projects(central_url,
                        central_email,
                        central_password)

#' Identify which project ID matches the project
#' name we are interested in
projectID <- projects$id[projects$name==project_name]

forms <- get_forms(central_url,
                   central_email,
                   central_password,
                   projectID)

# We are interested in the first form from this project
formID <- forms$xmlFormId[1]

# Get data on the
submissions_list <- get_submissions_list(central_url,
                                         central_email,
                                         central_password,
                                         projectID,
                                         formID)


submission_data_result <- get_submission_data(central_url,
                                              central_email,
                                              central_password,
                                              projectID,
                                              formID )










