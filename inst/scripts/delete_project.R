library(rhomis)

central_url <- "https://central.rhomis.cgiar.org"
# Accessing the environemnt variables
central_email <- Sys.getenv("RHOMIS_CENTRAL_EMAIL")
central_password <- Sys.getenv("RHOMIS_CENTRAL_PASSWORD")

# Reading command line arguments
args <- commandArgs(trailingOnly = T)
if (length(args)!=1){
    stop("Incorrect number of arguments.
           \nNeed to supply 1 argument when calling this function from the command line (in this order):
           \n1. The name of the project you would like to delete")
}

project_name <- args[1]

#project_name <- "test_project_2"

projects <-get_projects(central_url,
                        central_email,
                        central_password)
projectID <- projects$id[projects$name==project_name]


delete_project(central_url, central_email, central_password, projectID)
