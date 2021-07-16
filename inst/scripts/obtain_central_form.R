# Setup -------------------------------------------------------------------
library(rhomis)
library(knitr)
library(uuid)
# Loading environemnt variables from .env file
readRenviron(".env")

central_url <- "https://central.rhomis.cgiar.org"
# Accessing the environemnt variables
central_email <- Sys.getenv("RHOMIS_CENTRAL_EMAIL")
central_password <- Sys.getenv("RHOMIS_CENTRAL_PASSWORD")

# The name of the project we are interested in
project_name <- "Leo Test 1"
form_name <- "RHoMIS 1.6"

# Linkning to ODK Central -------------------------------------------------

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
#kable(forms)

# We are interested in the first form from this project
formID <- forms$xmlFormId[forms$name=="RHoMIS 1.6"]

formVersion <- forms$version[forms$name=="RHoMIS 1.6"]


# metadata <- extract_form_metadata(central_url,
#                          central_email,
#                          central_password,
#                          projectID,
#                          formID,
#                          version=formVersion)


submissions <- submissions_all(central_url,
                central_email,
                central_password,
                projectID,
                formID)

submissionIDs <- submissions$instanceId
submissionID <- submissionIDs[1]

submission_xml <- get_submission_xml(central_url,
                   central_email,
                   central_password,
                   projectID,
                   formID,
                   submissionID)

write(submission_xml,"inst/extdata/xml_data/submission_1_core.xml")
xml_string <- paste0(readLines("inst/extdata/xml_data/submission_1_core.xml"),collapse = "\n")
cat(xml_data)

set.seed(1)
deviceID <- uuid::UUIDgenerate()



