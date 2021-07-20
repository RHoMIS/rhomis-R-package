library(rhomis)


readRenviron(".env")

central_url <- "https://central.rhomis.cgiar.org"
# Accessing the environemnt variables
central_email <- Sys.getenv("RHOMIS_CENTRAL_EMAIL")
central_password <- Sys.getenv("RHOMIS_CENTRAL_PASSWORD")

survey_path <- "inst/extdata/survey_file/RHoMIS_v1.6.xlsx"


mock_response <- generate_mock_response(survey_path)

# Finding project information from the API
project_name <- "Leo Test 1"
projects <-get_projects(central_url,
                        central_email,
                        central_password)
projectID <- projects$id[projects$name==project_name]
forms <- get_forms(central_url,
                   central_email,
                   central_password,
                   projectID)
formID <- forms$xmlFormId[1]



# Submitting data
submit_xml_data(mock_response,
                central_url,
                central_email,
                central_password,
                projectID=projectID,
                formID=formID)
