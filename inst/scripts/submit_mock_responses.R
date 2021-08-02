library(rhomis)

number_of_mock_responses <- 10

# Metadata needed to access the forms

central_url <- "https://central.rhomis.cgiar.org"
project_name <- "august_demo_project_1"
form_name <- "project_1_form_1"

readRenviron(".env")
central_email <- Sys.getenv("RHOMIS_CENTRAL_EMAIL")
central_password <- Sys.getenv("RHOMIS_CENTRAL_PASSWORD")

#' Making directory to store the xls form
#' as well as the responses
if(!dir.exists("inst/extdata/mock_responses")){
    dir.create("inst/extdata/mock_responses")
}

if(!dir.exists(paste0("inst/extdata/mock_responses/",project_name))){
    dir.create(paste0("inst/extdata/mock_responses/",project_name))
}

if(!dir.exists(paste0("inst/extdata/mock_responses/",project_name,"/", form_name))){
    dir.create(paste0("inst/extdata/mock_responses/",project_name,"/", form_name))
}

if(!dir.exists("inst/extdata/survey_file")){
    dir.create("inst/extdata/survey_file")
}

if(!dir.exists(paste0("inst/extdata/survey_file/",project_name, "/", form_name))){
    dir.create(paste0("inst/extdata/survey_file/",project_name, "/", form_name))
}

if(!dir.exists(paste0("inst/extdata/survey_file/",project_name, "/", form_name))){
    dir.create(paste0("inst/extdata/survey_file/",project_name, "/", form_name))
}


# Finding project information from the API
projects <-get_projects(central_url,
                        central_email,
                        central_password)
projectID <- projects$id[projects$name==project_name]

# Finding form information from the API
forms <- get_forms(central_url,
                   central_email,
                   central_password,
                   projectID)
formID <- forms$xmlFormId[forms$name==form_name]


xls_file_path <- get_xls_survey_file(central_url,
             central_email ,
             central_password,
             projectID,
             formID)

xls_form$

mock_response <- generate_mock_response(survey_path)



# Submitting data
submit_xml_data(mock_response,
                central_url,
                central_email,
                central_password,
                projectID=projectID,
                formID=formID)
