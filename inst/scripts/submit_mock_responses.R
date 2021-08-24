library(rhomis)


# Obtaining the arguments from the command line call of the script
# The first argument is the number of responses to generate
# The second argument is the name of the project of interest
# The third argument is the name of the form of interest

args <- commandArgs(trailingOnly = T)

if (length(args)!=5){
    stop("Incorrect number of arguments.
           \nNeed to supply 4 arguments when calling this function from the command line (in this order):
           \n1  Enter 'true' if you would like to generate random responses. Enter 'false' if you would not
           \n2. The number of responses you would like to generate (999 if not generating data).
           \n3. The name of the project you would like to generate data for.
           \n4. The name of the form you are generating data for.
           \n5. The working directory that you are working from")
}



if (args[1]=="true"){
    generate_new_data <- T

}

if (args[1]=="false"){
    generate_new_data <- F

}

number_of_mock_responses <- args[2]

# Metadata needed to access the forms ----------------------------
central_url <- "https://central.rhomis.cgiar.org"


#project_name <- "august_demo_project_2"
project_name <- args[3]
#form_name <- "project_2_form_1"
form_name <- args[4]

setwd(args[5])

readRenviron(".env")
central_email <- Sys.getenv("RHOMIS_CENTRAL_EMAIL")
central_password <- Sys.getenv("RHOMIS_CENTRAL_PASSWORD")
#---------------------------------------------------------------
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

if(!dir.exists(paste0("inst/extdata/survey_file/",project_name))){
    dir.create(paste0("inst/extdata/survey_file/",project_name))
}

#---------------------------------------------------------------
# The file paths for the surveys and the various mock responses
survey_destination <- paste0("inst/extdata/survey_file/",project_name, "/", form_name,".xlsx")
responses_destination <- paste0("inst/extdata/mock_responses/",project_name,"/", form_name)

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

#' Writing the xlsx file from odk central to a specific location

xls_file_path <- get_xls_survey_file(central_url,
                                     central_email ,
                                     central_password,
                                     projectID,
                                     formID,
                                     file_destination = survey_destination)
#---------------------------------------------------------------

if (generate_new_data==F){
    number_of_mock_responses <- length(list.files(responses_destination))
}


# Writing fake data and saving the responses
for(i in 1:number_of_mock_responses)
{



    if (generate_new_data==T)
    {
    mock_response <- generate_mock_response(xls_file_path)
    write(mock_response, paste0(responses_destination,"/response_",i,".xml"))
    }

    if (generate_new_data==F)
    {
        # Linking to file
        filename <- paste0(responses_destination,"/response_",i,".xml")

        mock_response <- readChar(filename, file.info(filename)$size)
    }

    submit_xml_data(mock_response,
                    central_url,
                    central_email,
                    central_password,
                    projectID=projectID,
                    formID=formID)
}


