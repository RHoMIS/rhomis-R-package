readRenviron(".env")



dataSource="central"
outputType="mongodb"
coreOnly=F
surveyFile=NULL
moduleSaving=T
extractUnits=T
processDataSet=F
dataFilePath=NULL
central_url=paste0("https://",Sys.getenv("CENTRALURL"))
central_email=Sys.getenv("CENTRALEMAIL")
central_password=Sys.getenv("CENTRALPASSWORD")
project_name="aesffvsdvfsa"
form_name="form4"
form_version="version_gef2"
database="rhomis-data-dev"


xl_form <- get_xls_form(central_url,
              central_email,
              central_password,
              projectID,
              formID,
              form_version )

number_of_responses <- 10

for (response_index in 1:number_of_responses)
{
    mock_response <- generate_mock_response(survey = xl_form$survey,
                                            choices = xl_form$choices,
                                            metadata = xl_form$settings)

                                                    submit_xml_data(mock_response,
                                                                    central_url,
                                                                    central_email,
                                                                    central_password,
                                                                    projectID = projectID,
                                                                    formID = formID
                                                    )
}



