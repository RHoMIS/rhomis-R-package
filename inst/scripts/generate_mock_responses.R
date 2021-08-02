library(rhomis)
library(readr)

form_file_name <- "test_form_3"
file_extension <- ".xlsx"
number_of_responses <- 10

form_file_path<- paste0("inst/extdata/survey_file/",form_file_name,file_extension)

if(!dir.exists("inst/extdata/mock_responses")){
    dir.create("inst/extdata/mock_responses")
}

if(!dir.exists(paste0("inst/extdata/mock_responses/",form_file_name))){
    dir.create(paste0("inst/extdata/mock_responses/",form_file_name))
}


for (i in 1:number_of_responses){
    survey_response <- generate_mock_response(form_file_path)
    write(survey_response,file = paste0("inst/extdata/mock_responses/",form_file_name,"/response_",i,".xml"))
}
