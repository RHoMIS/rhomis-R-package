library(rhomis)

setwd("inst/sample_project/")

path <-"raw-data/Raw_Data.csv"

# dataSource="csv"
# outputType="csv"
# coreOnly=T
# surveyFile=NULL
# moduleSaving=F
# extractUnits=T
# processDataSet=F
# dataFilePath=path
# central_url=NULL
# central_email=NULL
# central_password=NULL
# project_name=NULL
# form_name=NULL
# form_version=NULL
# database=NULL

processData(proj_id = "test_project",
            form_id = "test_form",
            dataSource="csv",
            outputType="csv",
            coreOnly=T,
            surveyFile=NULL,
            moduleSaving=F,
            extractUnits=T,
            processDataSet=F,
            dataFilePath=path,
            central_url=NULL,
            central_email=NULL,
            central_password=NULL,
            project_name=NULL,
            form_name=NULL,
            form_version=NULL,
            database=NULL)

 setwd("../../")
