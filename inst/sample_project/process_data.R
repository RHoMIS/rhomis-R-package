library(rhomis)

setwd("inst/sample_project/")
path <-"raw-data/Raw_Data.csv"

processData(dataSource="csv",
            outputType="csv",
            coreOnly=T,
            surveyFile=NULL,
            moduleSaving=F,
            extractUnits=F,
            processDataSet=T,
            dataFilePath=path,
            central_url=NULL,
            central_email=NULL,
            central_password=NULL,
            project_name=NULL,
            form_name=NULL,
            form_version=NULL,
            database=NULL)

setwd("../../")
