library(rhomis)

setwd("./inst/sample_project/")
path <-"raw-data/raw-data.csv"


processData(
    dataFilePath="./raw-data/raw-data.csv",
    proj_id = "test_project",
    form_id = "test_form",
    dataSource="csv",
    outputType="csv",
    coreOnly=T,
    surveyFile=NULL,
    moduleSaving=F,
    extractUnitsOnly =F,
)

setwd("../../")
