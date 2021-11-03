library(rhomis)

setwd("./inst/extdata/projects/UG_CRP_2020/")


processData(dataSource="csv",
outputType="csv",
coreOnly=T,
surveyFile=NULL,
moduleSaving=F,
extractUnits=F,
processDataSet=T,
dataFilePath="./Raw_Data.csv",
central_url=NULL,
central_email=NULL,
central_password=NULL,
project_name=NULL,
form_name=NULL,
form_version=NULL,
database=NULL)

setwd("../../../../")


# dataSource="csv"
# outputType="csv"
# coreOnly=T
# surveyFile=NULL
# moduleSaving=F
# extractUnits=F
# processDataSet=T
# dataFilePath="./Raw_Data.csv"
# central_url=NULL
# central_email=NULL
# central_password=NULL
# project_name=NULL
# form_name=NULL
# form_version=NULL
# database=NULL
