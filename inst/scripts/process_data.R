path_to_package = "/Users/sp20532/Documents/rhomis/rhomis-R-package/"
devtools::load_all(path=path_to_package)


processData(dataSource="csv",
outputType="csv",
coreOnly=T,
surveyFile=NULL,
moduleSaving=F,
extractUnits=F,
processDataSet=T,
dataFilePath="inst/extdata/projects/UG_GEN_2021/raw_data.csv",
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
