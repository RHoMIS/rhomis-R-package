library(rhomis)
readRenviron(".env")



dataSource="csv"
outputType="csv"
coreOnly=T
surveyFile=NULL
moduleSaving=F
extractUnits=F
processDataSet=T
dataFilePath="inst/extdata/projects/KE_ESA_2021/Raw_Data.csv"
central_url=NULL
central_email=NULL
central_password=NULL
project_name=NULL
form_name=NULL
form_version=NULL
database=NULL
draft=NULL


# dataSource="central"
# outputType="mongodb"
# coreOnly=F
# moduleSaving=T
# extractUnits=F
# processDataSet=T
# dataFilePath=NULL
# central_url=paste0("https://",Sys.getenv("CENTRALURL"))
# central_email=Sys.getenv("CENTRALEMAIL")
# central_password=Sys.getenv("CENTRALPASSWORD")
# project_name="u1p1"
# form_name="form1"
# form_version="version_xyz"
# database="rhomis-data-dev"
# draft=TRUE
#

processData(
    dataSource="csv",
    outputType="mongodb",
    coreOnly=T,
    moduleSaving=F,
    extractUnits=T,
    processDataSet=F,
    dataFilePath=NULL,
    central_url=paste0("https://",Sys.getenv("CENTRALURL")),
    central_email=Sys.getenv("CENTRALEMAIL"),
    central_password=Sys.getenv("CENTRALPASSWORD"),
    project_name="aesffvsdvfsa",
    form_name="form4",
    form_version="version_gef2",
    database="rhomis-data-dev",
    draft=F

)


project_name="aesffvsdvfsa"
form_name="form4"
form_version= "version_gef2"
#
#
# central_url=paste0("https://",Sys.getenv("CENTRALURL"))
# central_email=Sys.getenv("CENTRALEMAIL")
# central_password=Sys.getenv("CENTRALPASSWORD")
# project_name="wsedfghfdxxdcvb"
# form_name="form1"
# form_version= "version_xyz"

generateData(
    central_url=paste0("https://",Sys.getenv("CENTRALURL")),
    central_email=Sys.getenv("CENTRALEMAIL"),
    central_password=Sys.getenv("CENTRALPASSWORD"),
    project_name="aesffvsdvfsa",
    form_name="form4",
    form_version= "version_gef2",
    number_of_responses=5,
    draft=F
)




