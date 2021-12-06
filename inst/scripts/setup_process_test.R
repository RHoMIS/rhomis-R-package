library(rhomis)
readRenviron(".env")



# dataSource="csv"
# outputType="csv"
# coreOnly=T
# surveyFile=NULL
# moduleSaving=F
# extractUnits=F
# processDataSet=T
# dataFilePath=NULL
# central_url=paste0("https://",Sys.getenv("CENTRALURL"))
# central_email=Sys.getenv("CENTRALEMAIL")
# central_password=Sys.getenv("CENTRALPASSWORD")
# project_name="wsedfghfdxxdcvb"
# form_name="form1"
# form_version="version_xyz"
# database="rhomis-data-dev"
# draft=TRUE


dataSource="central"
outputType="mongodb"
coreOnly=F
moduleSaving=T
extractUnits=F
processDataSet=T
dataFilePath=NULL
central_url=paste0("https://",Sys.getenv("CENTRALURL"))
central_email=Sys.getenv("CENTRALEMAIL")
central_password=Sys.getenv("CENTRALPASSWORD")
project_name="u1p1"
form_name="form1"
form_version="version_xyz"
database="rhomis-data-dev"
draft=TRUE


processData(
    dataSource="central",
    outputType="mongodb",
    coreOnly=F,
    moduleSaving=T,
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




