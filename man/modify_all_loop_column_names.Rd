% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CleanColumnNames.R
\name{modify_all_loop_column_names}
\alias{modify_all_loop_column_names}
\title{Modify all types of loop in RHoMIS}
\usage{
modify_all_loop_column_names(column_names, repeat_columns)
}
\arguments{
\item{column_names}{A list of all of the column names which}

\item{repeat_columns}{A list of all of the types
of repeat column which need to be changed}
}
\description{
The RHoMIS survey is structured using
a series of important loops. These are
often labelled "crop_repeat[x]". It is
important to be modify all of these loops
to have a simpler column name format for later analysis
}
\details{
Rpackage file: CleanColumnNames.R
}
\examples{
repeat_columns <- c(
  "crop_repeat",
  "livestock_repeat",
  "offfarm_repeat",
  "hh_rep"
)
column_names <- c(
  "xxx/crop_repeat[1]/crop_name",
  "xxx/livestock_repeat[2]/livestock_name",
  "xxx/crop_repeat[3]/crop_name",
  "xx/crop_repeat/crop_name",
  "x/offfarm_repeat[4]/offfarm_name",
  "y/hh_rep[5]/person_name",
  "z/crop_repeat/crop_name"
)
# Will return
# c("xxx/crop_repeat[1]/crop_name_1",
# "xxx/livestock_repeat[2]/livestock_name_2",
# "xxx/crop_repeat[3]/crop_name_3",
# "xx/crop_repeat/crop_name",
# "x/offfarm_repeat[4]/offfarm_name_4",
# "y/hh_rep[5]/person_name_5",
# "z/crop_repeat/crop_name")
}
