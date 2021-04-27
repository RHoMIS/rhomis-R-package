library(tidyverse)

#' Load RHoMIS
#'
#' Reads the RHoMIS data from a local file.
#'
#' @param path The path to the RHoMIS file. If
#'
#' @return Returns a tibble of the RHoMIS dataset.
#' @export
#'
#' @examples
#' # path <- "C:/User/Desktop/RHoMIS_Project/Data/raw_data.csv"
#' # data <- load_rhomis(path)
#' # Data will be loaded in tibble format
load_rhomis <- function(path){
    data <- read_csv(path)
}

