# Installation of packages, and saves said packages in a virtual environment

install.packages(c("devtools",
                   "dplyr",
                   "tidyr",
                   "ggplot2",
                   "renv",
                   "readxl",
                   "readr",
                   "magrittr"))

library(renv)
library(devtools)
devtools::install_github("https://github.com/RHoMIS/rhomis-R-package", force = TRUE)

renv::init()
renv::snapshot()
