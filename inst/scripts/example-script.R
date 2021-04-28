library(rhomis)

root_path <- "inst/extdata/"
project_path <-"projects/VN_TST_2020/"

rhomis_data <- read_csv(paste0(root_path,project_path,"data/raw_data.csv"), na=c("","NA","n/a"))



colnames(rhomis_data) <- clean_column_names(colnames(rhomis_data), seperator="/", repeat_columns=c("crop_repeat", "livestock_repeat", "offfarm_repeat", "hh_rep"))

write_core_values_to_convert_to_file(rhomis_data, folder=paste0(root_path,project_path,"/cleaning"))
