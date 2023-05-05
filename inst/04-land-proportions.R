library(rhomis)
library(readr)
library(dplyr)

# Full data path
rhomis_data <- readr::read_csv()

# Indicator sheet path
indicator_data <- readr::read_csv()
# Calculating Numeric Land Use Proportions --------------------------------

number_of_crop_loops <- find_number_of_loops(rhomis_data, "crop_land_area")
crop_land_area_columns <- paste0("crop_land_area_", c(1:number_of_crop_loops))
crop_land_prop_area_columns <- paste0("crop_land_area_proportions_numeric_", c(1:number_of_crop_loops))
crop_land_area_ha_columns <- paste0("crop_land_area_ha_", c(1:number_of_crop_loops))

crop_land_data <- rhomis_data[crop_land_area_columns]

crop_land_proportions_numeric <- sapply(crop_land_area_columns, function(col_name){

    temp_df <-crop_land_data[col_name]
    temp_df$index <- c(1:nrow(temp_df))

    temp_df <- temp_df %>% merge(rhomis::proportion_conversions,by.x = c(col_name),by.y=c("survey_value"), all.x = T, all.y = F)
    temp_df <- temp_df[order(temp_df$index),]
    temp_df[["conversion"]]
}, simplify = F) %>% dplyr::bind_cols()
colnames(crop_land_proportions_numeric) <- crop_land_prop_area_columns

rhomis_data <- add_column_after_specific_column(data = rhomis_data,
                                 new_data = crop_land_proportions_numeric,
                                 new_column_name = "crop_land_area_proportions_numeric",
                                 old_column_name = "crop_land_area",
                                 loop_structure = T
                                 )




# Calculating Actual Amounts of Land Allocated ----------------------------
indicator_data$land_cultivated_ha

crop_lands_ha <- indicator_data$land_cultivated_ha*crop_land_proportions_numeric
colnames(crop_lands_ha) <- crop_land_area_ha_columns
rhomis_data <- add_column_after_specific_column(data = rhomis_data,
                                                new_data = crop_lands_ha,
                                                new_column_name = "crop_land_area_ha",
                                                old_column_name = "crop_land_area_proportions_numeric",
                                                loop_structure = T
)


# crop_land_proportions_numeric$id_rhomis_dataset <- rhomis_data$id_rhomis_dataset
# crop_lands_ha$id_rhomis_dataset <- rhomis_data$id_rhomis_dataset
id_cols <- make_new_dataset(rhomis_data)
crop_lands_ha <- map_to_wide_format(data = rhomis_data,
                                    name_column = "crop_name",
                                    column_prefixes = "crop_land_area_ha",types = "num")
crop_lands_ha <- id_cols %>% dplyr::bind_cols(crop_lands_ha)

crop_land_proportions_numeric <- map_to_wide_format(data = rhomis_data,
                                    name_column = "crop_name",
                                    column_prefixes = "crop_land_area_proportions_numeric",types = "num")
crop_land_proportions_numeric <- id_cols %>% dplyr::bind_cols(crop_land_proportions_numeric)

readr::write_csv(rhomis_data,paste0(base_path,raw_data_path))

dir.create(paste0(base_path,"croplands/"))
readr::write_csv(crop_lands_ha,paste0(base_path,"croplands/crop_lands_ha.csv"))
readr::write_csv(crop_land_proportions_numeric,paste0(base_path,"croplands/crop_lands_proportions.csv"))

