#' RHoMIS Data Exploration
#'
#' The RHoMIS dataset has a number of variables,
#' some of which are stored in a "repeat loop"
#' structure which is difficult to use.
#'
#' This script is designed to show users some
#' of the key functions in the RHoMIS R-package
#' that could be used to explore RHoMIS
#'
#' In this script we explore the dataset,
#' with help from the RHoMIS codebook. We
#' then use the RHoMIS R-package to calculate the amount
#' of land allocated to each individual crop.


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# 1. Setup -------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------


# Load Packages -----------------------------------------------------------
#' Library for processing RHoMIS data
#' and calculating key indicators
library(rhomis)
library(dplyr) # Library for
library(readr) # Library for reading and writing csv files
library(readxl) # Library for reading sheets from codebook

# Read in data ------------------------------------------------------------

#' Here we read in the full datasets, as well as the codebooks.

# Full Survey Dataset
full_data <- read_csv("./processed_data/processed_data.csv")
full_data_cdbk <- read_xls("./codebook.xls",
                           sheet = "full_data_codebook")
# Summary Indicators
indicator_summary <- read_csv("./indicator_data/indicator_data.csv")
indicator_summary_cdbk <- read_xls("./codebook.xls",
                                   sheet = "indicator_summary_codebook")

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# 2. Exploring Datasets ---------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# View all themes covered in the dataset
print(unique(full_data_cdbk$theme))

# Find all indicators for a theme
crop_loop_columns <- full_data_cdbk$short_name[full_data_cdbk$theme=="crop_production" &
                                                   full_data_cdbk$indicator==T & !is.na(full_data_cdbk$indicator)]

# See what columns are available in a "looped" portion of the data, e.g. crop loops
crop_loop_columns <- gsub("_[[:digit:]]","", crop_loop_columns) # removing numbers at the end
crop_loop_columns <- unique(crop_loop_columns)
crop_loop_columns

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# 3. Calculating Proportions of land allocated to each crop ---------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

number_of_crop_loops <- find_number_of_loops(full_data, "crop_land_area")

# Identifying all of the looped columns we will need for our calculation
crop_land_area_columns <- paste0("crop_land_area_", c(1:number_of_crop_loops))

# Creating new column names for the variables we would like to subset
crop_land_prop_area_columns <- paste0("crop_land_area_proportions_numeric_", c(1:number_of_crop_loops))
crop_land_area_ha_columns <- paste0("crop_land_area_ha_", c(1:number_of_crop_loops))

# Subsetting crop area columns
crop_land_data <- full_data[crop_land_area_columns]

# Calculating the proportion of and allocated to each crop
crop_land_proportions_numeric <- sapply(crop_land_area_columns, function(col_name){
    temp_df <-crop_land_data[col_name]
    temp_df$index <- c(1:nrow(temp_df))

    temp_df <- temp_df %>% merge(rhomis::proportion_conversions,by.x = c(col_name),by.y=c("survey_value"), all.x = T, all.y = F)
    temp_df <- temp_df[order(temp_df$index),]
    temp_df[["conversion"]]
}, simplify = F) %>% dplyr::bind_cols()

# Resetting column names
colnames(crop_land_proportions_numeric) <- crop_land_prop_area_columns

# Adding crop area proportions (numeric) back into the main dataset
full_data <- add_column_after_specific_column(data = full_data,
                                              new_data = crop_land_proportions_numeric,
                                              new_column_name = "crop_land_area_proportions_numeric",
                                              old_column_name = "crop_land_area",
                                              loop_structure = T
)

# Calculating Actual Amounts of Land Allocated ----------------------------

# Multiplying the total land
crop_lands_ha <- indicator_summary$land_cultivated_ha*crop_land_proportions_numeric
# Renaming columns
colnames(crop_lands_ha) <- crop_land_area_ha_columns
# Adding crop lands back into the dataset
full_data <- add_column_after_specific_column(data = full_data,
                                              new_data = crop_lands_ha,
                                              new_column_name = "crop_land_area_ha",
                                              old_column_name = "crop_land_area_proportions_numeric",
                                              loop_structure = T
)

# Extracting only land information into a csv
crop_lands_ha <- map_to_wide_format(data = full_data,
                                    name_column = "crop_name",
                                    column_prefixes = "crop_land_area_ha",types = "num")[[1]]

crop_land_proportions_numeric <- map_to_wide_format(data = full_data,
                                                    name_column = "crop_name",
                                                    column_prefixes = "crop_land_area_proportions_numeric",types = "num")[[1]]




# Legume use and intercropping --------------------------------------------------------------
# Extracting looped information on legume use in the dataset

intercropping <- map_to_wide_format(data=full_data,
                                    name_column = "crop_name",
                                    column_prefixes = "crop_land_area_ha",types = "num")[[1]]

legume_use_count <- table(full_data$use_legumes_fertility)






# Collapsing Values -------------------------------------------------------


conversion_tibble <-
    tibble::as_tibble(
    list(
        "survey_value" = c("maize", "cassava", "irish_potato", "tomato"),
        "conversion" = c("starchy_veg", "starchy_veg", "starchy_veg", "tomato")
    )
)

conversion_tibble <- make_per_project_conversion_tibble(full_data$id_rhomis_dataset,conversion_tibble)
crop_lands_ha$id_rhomis_dataset <- full_data$id_rhomis_dataset


merged_examples <- switch_column_names_and_merge_categories(crop_lands_ha,conversion_tibble)

