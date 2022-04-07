
base_path <- "./"
directories <- c(
    "processed_data/",
    "indicator_data/",
    "crop_data/",
    "livestock_data/",
    "off_farm_data/",
    "converted_units/",
    "original_units/",
    "converted_prices/",
    "original_prices/",
    "completed_calorie_conversions/",
    "original_calorie_conversions/",
    "consumption_calorie_values/",
    "consumption_lcu_values/",
    "gender_control/"
)

directories_to_remove <- paste0(base_path, directories)

for (directory in directories_to_remove) {
    if (dir.exists(directory)) {
        unlink(directory, recursive = T)
    }
}

print("Directories Removed")