test_that("multiplication works", {

    result = tryCatch({
    units <- suppressWarnings(extract_units_and_conversions_csv(
        file_path = "https://raw.githubusercontent.com/l-gorman/rhomis-R-package/main/inst/sample_local_project/raw-data/raw-data.csv",
        id_type = "string",
        proj_id = "test_prj",
        form_id = "test_frm"
    ))

    rhomis_data <- suppressWarnings(load_rhomis_csv(file_path = "https://raw.githubusercontent.com/l-gorman/rhomis-R-package/main/inst/sample_local_project/raw-data/raw-data.csv",
                                   id_type = "string",
                                   proj_id = "test_prj",
                                   form_id = "test_frm"))


    units <- suppressWarnings(load_local_units(paste0( "./units_and_conversions/"), id_rhomis_dataset = rhomis_data[["id_rhomis_dataset"]]))




    secondary_units <- suppressWarnings(get_secondary_conversions(
        rhomis_data=rhomis_data,
        units_and_conversions=units,
        gender_categories = pkg.env$gender_categories))

    TRUE

    },
    error=function(error){
        traceback()
        print(error)
        return(FALSE)

    },

    finally={
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
            "gender_control/",
            ".original_calorie_conversions/",
            ".original_mean_prices_conversions/",
            ".original_units/",
            "calorie_conversions/",
            "units_and_conversions/",
            "mean_prices/"
        )

        directories_to_remove <- paste0(base_path, directories)

        for (directory in directories_to_remove) {
            if (dir.exists(directory)) {
                unlink(directory, recursive = T)
            }
        }

        print("Directories Removed")

    })

    expect_equal(result, TRUE)
})
