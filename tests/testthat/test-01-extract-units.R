test_that("Checking that can extract units from a dataset", {

    rhomis_data <- load_rhomis_csv(
        file_path = "https://raw.githubusercontent.com/l-gorman/rhomis-R-package/main/inst/sample_local_project/raw-data/raw-data.csv",
        id_type = "string",
        proj_id = "test_prj",
        form_id = "test_frm"
        )


    units <- suppressWarnings(extract_units_and_conversions(rhomis_data))

    expect_equal(length(units)>0, TRUE)
})


test_that("Checking that can extract units from a csv dataset", {

    result <- tryCatch({
        suppressWarnings(extract_units_and_conversions_csv(base_path="./",
                                                           file_path="https://raw.githubusercontent.com/l-gorman/rhomis-R-package/main/inst/sample_local_project/raw-data/raw-data.csv",
                                                           id_type = "string",
                                                           proj_id = "test_prj",
                                                           form_id = "test_frm"))

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
            ".original_stage_1_conversions/",
            "conversions_stage_1/"
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

test_that("Can take a dataset from an ODK central server and process",{


    result = tryCatch({

        suppressWarnings(extract_units_and_conversions_server(
            central_url="https://github.com/l-gorman/rhomis-R-package/blob/main/inst/sample_central_project/sample-central-data.csv.zip?raw=true",
            central_email="test@domain.com",
            central_password="testpassword",
            project_name="test_project",
            form_name="test_form",
            database="rhomis-test",
            isDraft = F,
            central_test_case=T
        ))

        TRUE

    },
    error=function(error){
        traceback()
        print(error)
        return(FALSE)

    },
    finally={

        # Remove test entries from database
        data_collection <- mongolite::mongo(collection = "data",
                                            db = "rhomis-test",
                                            url =  "mongodb://localhost")
        data_collection$drop()
        data_collection$disconnect()

        project_data_collection <- mongolite::mongo(collection = "projectData",
                                                    db = "rhomis-test",
                                                    url =  "mongodb://localhost")
        project_data_collection$drop()
        project_data_collection$disconnect()

        units_and_conversion_collection <- mongolite::mongo(collection = "units_and_conversions",
                                                            db = "rhomis-test",
                                                            url =  "mongodb://localhost")
        units_and_conversion_collection$drop()
        units_and_conversion_collection$disconnect()

        units_and_conversion_collection <- mongolite::mongo(collection = "unmodified_units",
                                                            db = "rhomis-test",
                                                            url =  "mongodb://localhost")
        units_and_conversion_collection$drop()
        units_and_conversion_collection$disconnect()

        print("Database cleared")

    })

    expect_equal(result, TRUE)


})
