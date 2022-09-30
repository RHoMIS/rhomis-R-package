test_that("Can run second calculation stage", {

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


        units <- suppressWarnings(load_local_units(paste0( "./conversions_stage_1/"), id_rhomis_dataset = rhomis_data[["id_rhomis_dataset"]]))




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
            ".original_stage_1_conversions",
            "conversions_stage_1"
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




test_that("Can run second calculation stage locally", {

    result = tryCatch({

        proj_id <- "test_prj"
        form_id <- "test_frm"
        file_path <- "https://raw.githubusercontent.com/l-gorman/rhomis-R-package/main/inst/sample_local_project/raw-data/raw-data.csv"
        id_type="string"
        suppressWarnings(extract_units_and_conversions_csv(base_path="./",
                                                           file_path=file_path,
                                                           id_type = id_type,
                                                           proj_id = proj_id,
                                                           form_id = form_id))
        suppressWarnings(calculate_prices_csv(
            base_path="./",
            file_path,
             id_type=id_type,
            proj_id,
            form_id
        ))


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
            "conversions_stage_1/",
            "conversions_stage_2/",
            ".original_stage_2_conversions/",
            ".original_stage_1_conversions/"
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


test_that("Can run second calculation stage on server", {

    result = tryCatch({

        central_url <- "https://github.com/l-gorman/rhomis-R-package/blob/main/inst/sample_central_project/sample-central-data.csv.zip?raw=true"
        central_email <- "test@domain.com"
        central_password <- "testpassword"
        project_name <- "test_project"
        form_name <- "test_form"
        database <- "rhomis-test"
        isDraft <- F
        central_test_case <- T

        suppressWarnings(extract_units_and_conversions_server(
            central_url = central_url,
            central_email = central_email,
            central_password = central_password,
            project_name = project_name,
            form_name = form_name,
            database = database,
            isDraft = isDraft,
            central_test_case = central_test_case
        ))
        suppressWarnings(calculate_prices_server(
            central_url = central_url,
            central_email = central_email,
            central_password = central_password,
            project_name = project_name,
            form_name = form_name,
            database = database,
            isDraft = isDraft,
            central_test_case = central_test_case
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




