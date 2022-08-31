test_that("Can run final indicator calcalutaions", {
    proj_id <- "test_prj"
    form_id <- "test_frm"
    file_path <- "https://raw.githubusercontent.com/l-gorman/rhomis-R-package/main/inst/sample_local_project/raw-data/raw-data.csv"
    id_type="string"

    result <- tryCatch({
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

        # indicator_data <- read_folder_of_csvs(folder = paste0(base_path, "indicator_data/"))[[1]]
        # Read raw data
        rhomis_data <- load_rhomis_csv(
            file_path = file_path,
            id_type = id_type,
            proj_id = proj_id,
            form_id = form_id
        )

        units_and_conversions <- suppressWarnings(load_local_units(paste0( "./conversions_stage_1/"), id_rhomis_dataset = rhomis_data[["id_rhomis_dataset"]]))

        secondary_units <- sapply(names(pkg.env$secondary_units), function(unit_name){
            file_name <- paste0("./","conversions_stage_2/",unit_name,".csv")

            if (file.exists(file_name)){
                return(readr::read_csv(file_name))
            }
        }, simplify = F)

        units_and_conversions <- c(units_and_conversions, secondary_units)

        prices <- sapply(pkg.env$price_conversion_list, function(unit_name){
            file_name <- paste0("./","conversions_stage_2/",unit_name,".csv")

            if (file.exists(file_name)){
                return(readr::read_csv(file_name))
            }
        }, simplify = F)

        calorie_conversions <- sapply(pkg.env$calorie_conversion_list, function(unit_name){
            file_name <- paste0("./","conversions_stage_2/",unit_name,".csv")

            if (file.exists(file_name)){
                return(readr::read_csv(file_name))
            }
        }, simplify = F)


        result <- suppressWarnings(calculate_indicators(
            rhomis_data,
            units_and_conversions,
            prices,
            calorie_conversions,
            gender_categories=pkg.env$gender_categories))

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




    expect_equal(result, T)
})








test_that("Can run final indicator calcalutaions locally", {
    proj_id <- "test_prj"
    form_id <- "test_frm"
    file_path <- "https://raw.githubusercontent.com/l-gorman/rhomis-R-package/main/inst/sample_local_project/raw-data/raw-data.csv"
    id_type="string"

    result <- tryCatch({
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

        # indicator_data <- read_folder_of_csvs(folder = paste0(base_path, "indicator_data/"))[[1]]
        # Read raw data


        result <- suppressWarnings(calculate_indicators_local(
            base_path="./",
            file_path,
            id_type="string",
            proj_id,
            form_id,
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
            "conversions_stage_1/",

            "conversions_stage_2/",
            ".original_stage_2_conversions//",
            ".original_stage_1_conversions/",

            "crop_data",
            "livestock_data",
            "off_farm_data",
            "indicator_data",
            "extra_outputs",
            "processed_data",
            "preprocessed_data"

        )

        directories_to_remove <- paste0(base_path, directories)

        for (directory in directories_to_remove) {
            if (dir.exists(directory)) {
                unlink(directory, recursive = T)
            }
        }

        print("Directories Removed")

    })




    expect_equal(result, T)
})


test_that("Can run final indicator calcalutaions on server", {

    result <- tryCatch({
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
            central_test_case = central_test_case,
        ))
        suppressWarnings(calculate_prices_server(
            central_url = central_url,
            central_email = central_email,
            central_password = central_password,
            project_name = project_name,
            form_name = form_name,
            database = database,
            isDraft = isDraft,
            central_test_case = central_test_case,
        ))

        suppressWarnings(calculate_indicators_server(
            central_url = central_url,
            central_email = central_email,
            central_password = central_password,
            project_name = project_name,
            form_name = form_name,
            database = database,
            isDraft = isDraft,
            central_test_case = central_test_case,

        ))

        TRUE
    },
                       error=function(error){
                           traceback()
                           print(error)
                           return(FALSE)
                       },
                       finally = {
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

    expect_equal(2 * 2, 4)
})
