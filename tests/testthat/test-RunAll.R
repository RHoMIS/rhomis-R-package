test_that("can create ID columns", {
    data <- tibble::as_tibble(list(
        "proj_id_column_test"=c("proj_1", "proj_2", "proj_1"),
        "form_id_column_test"=c("form_1", "form_1", "form_1"),
        "country_column_test"=c("country_x", "country_y", "country_x"),
        "unique_column_test"=c("a","b","c"),
        "hh_id_col_test"=c("a1","b1","t1"),
        "random_other_column"=c("srrfe","serasefdae","sresf")
    ))


    result <- make_id_columns(data=data,
                    country_column = "country_column_test",
                    unique_id_col = "unique_column_test",
                    hh_id_col = "hh_id_col_test",
                    id_type = "string",
                    proj_id = "proj_test",
                    form_id = "form_test"
                        )

    expected_result <- tibble::as_tibble(list(
        "id_unique"=c("a","b","c"),
        "id_hh"=c("50f0e25fb99189d87ff4d25b97f9347d", "362fe4f998ad34cce3a4bedea3228fef", "c522cd7db2c96587c2b9286f0b418736"),
        "id_rhomis_dataset"=c("8d1dd15d89377c8c0395d035630f04c6","b0c69e2a0ff049f17fb4d31a3eb0e56e","8d1dd15d89377c8c0395d035630f04c6"),
        "id_form"=c("form_test", "form_test", "form_test"),
        "id_proj"=c("proj_test", "proj_test", "proj_test"),
        "proj_id_column_test"=c("proj_1", "proj_2", "proj_1"),
        "form_id_column_test"=c("form_1", "form_1", "form_1"),
        "country_column_test"=c("country_x", "country_y", "country_x"),
        "unique_column_test"=c("a","b","c"),
        "hh_id_col_test"=c("a1","b1","t1"),
        "random_other_column"=c("srrfe","serasefdae","sresf")
    ))

    testthat::expect_equal(result, expected_result)


    result <- make_id_columns(data=data,
                              country_column = "country_column_test",
                              unique_id_col = "unique_column_test",
                              hh_id_col = "hh_id_col_test",
                              id_type = "column",
                              proj_id = "proj_id_column_test",
                              form_id = "form_id_column_test"
    )

    expected_result <- tibble::as_tibble(list(
        "id_unique"=c("a","b","c"),
        "id_hh"=c("50f0e25fb99189d87ff4d25b97f9347d", "362fe4f998ad34cce3a4bedea3228fef", "c522cd7db2c96587c2b9286f0b418736"),
        "id_rhomis_dataset"=c("111925a12bfd39fad140e104ae37da10","98b7efeacbd63306a37d0de4f84f4613","111925a12bfd39fad140e104ae37da10"),
        "id_form"=c("form_1", "form_1", "form_1"),
        "id_proj"=c("proj_1", "proj_2", "proj_1"),
        "proj_id_column_test"=c("proj_1", "proj_2", "proj_1"),
        "form_id_column_test"=c("form_1", "form_1", "form_1"),
        "country_column_test"=c("country_x", "country_y", "country_x"),
        "unique_column_test"=c("a","b","c"),
        "hh_id_col_test"=c("a1","b1","t1"),
        "random_other_column"=c("srrfe","serasefdae","sresf")
    ))

    testthat::expect_equal(result, expected_result)







})


testthat::test_that("Can process a whole central dataset",{

   
     result <- tryCatch({
        # Extract Units
        suppressWarnings(processData(
            extractUnitsOnly = T, # The stage of data processing

            # Arguments to indicate the type of processing being done (local or on server)
            dataSource = "central",
            outputType = "mongodb",

            # Arguments used for processing local data sets
            central_url = "https://github.com/l-gorman/rhomis-R-package/blob/end-to-end-example/inst/sample_central_project/sample-central-data.csv.zip?raw=true",
            central_email = "test@domain.com",
            central_password = "testpassword",
            project_name = "test_project",
            form_name = "test_form",
            form_version = "test_version",
            isDraft = F,
            central_test_case = T,
            database = "rhomis-test",
        ))

        # Calculate initial indicators
        suppressWarnings(processData(
            extractUnitsOnly = F,
            calculateInitialIndicatorsOnly =T, # The stage of data processing

            # Arguments to indicate the type of processing being done (local or on server)
            dataSource="central",
            outputType="mongodb",

            # Arguments used for processing local data sets
            central_url="https://github.com/l-gorman/rhomis-R-package/blob/end-to-end-example/inst/sample_central_project/sample-central-data.csv.zip?raw=true",
            central_email="test@domain.com",
            central_password="testpassword",
            project_name="test_project",
            form_name="test_form",
            form_version="test_version",
            isDraft = F,
            central_test_case=T,
            database="rhomis-test"
        ))

        # Calculate Final Indicators
        suppressWarnings(processData(
            extractUnitsOnly = F,
            calculateFinalIndicatorsOnly =T, # The stage of data processing

            # Arguments to indicate the type of processing being done (local or on server)
            # Arguments to indicate the type of processing being done (local or on server)
            dataSource="central",
            outputType="mongodb",

            # Arguments used for processing local data sets
            central_url="https://github.com/l-gorman/rhomis-R-package/blob/end-to-end-example/inst/sample_central_project/sample-central-data.csv.zip?raw=true",
            central_email="test@domain.com",
            central_password="testpassword",
            project_name="test_project",
            form_name="test_form",
            form_version="test_version",
            isDraft = F,
            central_test_case=T,
            database="rhomis-test"
        ))

        TRUE

    },
    error=function(){
        traceback()
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

    testthat::expect_true(result)

   

})



testthat::test_that("Can process a whole local dataset",{

   
     result <- tryCatch({
        # Extract Units
        suppressWarnings(processData(
            extractUnitsOnly=T, # The stage of data processing

            # Arguments to indicate the type of processing being done (local or on server)
            dataSource="csv",
            outputType="csv",

            # Arguments used for processing local data sets
            base_path = "./", #' Path to the folder where the analysis needs to take place
            dataFilePath="https://raw.githubusercontent.com/l-gorman/rhomis-R-package/dev/inst/sample_local_project/raw-data/raw-data.csv",
            id_type="string",
            proj_id="test_project",
            form_id="test_form"
        ))

        # Calculate initial indicators
        suppressWarnings(processData(
            extractUnitsOnly = F,
            calculateInitialIndicatorsOnly = T, # The stage of data processing

            # Arguments to indicate the type of processing being done (local or on server)
            dataSource = "csv",
            outputType = "csv",

            # Arguments used for processing local data sets
            base_path = "./", #' Path to the folder where the analysis needs to take place
            dataFilePath = "https://raw.githubusercontent.com/l-gorman/rhomis-R-package/dev/inst/sample_local_project/raw-data/raw-data.csv",
            id_type = "string",
            proj_id = "test_project",
            form_id = "test_form"
        ))

        # Calculate Final Indicators
        suppressWarnings(processData(
            extractUnitsOnly = F,
            calculateFinalIndicatorsOnly =T, # The stage of data processing

            # Arguments to indicate the type of processing being done (local or on server)
            dataSource="csv",
            outputType="csv",

            # Arguments used for processing local data sets
            base_path = "./", #' Path to the folder where the analysis needs to take place
            id_type="string",
            proj_id="test_project",
            form_id="test_form"
        ))

        TRUE

    },
    error=function(){
        traceback()
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

    testthat::expect_true(result)



})


