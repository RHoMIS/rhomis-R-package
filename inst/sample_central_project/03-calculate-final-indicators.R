library(rhomis)

initial_processed_data <- processData(
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
)
sho