library(rhomis)

initial_processed_data <- processData(
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
)
