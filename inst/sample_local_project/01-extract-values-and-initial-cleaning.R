# Example of how to process a raw dataset
library(rhomis)

new_units <- processData(
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
    )


