library(rhomis)

base_path <- "./inst/sample_project/"
raw_data_path <- "./inst/sample_project/raw-data/Raw_Data.csv"


extract_values_local(
    base_folder=base_path,
    file_path=preprocessed_data_path,
    id_type="string",
    proj_id="test_project",
    form_id="test_project",
)

