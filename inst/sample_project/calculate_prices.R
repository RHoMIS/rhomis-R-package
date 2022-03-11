library(rhomis)

file_path <- "./inst/sample_project/raw-data/Raw_Data.csv"


rhomis_data <- load_rhomis_csv(
    file_path=file_path,
    id_type="string",
    proj_id="snv_pro_arides",
    form_id="snv_pro_arides_1",
    repeat_column_names = c("crop_repeat",
                            "livestock_repeat",
                            "offfarm_repeat",
                            "offfarm_income_repeat",
                            "hh_pop_repeat",
                            "hh_rep",
                            "services_income_repeat")
)

results <- calculate_prices_and_indicator_local(data = rhomis_data,base_path="./inst/sample_project/")

