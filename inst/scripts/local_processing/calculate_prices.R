library(rhomis)

file_path <- "./"

rhomis_data <- load_rhomis_csv(
    file_path=file_path,
    country_column= "country",
    unique_id_col="_uuid",
    hh_id_col=NULL,
    id_type="string",
    proj_id="xxx",
    form_id="xxx"
)

results <- calculate_prices_and_indicator_local(data = rhomis_data,base_path="./",units_path = "./")
