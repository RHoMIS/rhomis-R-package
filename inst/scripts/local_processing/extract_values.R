library(rhomis)

file_path <- "./xxx"
overwrite <- T

rhomis_data <- load_rhomis_csv(
    file_path=file_path,
    country_column= "country",
    unique_id_col="_uuid",
    hh_id_col=NULL,
    id_type="string",
    proj_id="xxx",
    form_id="xxx"
)


new_values <- extract_values_by_project(rhomis_data)
new_values <- check_existing_conversions(list_of_df = new_values)

units_folder_dest <- "./original_units"
write_units_to_folder(list_of_df = new_values,
                      folder=units_folder_dest)

new_units_dest <- "./converted_units"

if (dir.exists(new_units_dest)==F){
    write_units_to_folder(list_of_df = new_values,
                          folder=new_units_dest)
}
if (overwrite==T){
write_units_to_folder(list_of_df = new_values,
                      folder=new_units_dest)
}



