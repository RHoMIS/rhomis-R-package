library(rhomis)

file_path <- ""

rhomis_data <- load_rhomis_csv()

units <- load_conversions_csv()

prices <- load_prices()

outputs <- calculate_indicators()
