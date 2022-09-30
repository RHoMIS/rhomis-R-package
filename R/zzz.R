pkg.env <- new.env()

.onLoad <- function(libname, pkgname) {


  set_repeat_column_names()
  set_gender_categories()
  # set_local_units_file_list()

  set_conversion_file_names()

  set_produce_list()
  set_identification_columns()


  set_secondary_units()
  set_prices_list()
  set_calories_list()
}
