pkg.env = new.env()

.onLoad <- function(libname, pkgname){

    set_repeat_column_names()
    set_gender_categories()
    set_local_units_file_list()

}


