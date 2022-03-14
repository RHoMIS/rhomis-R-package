
#' This function is run on load (see zzz.R) to set the default list of repeated columns when reading in a new rhomis dataset.
set_repeat_column_names <- function(){

    assign("repeat_columns",
           c("crop_repeat",
             "livestock_repeat",
             "offfarm_repeat",
             "offfarm_income_repeat",
             "hh_pop_repeat",
             "hh_rep"
             ),
           envir = pkg.env)

    return()
}
