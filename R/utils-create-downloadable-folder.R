
#' Write Docs
#'
#' Write a readme for a RHoMIS
#' dataset which has been automatically
#' generated
#'
#' @param project_name The name of the project
#' @param form_name The name of the form
#'
#' @return
#' @export
#'
#' @examples
write_docs <- function(project_name, form_name){

    date_time <- Sys.time()

    string_message <- sprintf("
project: %s
form: %s

processed_date: %s

This folder contains all of the outputs
automatically generated from the RHoMIS
2.0 system.

For information on how to reprocess this
data, please see here:

https://rhomis.github.io/rhomis-R-package/data-processing.html

For more information on the outputs produced
please see here:

https://rhomis.github.io/rhomis-R-package/outputs-and-data.html

For more information on the indicators, and how they link to
one another, please see here:

https://rhomis.github.io/rhomis-R-package/indicators-explained.html

Finally, if you encounter any issues, please report them here:

https://github.com/RHoMIS/rhomis-support/issues

        ", project_name, form_name, date_time)

    return(
        string_message

    )

}
