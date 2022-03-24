


#' Create a new RHoMIS module with Analysis scripts
#'
#' Create a module to save in the RHoMIS database
#'
#' @param module_name The name of the module you would like to add
#' @param module_type The type of module to add (e.g. core,optional_module, tailored)
#' @param processing_code The code used to process the modules
#' @param dependencies Which modules this module depends on
#'
#' @return
#' @export
#'
#' @examples
create_module <- function(module_name, module_type, processing_code, dependencies) {
  module_name <- tolower(module_name)
  #
  result <- tibble::as_tibble(list(
    module_name = module_name,
    module_type = module_type,
    processing_code = processing_code,
    dependencies = dependencies
  ))

  return(result)
}

merge_modules <- function(modules) {

}

test_module <- function(module, module_test) {

}
