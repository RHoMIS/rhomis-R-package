

general_check <- function(indicators, survey_data, indicator_data=NULL){



      survey_data

}

#' Dependency Check
#'
#' Check the the data for an indicator dependency
#' for a particular set of rows
#'
#' @param row_indexes Rows you are interested in
#' @param indicator Indicator interested in
#' @param survey_data Survey Dataset
#' @param indicator_data Indicator sheet
#'
#' @return
#' @export
#'
#' @examples
dependency_check <- function(row_indexes, indicator, survey_data, indicator_data=NULL){
    result <- list()

    if (indicator %in% names(indicator_list)==F){
        warning(paste0(indicator_data, "is not listed as an indicator in this package. If you expect to be, please contact package developers"))
        return("")
    }

    row_indexes <- c(1:10)
    indicator_details <- indicator_list[[indicator]]
    looped_dependencies <- find_nested_dependencies_list(indicator_name = indicator,
                                                         indicator_list = indicator_list,
                                                         dependency_required = "loop" )
    looped_dependencies <- unique(looped_dependencies)
    if (indicator_details$output_format=="loop"){
        looped_dependencies <- c(looped_dependencies,indicator)
    }



    individual_dependencies <- find_nested_dependencies_list(indicator_name = indicator,
                                                         indicator_list = indicator_list,
                                                         dependency_required = "individual" )
    if (indicator_details$output_format=="column"){
        individual_dependencies <- c(individual_dependencies,indicator)
    }

    indicator_dependencies <- find_nested_dependencies_list(indicator_name = indicator,
                                                             indicator_list = indicator_list,
                                                             dependency_required = "indicator" )


    looped_values <- survey_data[row_indexes, c("id_rhomis_dataset","survey_id")]
    if (length(looped_dependencies)>0){


        name_column <- grep("name",looped_dependencies, value=T)
        name_column <- unique(name_column)
        number_of_loops <- find_number_of_loops(survey_data, name_column = name_column)
        all_looped_columns <- lapply(c(1:number_of_loops), function(loop_number){
            dependencies <- paste0(looped_dependencies,"_",loop_number)
        }) %>% unlist()

        looped_values <- cbind(looped_values,survey_data[row_indexes, all_looped_columns[all_looped_columns %in%colnames(survey_data)]])
        looped_values <- cbind(looped_values,indicator_data[row_indexes, all_looped_columns[all_looped_columns %in%colnames(indicator_data)]])


    }

    individual_values <- survey_data[row_indexes, c("id_rhomis_dataset","survey_id")]
    if (length(individual_dependencies)>0){
        individual_values <- cbind(individual_values,survey_data[row_indexes, individual_dependencies[individual_dependencies %in%colnames(survey_data)]])
        individual_values <- cbind(individual_values,indicator_data[row_indexes, individual_dependencies[individual_dependencies %in%colnames(indicator_data)]])


    }

    indicator_values <- survey_data[row_indexes, c("id_rhomis_dataset","survey_id")]

    if (length(indicator_dependencies)>0){

        # dependency_check(row_indexes, "crop_sold_kg_per_year", survey_data, indicator_data  )
        # dependency_check(row_indexes, "crop_sold_units_numeric", survey_data, indicator_data  )
        # dependency_check(row_indexes, "crop_harvest_kg_per_year", survey_data, indicator_data )
        # dependency_check(row_indexes, "crop_yield_units_numeric", survey_data, indicator_data )


        new_indicator_values <- lapply(indicator_dependencies, function(sub_indicator){
            dependency_check(row_indexes, sub_indicator, survey_data, indicator_data )
        })



        if (length(new_indicator_values)==1){
            indicator_values <- indicator_values %>% merge(new_indicator_values)
        }
        if (length(new_indicator_values)>1){
            new_indicator_values <-Reduce(function(dtf1, dtf2) merge(dtf1, dtf2),new_indicator_values)

            indicator_values <- indicator_values %>% merge(new_indicator_values)

        }

        # indicator_values <- dependency_check(row_indexes, indicator, survey_data, indicator_data, result )

    }

    final_values <- looped_values

    final_values <- cbind(final_values, individual_values[colnames(individual_values) %in% colnames(final_values)==F])
    final_values <- cbind(final_values, indicator_values[colnames(indicator_values) %in% colnames(final_values)==F])

    order_of_cols <- c(colnames(indicator),colnames(survey_data))

    final_values <- final_values[order_of_cols[order_of_cols %in% colnames(final_values)]]


    return(final_values)
}

