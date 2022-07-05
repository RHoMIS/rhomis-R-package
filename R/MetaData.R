

new_indicator <- function(indicator_name,
                           output_format=c("column", "loop", "table"),
                           individual_columns_required=c(),
                           loop_columns_required=c(),
                           conversion_tables_required=c(),
                           api_data_required=c(),
                           indicators_required=c(),
                           function_calculated){

                             match.arg(output_format)
    stopifnot(
        is.character(indicator_name),
        is.character(function_calculated)

    )

    indicator <- list(
        indicator_name=indicator_name,
        format=format,
        individual_columns_required=individual_columns_required,
        loop_columns_required=loop_columns_required,
        conversion_tables_required=conversion_tables_required,
        api_data_required=api_data_required,
        indicators_required=indicators_required,
        file_located=function_calculated
    )


    return(indicator)
}


add_indicator <- function(indicator_list,
                        indicator_name,
                        output_format=c("column", "loop", "table"),
                        individual_columns_required=c(),
                        loop_columns_required=c(),
                        conversion_tables_required=c(),
                        api_data_required=c(),
                        indicators_required=c(),
                        function_calculated
                    ){


    indicator <- new_indicator(
    indicator_name = indicator_name,
    output_format=output_format,
    individual_columns_required=individual_columns_required, 
    loop_columns_required=loop_columns_required,
    conversion_tables_required=conversion_tables_required,
    api_data_required = api_data_required,
    indicators_required=indicators_required,
    function_calculated=function_calculated)

    if (indicator$indicator_name %in% names(indicator_list)){
        stop("Indicator already exists in list")
    }


    temp_list <- list(
        indicator=indicator
    )
    # names(temp_list) <- indicator$indicator_name

    return(
        append(indicator_list, temp_list)
    )
}
