#' New Indicator
#' 
#' Create a new indicator
#' @param indicator_list The Existing list of indicators
#' @param indicator_name The name of the new indicator
#' @param output_format The shape of the indicator (column, loop or table)
#' @param individual_columns_required Which individual columns are required (directly) for the RHoMIS dataset
#' @param loop_columns_required Which looped columns are required
#' @param conversion_tables_required Which conversion tables are required
#' @param api_data_required Which api connection data is required
#' @param indicators_required Other indicators which are required to calculate this one
#' @param function_calculated The function where this indicator is calculated 
#' @param search_term The search term you can use to find where this indicator is calculated
#' 
#' @return
#' @export
#'
#' @examples
new_indicator <- function(indicator_name,
                           output_format=c("column", "loop", "table"),
                           individual_columns_required=list(),
                           loop_columns_required=list(),
                           conversion_tables_required=list(),
                           api_data_required=list(),
                           indicators_required=list(),
                           function_calculated,
                           search_term){

    match.arg(output_format)
    stopifnot(
        is.character(indicator_name),
        is.character(output_format),
        is.character(search_term),

        is.list(individual_columns_required),
        is.list(loop_columns_required),        
        is.list(conversion_tables_required),        
        is.list(api_data_required),        
        is.list(indicators_required)     
        
    )
    environment <- as.list(environment())
    args <- as.list(match.call())

    function_args <- environment[names(args)]

    args <- lapply(function_args, function(x){
        type_check(x)
                           })




    indicator <- list(
        indicator_name=args$indicator_name,
        output_format=args$output_format,
        individual_columns_required=args$individual_columns_required,
        loop_columns_required=args$loop_columns_required,
        conversion_tables_required=args$conversion_tables_required,
        api_data_required=args$api_data_required,
        indicators_required=args$indicators_required,
        function_calculated=args$function_calculated,
        search_term=args$search_term
    )


    return(indicator)
}


#' Add Indicator
#' 
#' Add an indicator metadata entry for 
#' the RHoMIS dataset
#' 
#' @param indicator_list The Existing list of indicators
#' @param indicator_name The name of the new indicator
#' @param output_format The shape of the indicator (column, loop or table)
#' @param individual_columns_required Which individual columns are required (directly) for the RHoMIS dataset
#' @param loop_columns_required Which looped columns are required
#' @param conversion_tables_required Which conversion tables are required
#' @param api_data_required Which api connection data is required
#' @param indicators_required Other indicators which are required to calculate this one
#' @param function_calculated The function where this indicator is calculated 
#' @param search_term The search term you can use to find where this indicator is calculated
#' 
#' @return
#' @export
#'
#' @examples
add_indicator <- function(indicator_list,
                        indicator_name,
                        output_format=c("column", "loop", "table"),
                        individual_columns_required=list(),
                        loop_columns_required=list(),
                        conversion_tables_required=list(),
                        api_data_required=list(),
                        indicators_required=list(),
                        function_calculated,
                        search_term
                    ){
    
    
    
    indicator <- new_indicator(
    indicator_name = indicator_name,
    output_format=output_format,
    individual_columns_required=individual_columns_required, 
    loop_columns_required=loop_columns_required,
    conversion_tables_required=conversion_tables_required,
    api_data_required = api_data_required,
    indicators_required=indicators_required,
    function_calculated=function_calculated,
    search_term=search_term)

    # print(indicator)

    if (indicator$indicator_name %in% names(indicator_list)){
        stop("Indicator already exists in list")
    }

    indicator_list[[indicator$indicator_name]] <- indicator

    
    # names(temp_list) <- indicator$indicator_name

    return(
        indicator_list
    )
}

#' Add Gendered indicator
#' 
#' @param indicator_list The Existing list of indicators
#' @param gendered_list List of gender categories
#' @param indicator_name The name of the new indicator (without gender prefix)
#' @param output_format The shape of the indicator (column, loop or table)
#' @param individual_columns_required Which individual columns are required (directly) for the RHoMIS dataset
#' @param loop_columns_required Which looped columns are required
#' @param conversion_tables_required Which conversion tables are required
#' @param api_data_required Which api connection data is required
#' @param indicators_required Other indicators which are required to calculate this one
#' @param function_calculated The function where this indicator is calculated 
#' 
#' @return
#' @export
#'
#' @examples
add_gendered_indicator <- function(
     indicator_list,
     gendered_list=c("male_youth", "male_adult", "female_youth", "female_adult"),
     indicator_name = "male_youth_crop_consumed_kg_per_year",
     output_format, #column, loop, #table
     individual_columns_required=list(), 
     loop_columns_required=list(),
     conversion_tables_required=list(),
     api_data_required = list(),
     indicators_required=list(),
     function_calculated="crop_gender_calculations"
){

     for (gender in gendered_list){

          gender_indicator_name <- paste0(gender, "_", indicator_name)
          search_term <- paste0("indicator_search_", gender_indicator_name)

          indicator_list

          indicator_list <- add_indicator(
               indicator_list,
               indicator_name = gender_indicator_name,
               output_format=output_format, #column, loop, #table
               individual_columns_required=individual_columns_required, 
               loop_columns_required=loop_columns_required,
               conversion_tables_required=conversion_tables_required,
               api_data_required = api_data_required,
               indicators_required=indicators_required,
               function_calculated=function_calculated,
               search_term=search_term)
     }
     return(indicator_list)
}


type_check <- function(object){

    if (class(object)=="character"){
        return(object)
    }
    
    if (class(object)=="list")
    {
        if(length(object)==0){
            return(object)
        }

        if(length(object)>=1){
            return(unlist(object))
        }
    }

}

#' Find Nested Dependencies
#' 
#' In the RHoMIS datasets, there are indicators
#' which depend on indicators, indicators which depend
#' on individual columns, and indicators which depend on columns
#' embedded in loops. It is essential that we know how every 
#' indicator is linked in the data so that we can understand 
#' issues in survey design. This function tries to achieve this.
#' 
#' @param indicator_name The name of the indicator
#' @param indicator_list The list of indicators
#' @param dependency_required
find_nested_dependencies_list <- function(
    indicator_name, 
    indicator_list, 
    dependency_required=c("individual", "loop", "indicator", "conversion")
    ){
      if (is.character(indicator_name)==F){
          stop("indicator_name must be a character")
     }

     match.arg(dependency_required)
     
      if (is.character(indicator_name)==F){
          stop("indicator_name must be a character")
     }

     if (is.list(indicator_list)==F){
          stop("indicator_list must be a list")
     }
     if (indicator_name %in% names(indicator_list)==F){
          stop("Could not find indicator name in list")
     }

     

     indicator <- indicator_list[[indicator_name]]

    columns_required <- c()
     if (dependency_required=="individual"){
          columns_required <- c(indicator$individual_columns_required)
     }

     if (dependency_required=="list"){
          columns_required <- c(indicator$loop_columns_required)
     }

     if (dependency_required=="indicator"){
          columns_required <- c(indicator$indicators_required)
     }
     
     if (dependency_required=="conversion"){
          columns_required <- c(indicator$conversion_tables_required)
     }

     if (length(indicator$indicators_required)==0){
          return(columns_required)
     }

if (length(indicator$indicators_required)>0){
          dependent_columns_required <- lapply(indicator$indicators_required, function(indicator_name){
               find_nested_dependencies_list(
                    indicator_name=indicator_name, 
                    indicator_list=indicator_list, 
                    dependency_required=dependency_required)
          })  %>% unlist()


          columns_required <- unlist(c(columns_required, dependent_columns_required))
          return(columns_required)
     }
  
}



#' Find Nested Dependencies
#' 
#' In the RHoMIS datasets, there are indicators
#' which depend on indicators, indicators which depend
#' on individual columns, and indicators which depend on columns
#' embedded in loops. It is essential that we know how every 
#' indicator is linked in the data so that we can understand 
#' issues in survey design. This function tries to achieve this.
#' 
#' @param indicator_name The name of the indicator
#' @param indicator_list The list of indicators
#' 
#' @return
#' @export
#'
#' @examples
find_nested_dependencies_network <- function(
    indicator_name, 
    indicator_list){
      if (is.character(indicator_name)==F){
          stop("indicator_name must be a character")
     }


      if (is.character(indicator_name)==F){
          stop("indicator_name must be a character")
     }

     if (is.list(indicator_list)==F){
          stop("indicator_list must be a list")
     }
     if (indicator_name %in% names(indicator_list)==F){
          stop("Could not find indicator name in list")
     }

     

     indicator <- indicator_list[[indicator_name]]

    data_to_return <- tibble::as_tibble(
        list(
            from=character(),
            from_id=character(),
            to=character(),
            to_id=character(),
            raw=logical(),
            loop=logical(),
            indicator=logical()
        )
    )


     if (length(indicator$individual_columns_required)>0){
          data_to_return <- data_to_return  %>% 
          tibble::add_row(
            from=indicator$indicator_name,
            from_id=paste0(indicator$indicator_name,"_indicator"),
            to=indicator$individual_columns_required,
            to_id=paste0(indicator$individual_columns_required,"_raw"),
            raw=T,
            loop=F,
            indicator=F
          )

     }

     if (length(indicator$loop_columns_required)>0){
          data_to_return <- data_to_return  %>% 
          tibble::add_row(
            from=indicator$indicator_name,
            from_id=paste0(indicator$indicator_name,"_indicator"),
            to=indicator$loop_columns_required,
            to_id=paste0(indicator$loop_columns_required,"_raw"),
            raw=T,
            loop=T,
            indicator=F
          )
     }

     if (length(indicator$indicators_required)>0){
          data_to_return <- data_to_return  %>% 
          tibble::add_row(
            from=indicator$indicator_name,
            from_id=paste0(indicator$indicator_name,"_indicator"),
            to=indicator$indicators_required,
            to_id=paste0(indicator$indicators_required,"_indicator"),
            raw=F,
            loop=F,
            indicator=T
          )
     }
     
     if (length(indicator$indicators_required)==0){
          return(data_to_return)
     }

if (length(indicator$indicators_required)>0){
          new_data_to_return <- lapply(indicator$indicators_required, function(indicator_name){
               find_nested_dependencies_network(
                    indicator_name=indicator_name, 
                    indicator_list=indicator_list)
          })  %>% dplyr::bind_rows()


          data_to_return <- data_to_return %>% dplyr::bind_rows(new_data_to_return)
          return(data_to_return)
     }
  
}


find_d3_dependencies_network <- function(
    indicator_name, 
    indicator_list,
    d3_list=list()){

    if (is.character(indicator_name)==F){
          stop("indicator_name must be a character")
     }


      if (is.character(indicator_name)==F){
          stop("indicator_name must be a character")
     }

     if (is.list(indicator_list)==F){
          stop("indicator_list must be a list")
     }
     if (indicator_name %in% names(indicator_list)==F){
          stop("Could not find indicator name in list")
     }

     

    indicator <- indicator_list[[indicator_name]]
    d3_list[["name"]] <- indicator$indicator_name
    d3_list[["children"]] <- c()

     if (length(indicator$individual_columns_required)>0){
            new_children <- lapply(indicator$individual_columns_required, function(x){
                return(list(name=x))
            })
          d3_list[["children"]] <- append(d3_list$children,new_children)
     }

     if (length(indicator$loop_columns_required)>0){
        new_children <- lapply(indicator$loop_columns_required, function(x){
                return(list(name=x))
            })
          d3_list[["children"]] <- append(d3_list$children,new_children)
     }

     if (length(indicator$conversion_tables_required)>0){
        new_children <- lapply(indicator$conversion_tables_required, function(x){
                return(list(name=x))
            })
          d3_list[["children"]] <- append(d3_list$children,new_children)
     }

       if (length(indicator$api_data_required)>0){
        new_children <- lapply(indicator$api_data_required, function(x){
                return(list(name=x))
            })
          d3_list[["children"]] <- append(d3_list$children,new_children)
     }

     

    if (length(indicator$indicators_required)>0){
          children <- lapply(indicator$indicators_required, function(indicator_name){
               find_d3_dependencies_network(
                    indicator_name=indicator_name, 
                    indicator_list=indicator_list,
                    d3_list=d3_list)
          })  
        
         d3_list$children <- append(d3_list[["children"]],children)



     }
    
    return(d3_list)
  
}


#' Plot Dependency Network
#' 
#' Plot the dependency network of RHoMIS Indicators
#' 
#' @param indicator_name
#' @param indicator_list
#' 
#' @return
#' @export
#'
#' @examples
plot_dependency_network <- function(
    indicator_name,
    indicator_list,
    type="horizontal"
    ){

    d3_network <- find_d3_dependencies_network(
        indicator_name=indicator_name, 
        indicator_list=indicator_list

    )


    if (type=="horizontal"){
        plot <- networkD3::diagonalNetwork(List = d3_network)
    }

    if (type=="central"){
        plot <- networkD3::radialNetwork(List = d3_network)
    }


    


}




add_function_to_list <- function(
        function_list,
        function_name,
        called_by){

    stopifnot(
        is.character(function_name),
        is.character(called_by) | is.null(called_by)
    )

    if (function_name %in% names(function_list)){
        stop("Funciton already exists in list")
    }

    if (called_by %in% names(function_list)==F & is.null(called_by)==F){
        stop("Trying to say that it is called by a function that you have not added to this list")
    }
    
    if (is.null(called_by)){
        warning("Set called by to null, only do this in the case that this is a top level function")
    }

    new_function <- list(
        name=function_name,
        called_by=called_by    
    )

    function_list[[function_name]] <- new_function

    return(
        function_list
    )
}


get_function_stack <- function(
    function_list,
    function_name,
    d3_list=list()
){

    if (function_name %in% names(function_list)==F){
        stop("Could not find function you were looking for")
    }
    
    new_function <- function_list[[function_name]]
    
    d3_list[["name"]] <- new_function$name
    d3_list[["children"]] <- c()

    
    
    if (is.null(new_function$called_by)){
        return(d3_list)
    }

    if (length(new_function$called_by)>0 & !is.null(new_function$called_by)){
          children <-list(get_function_stack(
                    function_list=function_list, 
                    function_name=new_function$called_by,
                    d3_list=d3_list))
        }  
        
         d3_list$children <- append(d3_list[["children"]],children)

    return(d3_list)

     }
    
 

plot_function_dependency <- function(
    function_list,
    function_name,
    type="horizontal"
){


    d3_network <- get_function_stack(
            function_list=function_list,
            function_name=function_name,

        )

    if (type=="horizontal"){
    networkD3::diagonalNetwork(List = d3_network)
    }

    if (type=="central"){
        networkD3::radialNetwork(List = d3_network)
    }



}





