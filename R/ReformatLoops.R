library(tidyverse)




#' Find number of loops
#'
#' The RHoMIS data is arranged in a looping structure.
#' This function helps identify how many loops there are
#' for example for a variable like "crop_name".
#'
#' @param data The data containing the loops
#' @param name_column The "nam" column for the loops you are looking at, for example "crop_name"
#'
#' @return A single number, the number of loops for that variable
#' @export
#'
#' @examples
#' name_column <- "crop_name"
#' data <- as_tibble(list(crop_name_1=c("banana", "cassava", NA, "millet"),
#'                        crop_name_2=c("cassava",NA, "melon", "maize"),
#'                        random_crop_name_2=c("blue", "green",  "red",NA),
#'                        crop_name=c("orange", "purple", NA, "black")))
#' find_number_of_loops(data,name_column)
#'
find_number_of_loops <- function(data,name_column){

    regex_pattern <- paste0("^",name_column,"_[[:digit:]]") # Finding columns which start with "column_pattern_Integer"
    relevant_columns <- grep(regex_pattern, colnames(data), value=T)
    number_of_loops <- length(relevant_columns)
    return(number_of_loops)
}


#' Find Unique Names
#'
#' A function to find all of the unique names for a particular looped variable (e.g. livestock, crops, off-farm incomes)
#'
#' @param data The data-frame containing the loops of concern
#' @param name_column The original name of the loop (e.g. "crop_name")
#' @param number_of_loops The number of loops for the variable of concern
#'
#' @return A list of all of the unique entries for the name column
#' @export
#'
#' @examples
#' name_column <- "crop_name"
#' data <- as_tibble(list(crop_name_1=c("banana", "cassava", NA, "millet"),
#'                        crop_name_2=c("cassava",NA, "melon", "maize"),
#'                        random_crop_name_2=c("blue", "green",  "red",NA),
#'                        crop_name=c("orange", "purple", NA, "black")))
#' expected_result <- c("banana","cassava","millet","melon","maize")
#' find_unique_case(data, name_column)


find_unique_case <- function(data, name_column){

    number_of_loops <- find_number_of_loops(data,name_column)

    name_columns <- data[,paste0(name_column,"_",1:number_of_loops)]

    # Finding all of the unique values
    all_values <- unname(unlist(lapply(name_columns, function(x) unique(x))))
    # Removing NA column
    all_values <- all_values[!is.na(all_values)]
    # Finding unique values from all the columns
    unique_values <- unique(all_values)
    return(unique_values)
}


#' Item Number to Column Row Conversion
#'
#' Converting from loop to a format that is easy to analyze.
#'
#' @param data The data containing the loops you need to convert
#' @param name_column The column containing the variable which will eventually
#' column header (see example)
#' @param variable_to_convert What is the variable you would like to link to the name column
#' @param type What is the type of variable you are expecting? "chr" for character,
#' "num" for numeric, "int" for interger, and "fct" for factor.
#'
#'
#' @return
#' @export
#'
#' @examples
#'
#' name_column <- "crop_name"
#' variable_to_convert <- "crop_variable"
#' data <- as_tibble(list(crop_name_1=c("banana", "cassava", NA, "millet"),
#'                        crop_name_2=c("cassava",NA, "melon", "maize"),
#'                        random_crop_name_2=c("blue", "green",  "red",NA),
#'                        crop_name=c("orange", "purple", NA, "black"),
#'                        crop_variable_1=c("ex1","ex2",NA,NA),
#'                        crop_variable_2=c("ex3",NA,"ex4","ex5")))
#' number_of_loops <- find_number_of_loops(data,name_column)
#' loop_to_column_conversion(data, name_column, variable_to_convert)
#'
loop_to_column_conversion <- function(data, name_column, variable_to_convert, type){
    # name_column <- "offfarm_income_name"
    # variable_to_convert <-"offfarm_month"
    # number_of_loops<- 6
    # unique_names <- find_unique_case(data=data, name_column=name_column, number_of_loops=number_of_loops)


    unique_names <- find_unique_case(data, name_column)

    # Obtaining a table of the loop values
    value_table <- data[,paste0(variable_to_convert,"_",1:number_of_loops)]
    value_table$indexValues <- row.names(value_table)
    value_table <- value_table %>% group_by(indexValues) %>% gather(key = "column", value = "value", -indexValues)

    # Obtaining a table of the loop names
    name_table <- data[,paste0(name_column,"_",1:number_of_loops)]
    name_table$indexNames <- row.names(name_table)
    name_table <- name_table %>% group_by(indexNames) %>% gather(key = "column", value = "name", -indexNames)

    # Merging table of loop values, names and indexes
    merged_table <- cbind(name_table[,c("name","indexNames")],value_table[,c("indexValues","value")])

    if(!all(merged_table$indexNames==merged_table$indexValues))
    {
        stop("Indexes don't match when merging loop information")
    }

    # Converting the long table to wide format
    merged_table$index <- merged_table$indexNames
    merged_table <- merged_table[,c("index", "name", "value")]
    merged_table<- merged_table[!duplicated(merged_table[,c("index", "name")]),]
    merged_table <- merged_table %>%  group_by(index) %>% pivot_wider(id_cols = index,names_from = name, values_from = value)
    if(!all(merged_table$index==row.names(merged_table))){
        stop("Indexes don't match when merging loop information")

    }

    merged_table <- as_tibble(merged_table[,!colnames(merged_table) %in% c("NA","index")])
    if (type=="chr")
    {
        merged_table <- merged_table %>% mutate_all(as.character)
    }
    if (type=="num")
    {
        merged_table <- merged_table %>% mutate_all(as.numeric)
    }
    if (type=="int")
    {
        merged_table <- merged_table %>% mutate_all(as.integer)
    }
    if (type=="fct")
    {
        merged_table <- merged_table %>% mutate_all(as.factor)
    }
    return(merged_table)
}


#' Map To Wide Format
#'
#' A function to convert RHoMIS loops into a wider named format
#'
#' @param data The original dataframe containing the data to be reformatted
#' @param name_column The name column (e.g. crop_name_1)
#' @param number_of_loops The number of loops for this particular parameter
#' @param other_column_prefixes The other columns which need to be exported to a wider named format
#'
#' @return a list of data frames for each of the variables
#' @export
#'
#' @examples
map_to_wide_format <- function(data, name_column, number_of_loops, column_prefixes) {

    # data<-dat_all
    # name_column <-"offfarm_income_name"
    # number_of_loops <- number_of_loops
    # column_prefixes <- column_prefixes

    unique_names <- find_unique_case(data=data, name_column=name_column, number_of_loops=number_of_loops)
    if (length(unique_names)==1)
    {
        unique_names[unique_names=="NA"]<-NA
        if (is.na(unique_names))
        {
            data[1:nrow(data),paste0(name_column,"_",1:number_of_loops)]<-"none"
        }
    }


    reformatted_variables <- lapply(column_prefixes, function(x)
        loop_to_column_conversion(data = data,
                                  name_column = name_column,
                                  variable_to_convert=x,
                                  number_of_loops = number_of_loops,
                                  unique_names = unique_names ))
    names(reformatted_variables)<-column_prefixes




    return (reformatted_variables)
}


### Gender Splits for the dats

prop_or_na <- function(item){
    if(length(item)==1)
    {
        if(is.na(item)){
            return(NA)
        }
    }
    return(1/length(item))

}

number_controlling_resource <- function(item){

    #item <-offFarmLoopReformatted$offfarm_who_control_revenue$otherfarms


    item <- strsplit(item, " ")
    # Avoiding Duplicates
    item <- lapply(item, function(x) unique(x))
    # Counting number of people controlling
    item <- unlist(lapply(item, function(x) prop_or_na(x)))
    return(item)
}

check_val_in_list <- function(item, category){
    # "female_youth",
    # "female_adult",
    # "male_youth",
    # "male_adult"
    #category <- "male_adult"
    #item <- genderdf$otherfarms
    item <- strsplit(item, " ")
    item <-  unlist(lapply(item, function(x) category %in% x))
    as.numeric(item)

    return (as.numeric(item))

}

gender_control_props <- function(genderdf,numberControllingDF, category){
    #category <- "female_adult"
    #numberControllingDF

    # genderdf<-genderdf
    # numberControllingDF<-numberPeopleControlling
    # category<-"male_adult"
    value <- genderdf %>% mutate(across(.cols=everything(),~check_val_in_list(item=.x, category=category)))
    return (as_tibble(value*numberControllingDF))
}


split_gender_data <- function(genderdf){
    #genderdf <- offFarmLoopReformatted$offfarm_who_control_revenue
    categories <- c("female_youth",
                    "female_adult",
                    "male_youth",
                    "male_adult")
    #genderdf <- offFarmLoopReformatted$offfarm_who_control_revenue
    numberPeopleControlling <- genderdf %>% mutate(across(.cols=everything(),~number_controlling_resource(.x)))

    genderControlDFs <- lapply(categories, function(x) gender_control_props(genderdf=genderdf,numberControllingDF=numberPeopleControlling, category=x))
    names(genderControlDFs)<-categories
    return(genderControlDFs)


}


# number_of_loops<-length(grep("offfarm_income_name_", colnames(dat_all)))
# column_prefixes <- c(
#     "offfarm_income_name",
#     # "offfarm_label",
#     "offfarm_year_round",
#     "offfarm_month",
#     "offfarm_who_control_revenue"
# )
#
#
#
#
# offFarmLoopReformatted <- map_to_wide_format(data=dat_all,
#                                           name_column ="offfarm_income_name",
#                                           number_of_loops = number_of_loops,
#                                           column_prefixes = column_prefixes)
#
# offFarmLoopReformatted$offfarm_who_control_revenue
#
#
#
# offFarmGenders <- split_gender_data(genderdf = offFarmLoopReformatted$offfarm_who_control_revenue)
#
#
