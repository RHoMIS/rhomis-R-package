library(tibble)


#' fies score
#'
#' @param data The data set containing all of the RHoMIS data (or just the fies information)
#'
#' @return
#' @export
#'
#' @examples
fies_score <- function(data){

    number_of_fies_questions <- 8
    column_prefix <- "fies"
    fies_columns <- paste0(column_prefix, "_", c(1:number_of_fies_questions))

    fies_data <- data[fies_columns]

    values <- c("y","n")
    conversion <- c(1,0)

    fies_numeric <- switch_units(fies_data,values,conversion)
    fies_score <- rowSums(fies_numeric,na.rm = T)
    fies_score[rowSums(!is.na(fies_numeric))==0] <- NA

    return(fies_score)
}

hfias_score <- function(data){

    number_of_hfias_questions <- 9
    column_prefix <- "hfias"
    hfias_columns <- paste0(column_prefix, "_", c(number_of_hfias_questions:1))
    data <- data[hfias_columns]
    data[is.na(data)]<-"blank"

    hfias_rating <- apply(data, MARGIN = 1, function(x) row_wise_hfias(x))
    return(hfias_rating)
}

row_wise_hfias <- function(hfias_row)
{
    hfias_status<-'0'
    if  (hfias_row["hfias_9"]%in%c('daily','weekly','monthly')|hfias_row["hfias_8"]%in%c('daily','weekly','monthly')|hfias_row["hfias_7"]%in%c('daily','weekly','monthly')) {
        hfias_status<-'severely_fi'
    } else {
        if (hfias_row["hfias_6"]=='daily'|hfias_row["hfias_5"]=='daily') {
            hfias_status<-'severely_fi'
        }
        if (hfias_row["hfias_6"]%in%c('weekly','monthly')|hfias_row["hfias_5"]%in%c('weekly','monthly')) {
            hfias_status<-'moderately_fi'
        }
    }
    if (hfias_status=='0') {
        if (hfias_row["hfias_4"]%in%c('daily','weekly')|hfias_row["hfias_3"]%in%c('daily','weekly')) {
            hfias_status<-'moderately_fi'
        }
        if (hfias_row["hfias_4"]%in%c('monthly')|hfias_row["hfias_3"]%in%c('monthly')) {
            hfias_status<-'mildly_fi'
        }
    }
    if (hfias_status=='0') {
        if (hfias_row["hfias_2"]%in%c('monthly','daily','weekly')) {
            hfias_status<-'mildly_fi'
        }
    }
    if (hfias_status=='0') {
        if (hfias_row["hfias_1"]%in%c('daily','weekly')) {
            hfias_status<-'mildly_fi'
        }
    }
    if (hfias_status=='0') {
        hfias_status<-'food_secure'
    }
    return(hfias_status)
}

#' Food Security Calculations All
#'
#' Calculate food security calculations
#' based on whether HFIAS or FIES data were collected in the RHoMIS
#' survey
#'
#' @param data Survey data containing either FIES or
#' HFIAS data
#'
#' @return
#' @export
#'
#' @examples
food_security_calculations <- function(data){
    hfias_columns <- paste0("hfias_",c(1:9))
    fies_columns <- paste0("fies_",c(1:8))

    if (all(hfias_columns%in%colnames(data)) & all(fies_columns%in%colnames(data))==F){
        column_name <- "hfias_status"
        score <- hfias_score(data)
        data_to_return <- tibble::as_tibble(score)
        colnames(data_to_return) <-column_name
        return(data_to_return)
    }
    if(all(fies_columns%in%colnames(data)) & all(hfias_columns%in%colnames(data))==F){
        column_name <- "fies_score"
        score <- fies_score(data)
        data_to_return <- tibble::as_tibble(score)
        colnames(data_to_return) <-column_name
        return(data_to_return)
    }

    if(all(fies_columns%in%colnames(data)) & all(hfias_columns%in%colnames(data))){
        fies_score_result <- fies_score(data)
        hfias_score_result <- hfias_score(data)
        data_to_return <- tibble::as_tibble(list("hfias_status"=hfias_score_result,
                                         "fies_score"=fies_score_result))
        colnames(data_to_return) <-column_names
        return(data_to_return)
    }
}



