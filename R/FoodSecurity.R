
#' FIES score
#'
#' @param data The datase containing all of the RHoMIS data (or just the FIES information)
#'
#' @return
#' @export
#'
#' @examples
fies_score <- function(data){

    number_of_FIES_questions <- 8
    column_prefix <- "FIES"
    FIES_columns <- paste0(column_prefix, "_", c(1:number_of_FIES_questions))

    fies_data <- data[FIES_columns]

    values <- c("Y","N")
    conversion <- c(1,0)

    fies_numeric <- switch_units(fies_data,values,conversion)
    fies_score <- rowSums(fies_numeric,na.rm = T)
    fies_score[rowSums(!is.na(fies_numeric))==0] <- NA

    return(fies_score)
}

hfias_score <- function(data){

    number_of_HFIAS_questions <- 9
    column_prefix <- "HFIAS"
    HFIAS_columns <- paste0(column_prefix, "_", c(number_of_HFIAS_questions:1))
    data <- data[HFIAS_columns]
    data[is.na(data)]<-"blank"

    hfias_rating <- apply(data, MARGIN = 1, function(x) row_wise_hfias(x))
    return(hfias_rating)
}

row_wise_hfias <- function(HFIAS_row)
{
    HFIAS_status<-'0'
    if  (HFIAS_row["HFIAS_9"]%in%c('daily','weekly','monthly')|HFIAS_row["HFIAS_8"]%in%c('daily','weekly','monthly')|HFIAS_row["HFIAS_7"]%in%c('daily','weekly','monthly')) {
        HFIAS_status<-'SeverelyFI'
    } else {
        if (HFIAS_row["HFIAS_6"]=='daily'|HFIAS_row["HFIAS_5"]=='daily') {
            HFIAS_status<-'SeverelyFI'
        }
        if (HFIAS_row["HFIAS_6"]%in%c('weekly','monthly')|HFIAS_row["HFIAS_5"]%in%c('weekly','monthly')) {
            HFIAS_status<-'ModeratelyFI'
        }
    }
    if (HFIAS_status=='0') {
        if (HFIAS_row["HFIAS_4"]%in%c('daily','weekly')|HFIAS_row["HFIAS_3"]%in%c('daily','weekly')) {
            HFIAS_status<-'ModeratelyFI'
        }
        if (HFIAS_row["HFIAS_4"]%in%c('monthly')|HFIAS_row["HFIAS_3"]%in%c('monthly')) {
            HFIAS_status<-'MildlyFI'
        }
    }
    if (HFIAS_status=='0') {
        if (HFIAS_row["HFIAS_2"]%in%c('monthly','daily','weekly')) {
            HFIAS_status<-'MildlyFI'
        }
    }
    if (HFIAS_status=='0') {
        if (HFIAS_row["HFIAS_1"]%in%c('daily','weekly')) {
            HFIAS_status<-'MildlyFI'
        }
    }
    if (HFIAS_status=='0') {
        HFIAS_status<-'FoodSecure'
    }
    return(HFIAS_status)
}

food_security_calculations <- function(data){
    HFIAS_columns <- paste0("HFIAS_",c(1:9))
    FIES_columns <- paste0("FIES_",c(1:8))

    if (all(HFIAS_columns%in%colnames(data)) & all(FIES_columns%in%colnames(data))==F){
        column_name <- "HFIAS_status"
        score <- hfias_score(data)
        data_to_return <- as_tibble(score)
        colnames(data_to_return) <-column_name
        return(data_to_return)
    }
    if(all(FIES_columns%in%colnames(data)) & all(HFIAS_columns%in%colnames(data))==F){
        column_name <- "FIES_score"
        score <- fies_score(data)
        data_to_return <- as_tibble(score)
        colnames(data_to_return) <-column_name
        return(data_to_return)
    }

    if(all(FIES_columns%in%colnames(data)) & all(HFIAS_columns%in%colnames(data))){
        fies_score_result <- fies_score(data)
        hfias_score_result <- hfias_score(data)
        data_to_return <- as_tibble(list("HFIAS_status"=hfias_score_result,
                                         "FIES_score"=fies_score_result))
        colnames(data_to_return) <-column_names
        return(data_to_return)
    }
}

