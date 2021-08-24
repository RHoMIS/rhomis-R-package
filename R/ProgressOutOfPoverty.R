library(tibble)

#' PPI score
#'
#' This function calculates the ppi score based off of a typical RHoMIS
#' dataset. It relies on conversion sheets and information which can be found
#' in the "data" folder of the package.
#'
#' @param data The data set containing the PPI columns
#' @param country_code_column The country codes for the data-sets concerned
#'
#' @return
#' @export
#'
#' @examples
#' data <- tibble::as_tibble(list("PPI_1"=c(5,1,3,4),
#'                        "PPI_2"=c(3,12,7,18),
#'                        "PPI_3"=c(17,1,2,3),
#'                        "PPI_4"=c(NA,2,1,6),
#'                        "PPI_5"=c(1,NA,3,1),
#'                        "PPI_6"=c(3,7,4,1),
#'                        "PPI_7"=c(8,3,NA,2),
#'                        "PPI_8"=c(3,8,1,3),
#'                        "PPI_9"=c(9,2,8,5),
#'                        "PPI_10"=c(12,4,7,1),
#'                        "random_other_column"=c(NA,NA,NA,NA)))
#'
#' country_code_column <- c("VN","VN","KE","KE")
#' ppi_score(data,country_code_column)

ppi_score <- function(data, country_code_column){

    colnames(data) <- tolower(colnames(data))
    ppi_columns <- paste0("ppi_",1:10)

    if (all(ppi_columns%in% colnames(data)))
    {
        ppi_data <- data[ppi_columns]
        ppi_score <- rowSums(ppi_data,na.rm = T)

        country_code_column<-toupper(country_code_column)
        ppi_limit_column <- unname(sapply(country_code_column, function(x) identify_ppi_limit(x)))
        ppi_likelihood <- unname(unlist(sapply(c(1:length(ppi_score)),function(x) identify_ppi_conversion(ppi_score[x],country_code_column[x]))))

        ppi_info <- tibble::as_tibble(list(ppi_likelihood=ppi_likelihood,
                                   ppi_limit=ppi_limit_column))
    }
    return(ppi_info)

}

identify_ppi_conversion <- function(score, country_code){
    score_to_return <- ppi_score_card[ppi_score_card$Score==score,paste0(paste0("PPI_Likelihood_",country_code))]
    return(as.numeric(score_to_return))
}

identify_ppi_limit <- function(country_code){
    ppi_limit <- ppi_limits$limit[ppi_limits$country==country_code]
    return(ppi_limit)
}

