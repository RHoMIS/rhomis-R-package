

#' PPI score
#'
#' This function calculates the ppi score based off of a typical RHoMIS
#' dataset. It relies on conversion sheets and information which can be found
#' in the "data" folder of the package.
#'
#' Rpackage file: ProgressOutOfPoverty.R
#'
#' @param data The data set containing the PPI columns
#' @param country_code_column The country codes for the data-sets concerned
#'
#' @return
#' @export
#'
#' @examples
# data <- tibble::as_tibble(list(
#     "PPI_1" = c(5, 1, 3, 4,1,3),
#     "PPI_2" = c(3, 12, 7, 18,4,5),
#     "PPI_3" = c(17, 1, 2, 3,5,3),
#     "PPI_4" = c(NA, 2, 1, 6,8,7),
#     "PPI_5" = c(1, NA, 3, 1,1,2),
#     "PPI_6" = c(3, 7, 4, 1,4,5),
#     "PPI_7" = c(8, 3, NA, 2,7,6),
#     "PPI_8" = c(3, 8, 1, 3,0,2),
#     "PPI_9" = c(9, 2, 8, 5,NA,1),
#     "PPI_10" = c(12, 4, 7, 1,1,1),
#     "random_other_column" = c(NA, NA, NA, NA,NA,NA)
# ))
#
# country_code_column <- c("VN", "VN", "KE", "KE",NA,"AZ")
#' ppi_score(data, country_code_column)
#'
ppi_score <- function(data, country_code_column) {
    colnames(data) <- tolower(colnames(data))
    ppi_columns <- paste0("ppi_", 1:10)

    if (all(ppi_columns %in% colnames(data))) {
        ppi_data <- data[ppi_columns]
        ppi_data <- ppi_data %>% dplyr::mutate_all(as.numeric)
        ppi_score <- rowSums(ppi_data, na.rm = T)

        na_rows <- rowSums(is.na(ppi_data))==ncol(ppi_data)

        country_code_column <- toupper(country_code_column)
        ppi_limit_column <- unname(sapply(country_code_column, function(x) identify_ppi_limit(x)))
        ppi_likelihood <- unname(unlist(sapply(c(1:length(ppi_score)), function(x) identify_ppi_conversion(ppi_score[x], country_code_column[x]))))

        ppi_info <- tibble::as_tibble(list(
            ppi_likelihood = ppi_likelihood,
            ppi_limit = ppi_limit_column
        ))


        ppi_info$ppi_likelihood[na_rows] <- NA

        return(ppi_info)
    }

    return(NULL)

}

identify_ppi_conversion <- function(score, country_code) {
    column <- paste0("PPI_Likelihood_", country_code)

    if (score %in% ppi_score_card$Score & column %in% colnames(ppi_score_card)){
        score_to_return <- ppi_score_card[ppi_score_card$Score == score, column]
        return(as.numeric(score_to_return))

    }else{
        return(NA)
    }
}

identify_ppi_limit <- function(country_code) {
    column <- paste0("PPI_Likelihood_", country_code)
    if (country_code %in%ppi_limits$country ){
        ppi_limit <- ppi_limits$limit[ppi_limits$country == country_code]
    }else{
        return(NA)
    }
    return(ppi_limit)
}
