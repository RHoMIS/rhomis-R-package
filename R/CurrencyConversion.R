library(wbstats)
library(tibble)
library(dplyr)




#' Query World Bank Statistics
#'
#' Find out PPP conversions based on the
#' world bank statistics
#'
#' @param year The year you are hoping to query
#' @param country_code The two letter ISO country code
#'
#' @return
#' @export
#'
#' @examples
querywb_stats <- function(year,country_code){

    tryCatch(
        {
            wbstats::wb_data(indicator="PA.NUS.PRVT.PP",
                             country=country_code,
                             start_date = year)

        },
        error=function(cond) {
            message("Could not obtain currency conversion factor")
            message("error message:")
            message(cond)
            # Choose a return value in case of error
            return(tibble::as_tibble(list(
                "PA.NUS.PRVT.PP"=c(NA),
                "date"=c(NA)
            )))
        },
        finally = {

        }

    )


}


#' Conversion_factor
#'
#' Identify the closest available currency conversion factor
#' based on world bank data. If we cannot find the current
#' currency conversion factor, we will check for the most recent,
#' going as far back as 10 years
#'
#' @param year The year you are querying the conversion factors for
#' @param country_code The country codes that you are querying for
#'
#' @return
#' @export
#'
#' @examples
currency_conversion_factor <- function(year,country_code){


    conversion_factor <- NA
    year <- as.numeric(year)
    country_code <- as.character(country_code)
    initial_year <- year

    if(is.na(year)|is.na(country_code)){

        message("Either country or year of survey are not well defined, so unable to obtain currency conversion")

        return(list("conversion_year"=NA,
                    "conversion_factor"=NA))
    }



    while(is.na(conversion_factor) & initial_year-year<10)
    {

        wb_result <- querywb_stats(year,country_code)
        conversion_factor <- as.numeric(wb_result$PA.NUS.PRVT.PP)
        conversion_year <- as.numeric(wb_result$date)

        if(nrow(wb_result)==0){
            conversion_factor<-NA
            conversion_year<-NA
        }


        year<-year-1

        if(is.na(conversion_factor)){
            conversion_year <- NA
        }
    }

    return(list("conversion_year"=conversion_year,
                "conversion_factor"=conversion_factor))

}


convert_all_currencies <- function(data, country_column="country", year_column="year"){

    data <- tibble::as_tibble((list(

        "country_code"=c("KM","KM","VN", "VN", "UG", NA, "XY"),
        "year"=c("2016","2016","2021", "2014", "2016", NA, "2020")
    )))

    combinations <- tibble::as_tibble(table(data)) %>%
        dplyr::filter(n>0) %>%
        dplyr::select(-c("n"))

    conversion_factors <- c()
    conversion_years <- c()

    for (i in 1:nrow(combinations)){

        conversion_data <- currency_conversion_factor(combinations[i,"year"], combinations[i,"country_code"])
        conversion_factors <- c(conversion_factors,conversion_data["conversion_factor"])
        conversion_years <- c(conversion_years,conversion_data["conversion_year"])

    }

    combinations$conversion_factor <-unlist(conversion_factors)
    combinations$conversion_year <-unlist(conversion_years)

    data_with_conversions <- dplyr::left_join(data,combinations, by=c("country_code"="country_code","year"="year"))

    data <- add_column_after_specific_column(
        data = data,
        new_data = data_with_conversions["conversion_factor"],
        new_column_name = "conversion_factor",
        old_column_name = "country_code",
        loop_structure = F

    )

    data <- add_column_after_specific_column(
        data = data,
        new_data = data_with_conversions["conversion_year"],
        new_column_name = "conversion_year",
        old_column_name = "country_code",
        loop_structure = F

    )

    return(data)
}


