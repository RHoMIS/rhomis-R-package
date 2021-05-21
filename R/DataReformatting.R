library(tibble)

#' Split string to dummy columns
#'
#' Many RHoMIS columns come in the format c("cropA cropB cropC", "cropD cropA").
#' This function helps split these into more interperatable dummy columns
#'
#' @param x A vector which needs to be split
#' @param seperator The separating character
#'
#' @return
#' @export
#'
#' @examples
split_string_categories_to_dummy <- function(x, seperator)
{
    x[is.na(x)] <- "NA"
    x <- tolower(x)
    split <- strsplit(x, seperator, fixed = T)
    all_potential_value <- unique(unlist(strsplit(x, seperator,
                                                  fixed = T)))
    boolean_nested_list <- lapply(split, function(x) create_nested_lest(longer_list = all_potential_value,
                                                                        shorter_list = x))
    df_to_return <- tibble::as_tibble(do.call(rbind, boolean_nested_list),
                              check.names = F)
    return(df_to_return)
}

create_nested_lest <- function (longer_list, shorter_list)
{
    temp_list <- longer_list %in% shorter_list
    names(temp_list) <- longer_list
    return(temp_list)
}

