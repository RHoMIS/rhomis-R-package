library(testthat)
library(tibble)

testthat::test_that("PPI calculation works", {
    data <- tibble::as_tibble(list("PPI_1"=c(5,1,3,4),
                           "PPI_2"=c(3,12,7,18),
                           "PPI_3"=c(17,1,2,3),
                           "PPI_4"=c(NA,2,1,6),
                           "PPI_5"=c(1,NA,3,1),
                           "PPI_6"=c(3,7,4,1),
                           "PPI_7"=c(8,3,NA,2),
                           "PPI_8"=c(3,8,1,3),
                           "PPI_9"=c(9,2,8,5),
                           "PPI_10"=c(12,4,7,1),
                           "random_other_column"=c(NA,NA,NA,NA)))

    country_code_column <- c("VN","VN","KE","KE")

    expected_result <- structure(list(ppi_likelihood = c(14.1, 52, 38.8, 26.5),
                                      ppi_limit = c(1.75, 1.75, 1.9, 1.9)),
                                 row.names = c(NA, -4L), class = c("tbl_df", "tbl", "data.frame"))

    actual_result <- ppi_score(data,country_code_column)

  expect_equal(actual_result, expected_result)
})
