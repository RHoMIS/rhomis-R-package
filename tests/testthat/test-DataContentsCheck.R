library(tibble)
library(testthat)

testthat::test_that("Can check whether an individual column exists", {

    data <- tibble::as_tibble(list("one_column"=c(1,2,3),
                                   "second_column"=c(3,2,5),
                                   "column_of_interest"=c(4,20,4),
                                   "other_column"=c(NA,NA,NA),
                                   "second_column_of_interest"=c("x","y","z")
    ))

    expected_result <- c("column which doesn't exist")
    actual_result <- check_columns_individual(data,c("column which doesn't exist"))
    testthat::expect_equal(actual_result, expected_result)

    expected_result <- character()
    actual_result <- check_columns_individual(data,c("column_of_interest"))
    testthat::expect_equal(actual_result, expected_result)

    expected_result <- c("column which doesn't exist",
                         "another column which doesn't exist")
    actual_result <- check_columns_individual(data,c("column which doesn't exist",
                                                     "second_column",
                                                     "another column which doesn't exist"))
    testthat::expect_equal(actual_result, expected_result)
})


testthat::test_that("Can check whether an individual column exists", {

    data <- tibble::as_tibble(list("one_column"=c(1,2,3),
                                   "second_column"=c(3,2,5),
                                   "column_of_interest"=c(4,20,4),
                                   "other_column"=c(NA,NA,NA),
                                   "second_column_of_interest"=c("x","y","z"),

                                   "loop_1"=c("x","y","z"),
                                   "loop_2"=c("x","y","z"),
                                   "loop_3"=c("x","y","z"),

                                   "other_loop_1"=c("x","y","z"),
                                   "other_loop_2"=c("x","y","z"),
                                   "other_loop_3"=c("x","y","z"),
                                   "other_loop_4"=c("x","y","z")
    ))


    actual_result <- check_columns_loop(data, c("loop","other_loop", "non_existent_loop"))
    expected_result <- c("non_existent_loop")
    testthat::expect_equal(actual_result, expected_result)

    actual_result <- check_columns_loop(data, c("loop","other_loop"))
    expected_result <- character()
    testthat::expect_equal(actual_result, expected_result)

    actual_result <- check_columns_loop(data, c("other_column","other_loop"))
    expected_result <- character()
    testthat::expect_equal(actual_result, expected_result)
})


testthat::test_that("Can check for missing columns in data",{

    data <- tibble::as_tibble(list("one_column"=c(1,2,3),
                                   "second_column"=c(3,2,5),
                                   "column_of_interest"=c(4,20,4),
                                   "other_column"=c(NA,NA,NA),
                                   "second_column_of_interest"=c("x","y","z"),

                                   "loop_1"=c("x","y","z"),
                                   "loop_2"=c("x","y","z"),
                                   "loop_3"=c("x","y","z"),

                                   "other_loop_1"=c("x","y","z"),
                                   "other_loop_2"=c("x","y","z"),
                                   "other_loop_3"=c("x","y","z"),
                                   "other_loop_4"=c("x","y","z")
    ))


    testthat::expect_warning(actual_result <- check_columns_in_data(data,
                                                                    loop_columns=c("loop","other_loop", "non_existent_loop"),
                                                                    individual_columns=c("column_of_interest", "random_other_column")))
    expected_result <- c("random_other_column","non_existent_loop")
    testthat::expect_equal(actual_result, expected_result)


    testthat::expect_warning(actual_result <- check_columns_in_data(data,
                                                                    loop_columns=c("loop","other_loop", "non_existent_loop")))
    expected_result <- c("non_existent_loop")
    testthat::expect_equal(actual_result, expected_result)


    testthat::expect_warning(actual_result <- check_columns_in_data(data,
                                                                    individual_columns=c("column_of_interest", "random_other_column")))
    expected_result <- c("random_other_column")
    testthat::expect_equal(actual_result, expected_result)


    actual_result <- check_columns_in_data(data,
                                           individual_columns=c("column_of_interest"))
    expected_result <- c(character())
    testthat::expect_equal(actual_result, expected_result)
})
