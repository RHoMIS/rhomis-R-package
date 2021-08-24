library(testthat)
library(tibble)

testthat::test_that("Test that can swap individual units out",{
    item_to_convert <- "wheel_barrows_100kg"
    unit_conv_tibble <- tibble::as_tibble(list(unit=c("kg", "sacks_50kg", "wheel_barrows_100kg","litres"), conversion_factors= c(1,50,100,1)))

    actual_result <- replace_unit_with_conversion_factor(item_to_convert, unit_conv_tibble)
    expected_result <- 100

    expect_equal(actual_result, expected_result)

})

testthat::test_that("Test that can convert a list of units",{
    list_to_convert <- c("kg", "sacks_50kg","other_unit",NA, NA, "wheel_barrows_100kg","litres")
    unit_conv_tibble <- tibble::as_tibble(list(unit=c("kg", "sacks_50kg", "wheel_barrows_100kg","litres"), conversion_factors= c(1,50,100,1)))

    expected_result <- c(1,50,NA,NA,NA,100,1)

    actual_result <- replace_unit_list(list_to_convert,unit_conv_tibble)
    expect_equal(actual_result, expected_result)

})

testthat::test_that("Test that can convert either a list or whole dataframe of units",{

    units <- c("kg", "sacks_50kg", "wheel_barrows_100kg","litres")
    conversion_factors <- c(1,50,100,1)

    list_to_convert <- c("kg", "sacks_50kg","other_unit",NA, NA, "wheel_barrows_100kg","litres")

    actual_result <- switch_units(list_to_convert, units, conversion_factors)
    expected_result<-c(1,50,NA,NA,NA,100,1)
    expect_equal(actual_result, expected_result)


    tibble_to_convert <- tibble::as_tibble(list("maize"=c("kg", "other_random_unit","wheel_barrows_100kg"),
                                                "cassava"=c("sacks_50kg",NA,"another_random_unit"),
                                                "banana"=c("bunches", "wheel_barrows_100kg",NA)))
    actual_result <- switch_units(tibble_to_convert, units, conversion_factors)

    expected_result <- tibble::as_tibble(list("maize"=c(1, NA,100),
                                              "cassava"=c(50,NA,NA),
                                              "banana"=c(NA, 100,NA)))
    expect_equal(actual_result, expected_result)
})


testthat::test_that("Can convert columns to lower case",{

    data <- tibble::as_tibble(list("one column"=c(1,2,4),
                                   "second_column"=c("BFJKKE","Bnjwkefn","Ejn,serkjsfn"),
                                   "third column"=c("abcdE",NA,"fgFt")))

    expected_result <- tibble::as_tibble(list("one column"=c(1,2,4),
                                              "second_column"=c("bfjkke","bnjwkefn","ejn,serkjsfn"),
                                              "third column"=c("abcde",NA,"fgft")))

    actual_result <-convert_all_columns_to_lower_case(data)

    expect_equal(actual_result, expected_result)


})
