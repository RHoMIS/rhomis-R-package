library(testthat)
library(tibble)
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


testthat::test_that('Can replace crop and livestock "other" with the name',{

    data <- tibble::as_tibble(list("crops_other1"=c("replacehh1oth1","replacehh2oth1","replacehh3oth1",NA),
                                   "crops_other2"=c(NA,NA,"replacehh3oth2",NA),
                                   "crops_other3"=c(NA,"replacehh2oth3","replacehh3oth3","replacehh4oth3"),

                                   "crop_name_1"=c("maize","banana","cassava","wheat"),
                                   "crop_name_2"=c("banana","other1","maize","banana"),
                                   "crop_name_3"=c("other1","other2",NA,NA),
                                   "crop_name_4"=c(NA,"other3","other1","other3"),

                                   "livestock_other1"=c("replacehh1oth1",NA,"replacehh3oth1",NA),


                                   "livestock_name_1"=c("other1","pigs","cattle","sheep"),
                                   "livestock_name_2"=c("sheep",NA,"other1","cattle"),



                                   "random_other_column"=c("bla","bla","bla","bla")))


    expected_result <- tibble::as_tibble(list("crops_other1"=c("replacehh1oth1","replacehh2oth1","replacehh3oth1",NA),
                                                      "crops_other2"=c(NA,NA,"replacehh3oth2",NA),
                                                      "crops_other3"=c(NA,"replacehh2oth3","replacehh3oth3","replacehh4oth3"),

                                                      "crop_name_1"=c("maize","banana","cassava","wheat"),
                                                      "crop_name_2"=c("banana","replacehh2oth1","maize","banana"),
                                                      "crop_name_3"=c("replacehh1oth1",NA,NA,NA),
                                                      "crop_name_4"=c(NA,"replacehh2oth3","replacehh3oth1","replacehh4oth3"),

                                              "livestock_other1"=c("replacehh1oth1",NA,"replacehh3oth1",NA),


                                              "livestock_name_1"=c("replacehh1oth1","pigs","cattle","sheep"),
                                              "livestock_name_2"=c("sheep",NA,"replacehh3oth1","cattle"),


                                                      "random_other_column"=c("bla","bla","bla","bla")))

    actual_result <- replace_crop_and_livestock_other(data)

    expect_equal(actual_result, expected_result)


})


testthat::test_that('Can swap main units with "other" units',{

    data <- tibble::as_tibble(list(
        "unitland"=c(NA,"other"),
        "areaunits_other"=c(NA,"other_area_unit"),

        "fertiliser_units"=c("kg",NA),
        "fertiliser_units_other"=c(NA,NA),

        "crop_yield_units_1"=c("unit","other"),
        "crop_yield_units_other_1"=c("unit","other_unit_subbed"),
        "crop_yield_units_2"=c("other",NA),
        "crop_yield_units_other_2"=c(NA,NA),
        "crop_yield_units_3"=c("kg","other"),
        "crop_yield_units_other_3"=c("unit",NA),

        "milk_units_1"=c("litres","other"),
        "milk_amount_units_other_1"=c(NA,"other_milk_unit"),
        "milk_units_2"=c("other","other"),
        "milk_amount_units_other_2"=c(NA,"milk_unit_2"),

        "random_other_column"=c("bla",'bla')))


        expected_result  <- tibble::as_tibble(list(
            "unitland"=c(NA,"other_area_unit"),
            "areaunits_other"=c(NA,"other_area_unit"),

            "fertiliser_units"=c("kg",NA),
            "fertiliser_units_other"=c(NA,NA),

            "crop_yield_units_1"=c("unit","other_unit_subbed"),
            "crop_yield_units_other_1"=c("unit","other_unit_subbed"),
            "crop_yield_units_2"=c(NA,NA),
            "crop_yield_units_other_2"=c(NA,NA),
            "crop_yield_units_3"=c("kg",NA),
            "crop_yield_units_other_3"=c("unit",NA),

            "milk_units_1"=c("litres","other_milk_unit"),
            "milk_amount_units_other_1"=c(NA,"other_milk_unit"),
            "milk_units_2"=c(NA,"milk_unit_2"),
            "milk_amount_units_other_2"=c(NA,"milk_unit_2"),

            "random_other_column"=c("bla",'bla')

    ))

        expected_result <- dplyr::mutate_all(expected_result, as.character)


        actual_result <- replace_units_with_other_all(data)

        testthat::expect_equal(actual_result, expected_result)

        actual_result==expected_result




})
