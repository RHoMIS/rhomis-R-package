library(testthat)
library(tibble)
library(tibble)



testthat::test_that("Can create a a by-project unit conversion table for standard units",{
    proj_id_vector <- c("id_1", "id_1", "id_3", "id_2")

    result <- make_per_project_conversion_tibble(proj_id_vector = proj_id_vector,unit_conv_tibble = eggs_amount_units)

    # Generated with dput
    expected_result <- structure(list(survey_value = c("pieces/day", "pieces/week",
                                                       "pieces/month", "pieces/animal/day", "total", "pieces/day", "pieces/week",
                                                       "pieces/month", "pieces/animal/day", "total", "pieces/day", "pieces/week",
                                                       "pieces/month", "pieces/animal/day", "total"),
                                      conversion = c("365",
                                                     "52.1428571428571", "13.0357142857143", "pieces/animal/day",
                                                     "1", "365", "52.1428571428571", "13.0357142857143", "pieces/animal/day",
                                                     "1", "365", "52.1428571428571", "13.0357142857143", "pieces/animal/day",
                                                     "1"),
                                      id_rhomis_dataset = c("id_1", "id_1", "id_1", "id_1", "id_1",
                                                            "id_3", "id_3", "id_3", "id_3", "id_3", "id_2", "id_2", "id_2",
                                                            "id_2", "id_2")),
                                 row.names = c(NA, -15L),
                                 class = c("tbl_df",
                                           "tbl", "data.frame"))

    expect_equal(result, expected_result)


})

testthat::test_that("Can switch units based on project identification",{
    unit_conversion_tibble <- tibble::as_tibble(list(
        id_rhomis_dataset=c("project_1", "project_2", "project_3", "project_3", "project_4"),
        survey_value=c("kg", "sacks", "wheel_barrows_100kg","litres", "sacks"),
        conversion=c(1,50,100,NA, 25)))


    list_to_convert <- c("kg", "sacks","other_unit",NA, "sacks", "wheel_barrows_100kg","litres")
    id_rhomis_dataset <- c("project_1", "project_2","project_5","project_4", "project_4", "project_3","project_3")

    result <- switch_units(data_to_convert = list_to_convert,unit_tibble = unit_conversion_tibble,id_vector = id_rhomis_dataset)
    expected_result <- c(1, 50, NA, NA, 25, 100, NA)

    expect_equal(result, expected_result)



    unit_conversion_tibble <- tibble::as_tibble(list(
        id_rhomis_dataset=c("project_1", "project_1", "project_2", "project_3", "project_4"),
        survey_value=c("kg", "sacks", "wheel_barrows_100kg","litres", "sacks"),
        conversion=c(1,50,100,NA, 25)))


    tibble_to_convert <- tibble::as_tibble(list("maize"=c("kg", "other_random_unit","sacks"),
                                                "cassava"=c("sacks",NA,"litres"),
                                                "banana"=c("bunches", "wheel_barrows_100kg",NA)))
    id_rhomis_dataset <- c("project_1", "project_2","project_4")

    result <- switch_units(data_to_convert = tibble_to_convert,unit_tibble = unit_conversion_tibble,id_vector = id_rhomis_dataset)
    expected_result <- tibble::as_tibble(list(
        maize=c(1,NA,25),
        cassava=c(50,NA,NA),
        banana=c(NA,100,NA)
    ))

    expect_equal(result, expected_result)



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
        "crop_yield_units_other_2"=c("abc",NA),
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
        "crop_yield_units_2"=c("abc",NA),
        "crop_yield_units_other_2"=c("abc",NA),
        "crop_yield_units_3"=c("kg",NA),
        "crop_yield_units_other_3"=c("unit",NA),

        "milk_units_1"=c("litres","other_milk_unit"),
        "milk_amount_units_other_1"=c(NA,"other_milk_unit"),
        "milk_units_2"=c(NA,"milk_unit_2"),
        "milk_amount_units_other_2"=c(NA,"milk_unit_2"),

        "random_other_column"=c("bla",'bla')

    ))

    # expected_result <- dplyr::mutate_all(expected_result, as.character)


    actual_result <- replace_units_with_other_all(data)

    testthat::expect_equal(actual_result, expected_result)




})
