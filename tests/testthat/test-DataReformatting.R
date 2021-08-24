library(tibble)
library(testthat)

testthat::test_that("Splitting strings into dummy variables works", {
    test_data <- c("maize banana cassava","melon maize cassava", "banana")
    seperator <- " "

    expected_result <- structure(list(maize = c(TRUE, TRUE, FALSE),
                                      banana = c(TRUE, FALSE, TRUE),
                                      cassava = c(TRUE, TRUE, FALSE),
                                      melon = c(FALSE, TRUE, FALSE)),
                                 row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame"))
    actual_result <- split_string_categories_to_dummy(test_data,seperator)

    expect_equal(actual_result, expected_result)
})

testthat::test_that("Can switch a pair of columns with other columns",{

    data <- tibble::as_tibble(list("units"=c("kg","other","tonnes"),
    "units_other"=c(NA,"random_new_unit",NA)))

    actual_result <- merge_original_and_other_unit_single_pair(data$units,data$units_other)
    expected_result <- c("kg", "random_new_unit", "tonnes")

    expect_equal(actual_result, expected_result)


})

testthat::test_that("Can swap main names with other names",{

    data <-  tibble::as_tibble(list("units"=c("kg","other","tonnes"),
                                    "units_other"=c(NA,"random_new_unit",NA),

                                    "units_1"=c("kg","other1","other2"),
                                    "units_other_1"=c(NA,"sacks_100kg","qwerty"),
                                    "units_2"=c("kg","sacks_100kg","other"),
                                    "units_other_2"=c(NA,NA,"xys"),
                                    "units_3"=c("kg",NA,"dfgh"),
                                    "units_other_3"=c(NA,NA,"tonnes")))

    # Checking can do it for a non-loop structure
    main_column <- "units"
    other_column <- "units_other"
    loop_structure <- F

    actual_result <- merge_original_and_other_units(data, main_column,other_column,F)
    expected_result <- tibble::as_tibble(list("units"=c("kg","random_new_unit","tonnes")))

    expect_equal(actual_result, expected_result)


    loop_structure <- T

    actual_result <- merge_original_and_other_units(data, main_column,other_column,T)
    expected_result <- tibble::as_tibble(list("units_1"=c("kg","sacks_100kg","qwerty"),
                                              "units_2"=c("kg","sacks_100kg","xys"),
                                              "units_3"=c("kg",NA,"tonnes")))

    expect_equal(actual_result, expected_result)
})

testthat::test_that("Can swap individual pairs of names and 'other'",{

    main_column <- c("kg","other","tonnes")
    other_column <- c(NA,"random_new_unit",NA)

    expected_result <- c("kg","random_new_unit","tonnes")
    actual_result <- merge_original_and_other_unit_single_pair(main_column, other_column)

    expect_equal(actual_result, expected_result)


})

testthat::test_that("Can add new loop columns in the correct place",{
    # Checking can reorder individual columns
    data <- tibble::as_tibble(list("column_x"=c("kg","sacks_100kg","qwerty"),
                                   "column_y"=c("kg","sacks_100kg","xys"),
                                   "column_z"=c("kg",NA,"tonnes")))
    new_data <-tibble::as_tibble(list("column_new"=c("a","b","c")))
    old_column_name <- "column_y"

    actual_result <- add_column_after_specific_column(data,new_data, new_column_name=NULL,old_column_name, loop_structure=F)
    expected_result <- data <- tibble::as_tibble(list("column_x"=c("kg","sacks_100kg","qwerty"),
                                                      "column_y"=c("kg","sacks_100kg","xys"),
                                                      "column_new"=c("a","b","c"),
                                                      "column_z"=c("kg",NA,"tonnes")))

    expect_equal(actual_result, expected_result)

    # Checking can reorder multiple columns in the loop structure
    data <- tibble::as_tibble(list("old_column_1"=c("kg","sacks_100kg","qwerty"),
                                   "old_column_2"=c("kg","sacks_100kg","xys"),
                                   "old_column_3"=c("kg",NA,"tonnes")))
    new_data <- tibble::as_tibble(list("new_column_1"=c("kg","sacks_100kg","qwerty"),
                                       "new_column_2"=c("kg","sacks_100kg","xys"),
                                       "new_column_3"=c("kg",NA,"tonnes")))
    old_column_name <- "old_column"
    new_column_name <- "new_column"

    actual_result <- add_column_after_specific_column(data = data,
                                                      new_data = new_data,
                                                      new_column_name =new_column_name,
                                                      old_column_name = old_column_name,
                                                      loop_structure=T)

    expected_result <-tibble::as_tibble(list("old_column_1"=c("kg","sacks_100kg","qwerty"),
                                             "new_column_1"=c("kg","sacks_100kg","qwerty"),
                                             "old_column_2"=c("kg","sacks_100kg","xys"),
                                             "new_column_2"=c("kg","sacks_100kg","xys"),
                                             "old_column_3"=c("kg",NA,"tonnes"),
                                             "new_column_3"=c("kg",NA,"tonnes")))


    expect_equal(actual_result, expected_result)

})


testthat::test_that("Can conduct proportions swap to numeric",{
    data <- tibble::as_tibble(list(
        "crop_use_1"=c("eat","eat sell","sell feed_livestock"),
        "crop_consumed_prop_1"=c(NA,"most",NA),
        "crop_sold_prop_1"=c(NA,"little","little"),
        "crop_feed_lstk_prop_1"=c(NA,NA,"little")
    ))


    # Checking can do it for a looped column
    actual_result <- proportions_calculation(data,use="eat",
                                             use_column = "crop_use",
                                             prop_column = "crop_consumed_prop",
                                             loop_number = 1)
    expected_result <- c(1, 0.7, NA)

    expect_equal(actual_result,expected_result)

    # Checking can do it for individually specified columns

    actual_result <- proportions_calculation(data,use="eat",
                                             use_column = "crop_use_1",
                                             prop_column = "crop_consumed_prop_1")
    expected_result <- c(1, 0.7, NA)


    expect_equal(actual_result, actual_result)
})
