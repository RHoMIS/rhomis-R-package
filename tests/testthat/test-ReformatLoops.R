library(testthat)
library(dplyr)
testthat::test_that("Test that can identify number of loops", {

    name_column <- "crop_name"
    data <- tibble::as_tibble(list(crop_name_1=c("banana", "cassava", NA, "millet"),
                                   crop_name_2=c("cassava",NA, "melon", "maize"),
                                   random_crop_name_2=c("blue", "green",  "red",NA),
                                   crop_name=c("orange", "purple", NA, "black")))

    actual_result <- find_number_of_loops(data,name_column)
    expected_result <- 2

    expect_equal(actual_result, expected_result)

})

testthat::test_that("Test can find unique values for the specific name column",{
    name_column <- "crop_name"
    data <- tibble::as_tibble(list(crop_name_1=c("banana", "cassava", NA, "millet"),
                                   crop_name_2=c("cassava",NA, "melon", "maize"),
                                   random_crop_name_2=c("blue", "green",  "red",NA),
                                   crop_name=c("orange", "purple", NA, "black")))

    expected_result <- c("banana","cassava","millet","melon","maize")
    actual_result <- find_unique_case(data, name_column)
    expect_equal(actual_result,expected_result)

    # Checking for case with no expected values
    data <- tibble::as_tibble(list(crop_name_1=c(NA, NA, NA, NA),
                                   crop_name_2=c(NA,NA, NA, NA),
                                   random_crop_name_2=c("blue", "green",  "red",NA),
                                   crop_name=c("orange", "purple", NA, "black")))

    name_column <- "crop_name"
    expected_result <- c("none")
    actual_result <- find_unique_case(data, name_column)
    expect_equal(actual_result,expected_result)
})

testthat::test_that("Individual loop variables can be properly formatted",{
    name_column <- "crop_name"
    variable_to_convert <- "crop_variable"
    data <- tibble::as_tibble(list(crop_name_1=c("banana", "cassava", NA, "millet"),
                                   crop_name_2=c("cassava",NA, "melon", "maize"),
                                   random_crop_name_2=c("blue", "green",  "red",NA),
                                   crop_name=c("orange", "purple", NA, "black"),
                                   crop_variable_1=c("ex1","ex2",NA,NA),
                                   crop_variable_2=c("ex3",NA,"ex4","ex5")))

    expected_result <- tibble::as_tibble(list(banana=c("ex1", NA, NA, NA),
                                              cassava=c("ex3", "ex2", NA, NA),
                                              millet=c(NA, NA, NA, NA),
                                              melon=c(NA, NA, "ex4", NA),
                                              maize=c(NA, NA, NA, "ex5")))


    expected_result <- tibble::as_tibble(list(banana=c("ex1", NA, NA, NA),
                                      cassava=c("ex3", "ex2", NA, NA),
                                      millet=c(NA, NA, NA, NA),
                                      melon=c(NA, NA, "ex4", NA),
                                      maize=c(NA, NA, NA, "ex5")))
    expected_result<- expected_result %>% dplyr::mutate_all(as.character)


    actual_result <- loop_to_column_conversion(data, name_column, variable_to_convert, type="chr")

    expect_equal(actual_result,expected_result)


    # Checking case for no values
    name_column <- "crop_name"
    variable_to_convert <- "crop_variable"
    data <- tibble::as_tibble(list(crop_name_1=c(NA, NA, NA, NA),
                                   crop_name_2=c(NA,NA, NA, NA),
                                   random_crop_name_2=c("blue", "green",  "red",NA),
                                   crop_name=c("orange", "purple", NA, "black"),
                                   crop_variable_1=c(NA,NA,NA,NA),
                                   crop_variable_2=c(NA,NA,NA,NA)))

    expected_result <- tibble::as_tibble(list(none=c(NA, NA, NA, NA)))
    expected_result<- expected_result %>% dplyr::mutate_all(as.character)
    actual_result <- loop_to_column_conversion(data, name_column, variable_to_convert, type="chr")

    expect_equal(actual_result,expected_result)

})


testthat::test_that("Test that we can convert multiple columns into the wide format",{

    name_column <- "crop_name"
    column_prefixes <- c("crop_variable", "crop_unit", "crop_price")
    types <- c("chr", "chr", "chr")

    data <- tibble::as_tibble(list(crop_name_1=c("banana", "cassava", NA, "millet"),
                                   crop_name_2=c("cassava",NA, "melon", "maize"),
                                   random_crop_name_2=c("blue", "green",  "red",NA),
                                   crop_name=c("orange", "purple", NA, "black"),
                                   crop_variable_1=c("ex1","ex2",NA,NA),
                                   crop_variable_2=c("ex3",NA,"ex4","ex5"),
                                   crop_unit_1=c("unit1","unit2","unit3",NA),
                                   crop_unit_2=c(NA,NA,"unit4","unit5"),
                                   crop_price_1=c(NA,NA,NA,NA),
                                   crop_price_2=c(NA,NA,NA,NA)))
    actual_result <- map_to_wide_format(data, name_column, column_prefixes, types)
    # Expected result
    crop_variable <- tibble::as_tibble(list(banana=c("ex1",NA,NA,NA),
                                            cassava=c("ex3","ex2",NA,NA),
                                            millet=c(NA,NA,NA,NA),
                                            melon=c(NA,NA,"ex4",NA),
                                            maize=c(NA,NA,NA,"ex5")))
    crop_variable<- crop_variable %>% dplyr::mutate_all(as.character)
    crop_unit <- tibble::as_tibble(list(banana=c("unit1",NA,NA,NA),
                                        cassava=c(NA,"unit2",NA,NA),
                                        millet=c(NA,NA,NA,NA),
                                        melon=c(NA,NA,"unit4",NA),
                                        maize=c(NA,NA,NA,"unit5")))
    crop_unit<- crop_unit %>% dplyr::mutate_all(as.character)
    crop_price <- tibble::as_tibble(list(banana=c(NA,NA,NA,NA),
                                         cassava=c(NA,NA,NA,NA),
                                         millet=c(NA,NA,NA,NA),
                                         melon=c(NA,NA,NA,NA),
                                         maize=c(NA,NA,NA,NA)))

    actual_result <- map_to_wide_format(data, name_column, column_prefixes, types)
    # Expected result
    crop_variable <- tibble::as_tibble(list(banana=c("ex1",NA,NA,NA),
                                    cassava=c("ex3","ex2",NA,NA),
                                    millet=c(NA,NA,NA,NA),
                                    melon=c(NA,NA,"ex4",NA),
                                    maize=c(NA,NA,NA,"ex5")))
    crop_variable<- crop_variable %>% dplyr::mutate_all(as.character)
    crop_unit <- tibble::as_tibble(list(banana=c("unit1",NA,NA,NA),
                                cassava=c(NA,"unit2",NA,NA),
                                millet=c(NA,NA,NA,NA),
                                melon=c(NA,NA,"unit4",NA),
                                maize=c(NA,NA,NA,"unit5")))
    crop_unit<- crop_unit %>% dplyr::mutate_all(as.character)
    crop_price <- tibble::as_tibble(list(banana=c(NA,NA,NA,NA),
                                 cassava=c(NA,NA,NA,NA),
                                 millet=c(NA,NA,NA,NA),
                                 melon=c(NA,NA,NA,NA),
                                 maize=c(NA,NA,NA,NA)))
    crop_price<- crop_price %>% dplyr::mutate_all(as.character)
    expected_result <- list(crop_variable=crop_variable,
                            crop_unit=crop_unit,
                            crop_price=crop_price)

    expect_equal(actual_result,expected_result)
})


testthat::test_that("Gender splitting of information works",{
    data <- tibble::as_tibble(list(crop_name_1=c("banana", "cassava", NA, "millet"),
                                   crop_name_2=c("cassava",NA, "melon", "maize"),
                                   random_crop_name_2=c("blue", "green",  "red",NA),
                                   crop_name=c("orange", "purple", NA, "black"),
                                   crop_control_1=c("male_adult female_adult","male_adult",NA,"female_youth"),
                                   crop_control_2=c("male_youth",NA,"female_youth","female_adult")))


    wide_data <- map_to_wide_format(data, "crop_name", c("crop_control"), c("chr"))
    gender_data <- wide_data$crop_control

    female_youth <- tibble::as_tibble(list(banana=c(0,NA,NA,NA),

                                           cassava=c(0,0,NA,NA),
                                           millet=c(NA,NA,NA,1),
                                           melon=c(NA,NA,1,NA),
                                           maize=c(NA,NA,NA,0)))
    female_adult <- tibble::as_tibble(list(banana=c(0.5,NA,NA,NA),
                                           cassava=c(0,0,NA,NA),
                                           millet=c(NA,NA,NA,0),
                                           melon=c(NA,NA,0,NA),
                                           maize=c(NA,NA,NA,1)))
    male_youth <- tibble::as_tibble(list(banana=c(0,NA,NA,NA),
                                         cassava=c(1,0,NA,NA),
                                         millet=c(NA,NA,NA,0),
                                         melon=c(NA,NA,0,NA),
                                         maize=c(NA,NA,NA,0)))
    male_adult <- tibble::as_tibble(list(banana=c(0.5,NA,NA,NA),
                                         cassava=c(0,1,NA,NA),
                                         millet=c(NA,NA,NA,0),
                                         melon=c(NA,NA,0,NA),
                                         maize=c(NA,NA,NA,0)))

    female_adult <- tibble::as_tibble(list(banana=c(0.5,NA,NA,NA),
                                   cassava=c(0,0,NA,NA),
                                   millet=c(NA,NA,NA,0),
                                   melon=c(NA,NA,0,NA),
                                   maize=c(NA,NA,NA,1)))
    male_youth <- tibble::as_tibble(list(banana=c(0,NA,NA,NA),
                                 cassava=c(1,0,NA,NA),
                                 millet=c(NA,NA,NA,0),
                                 melon=c(NA,NA,0,NA),
                                 maize=c(NA,NA,NA,0)))
    male_adult <- tibble::as_tibble(list(banana=c(0.5,NA,NA,NA),
                                 cassava=c(0,1,NA,NA),
                                 millet=c(NA,NA,NA,0),
                                 melon=c(NA,NA,0,NA),
                                 maize=c(NA,NA,NA,0)))
    expected_result <- list(female_youth=female_youth,
                            female_adult=female_adult,
                            male_youth=male_youth,
                            male_adult=male_adult)

    actual_result <- split_gender_data(gender_data)

    expect_equal(actual_result,expected_result)
})


testthat::test_that("Test that can work out gender proportion scores for a single column",{
    column <- c("male_adult female_adult","male_adult",NA,"female_youth")
    actual_result <- proportion_control_per_person(column)

    expected_result <- c(0.5,1,NA,1)
    expect_equal(actual_result,expected_result)

})

testthat::test_that("Test that can correctly identify the categories in a column", {
    item <- c("male_adult female_adult","male_adult",NA,"female_youth")
    category <- "female_youth"
    actual_result <-check_val_in_list(item, category)
    expected_result <- c(0,0,0,1)
    expect_equal(actual_result,expected_result)
    item <- c("male_adult female_adult","male_adult",NA,"female_youth")
    category <- "male_adult"
    actual_result <-check_val_in_list(item, category)
    expected_result <- c(1,1,0,0)
    expect_equal(actual_result,expected_result)
})

testthat::test_that("Test that can identify gender control proportions for individual categories",{

    data <- tibble::as_tibble(list(crop_name_1=c("banana", "cassava", NA, "millet"),
                                   crop_name_2=c("cassava",NA, "melon", "maize"),
                                   random_crop_name_2=c("blue", "green",  "red",NA),
                                   crop_name=c("orange", "purple", NA, "black"),
                                   crop_control_1=c("male_adult female_adult","male_adult",NA,"female_youth"),
                                   crop_control_2=c("male_youth",NA,"female_youth","female_adult")))

    # Converting the dataset to the wide format (i.e crops as header, and genders controlling for each crop)
    wide_data <- map_to_wide_format(data, "crop_name", c("crop_control"), c("chr"))
    genderdf <- wide_data$crop_control

    numberControllingDF <- genderdf %>% dplyr::mutate(across(.cols=everything(),~proportion_control_per_person(.x)))
    category <- "male_adult"
    actual_result <- gender_control_props(genderdf, numberControllingDF, category)

    expected_result <- tibble::as_tibble(list(banana=c(0.5,NA,NA,NA),
                                              cassava=c(0,1,NA,NA),
                                              millet=c(NA,NA,NA,0),
                                              melon=c(NA,NA,0,NA),
                                              maize=c(NA,NA,NA,0)))


    expect_equal(actual_result,expected_result)


})



testthat::test_that("Gender split for individual column works",{

    data <- c("male_adult","male_adult female_adult",NA,"female_youth male_adult female_adult")
    actual_result <- split_gender_columns(data)
    expected_result <- structure(list(female_youth = c(0, 0, NA, 0.333333333333333),
                                      female_adult = c(0, 0.5, NA, 0.333333333333333),
                                      male_youth = c(0, 0, NA, 0),
                                      male_adult = c(1, 0.5, NA, 0.333333333333333)),
                                 row.names = c(NA, -4L),
                                 class = c("tbl_df", "tbl", "data.frame"))
    expect_equal(actual_result,expected_result)


    })

