library(testthat)


test_that("Extract unique values from loop column", {
    column_pattern <- "crop_name"
    number_of_loops <- 2
    data <- tibble(crop_name_1=c("maize","cassava","millet"),
                   random_column=c("blue","red","green"),
                   crop_name_2=c("potato",NA,"cucumber"))
    expected_result <- c("maize","cassava","millet","potato","cucumber")
    result <- extract_new_values(data, loop_or_individual_column="loop",column_pattern="crop_name", number_of_loops=2)

  expect_equal(expected_result, result)
})

test_that("Extract unique values from individual column", {
    column_name <- "random_column"
    data <- tibble(crop_name_1=c("maize",NA,"cassava",NA,"millet"),
                   random_column=c("blue","purple","red",NA,"green"),
                   crop_name_2=c(NA,"potato",NA,"cucumber",NA))
    expected_result <- c("blue","purple","red","green")
    result <- extract_new_values(data, loop_or_individual_column="column", column_name="random_column")

    expect_equal(expected_result, result)
})
