test_that("Test that can identify number of loops", {

    name_column <- "crop_name"
    data <- as_tibble(list(crop_name_1=c("banana", "cassava", NA, "millet"),
                           crop_name_2=c("cassava",NA, "melon", "maize"),
                           random_crop_name_2=c("blue", "green",  "red",NA),
                           crop_name=c("orange", "purple", NA, "black")))
    actual_result <- find_number_of_loops(data,name_column)
    expected_result <- 2

  expect_equal(actual_result, expected_result)
})

test_that("Test can find unique values for the specific name column",{
    name_column <- "crop_name"
    data <- as_tibble(list(crop_name_1=c("banana", "cassava", NA, "millet"),
                           crop_name_2=c("cassava",NA, "melon", "maize"),
                           random_crop_name_2=c("blue", "green",  "red",NA),
                           crop_name=c("orange", "purple", NA, "black")))
    expected_result <- c("banana","cassava","millet","melon","maize")
    actual_result <- find_unique_case(data, name_column)
    expect_equal(actual_result,expected_result)
})

test_that("Individual loop variables can be properly formatted",{
    name_column <- "crop_name"
    variable_to_convert <- "crop_variable"
    data <- as_tibble(list(crop_name_1=c("banana", "cassava", NA, "millet"),
                           crop_name_2=c("cassava",NA, "melon", "maize"),
                           random_crop_name_2=c("blue", "green",  "red",NA),
                           crop_name=c("orange", "purple", NA, "black"),
                           crop_variable_1=c("ex1","ex2",NA,NA),
                           crop_variable_2=c("ex3",NA,"ex4","ex5")))
    number_of_loops <- find_number_of_loops(data,name_column)

    expected_result <- as_tibble(list(banana=c("ex1", NA, NA, NA),
                                      cassava=c("ex3", "ex2", NA, NA),
                                      millet=c(NA, NA, NA, NA),
                                      melon=c(NA, NA, "ex4", NA),
                                      maize=c(NA, NA, NA, "ex5")))
    expected_result<- expected_result %>% mutate_all(as.character)


    actual_result <- loop_to_column_conversion(data, name_column, variable_to_convert, type="chr")

    expect_equal(actual_result,expected_result)

})
