test_that("Splitting strings into dummy variables works", {
    test_data <- c("maize banana cassava","melon maize cassava", "banana")
    seperator <- " "

    expected_result <- structure(list(maize = c(TRUE, TRUE, FALSE),
                                      banana = c(TRUE, FALSE, TRUE),
                                      cassava = c(TRUE, TRUE, FALSE),
                                      melon = c(FALSE, TRUE, FALSE)),
                                 row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame"))
    actual_result <- split_string_categories_to_dummy(test_data,seperator)

    expect_equal(actual_result, actual_result)
})
