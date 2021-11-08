library(testthat)
library(tibble)

testthat::test_that("can convert from data fram to json", {
    sample_data_frame <- tibble::as_tibble(list("original_spelling"=c("benana","maz","wetermalon","cokonut"),
    "standardised_spelling"=c("benana",NA,"wetermalon","cokonut")))
    actual_result <- data_frame_to_json(sample_data_frame)
    expected_result <- '[{"original_spelling":"benana","standardised_spelling":"benana"},{"original_spelling":"maz","standardised_spelling":null},{"original_spelling":"wetermalon","standardised_spelling":"wetermalon"},{"original_spelling":"cokonut","standardised_spelling":"cokonut"}]'

    expect_equal(actual_result, expected_result)
    })
