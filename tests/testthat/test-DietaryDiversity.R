library(testthat)
library(tibble)

testthat::test_that("HDDS Calculations work", {


    actual_result <-hdds_calc(hdds_10_test)
    expected_result <- structure(list(HDDS_good_season = c(8, 10, 10, 9, 8),
                                      HDDS_good_season_bought = c(2, 5, 4, 5, 3),
                                      HDDS_good_season_farm = c(6, 7, 6, 4, 5),
                                      HDDS_bad_season = c(4, 8, 6, 6, 5),
                                      HDDS_bad_season_bought = c(4, 5, 6, 6, 5),
                                      HDDS_bad_season_farm = c(0, 3, 0, 0, 0),
                                      HDDS_last_month = c(0, 0, 0, 0, 0)),
                                 row.names = c(NA, -5L), class = c("tbl_df", "tbl", "data.frame"))


  expect_equal(actual_result, expected_result)

  actual_result <-hdds_calc(hdds_14_test)
  expected_result <- structure(list(HDDS_bad_season = c(0, 4, 0, 5, 5),
                                    HDDS_bad_season_bought = c(0, 4, 0, 3, 4),
                                    HDDS_bad_season_farm = c(0, 0, 0, 4, 4),
                                    HDDS_good_season = c(0, 5, 0, 4, 8),
                                    HDDS_good_season_bought = c(0, 5, 0, 2, 7),
                                    HDDS_good_season_farm = c(0, 0, 0, 3, 5),
                                    HDDS_last_month = c(9, 6, 5, 4, 7),
                                    HDDS_last_month_bought = c(6, 0, 3, 0, 0),
                                    HDDS_last_month_farm = c(5, 0, 5, 0, 0)),
                               row.names = c(NA, -5L), class = c("tbl_df", "tbl", "data.frame"))

  expect_equal(actual_result, expected_result)
})
