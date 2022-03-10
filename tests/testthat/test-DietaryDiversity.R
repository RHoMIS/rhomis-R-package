library(testthat)
library(tibble)

testthat::test_that("HDDS Calculations work", {



    actual_result <-hdds_calc(hdds_10_test)
    expected_result <- structure(list(hdds_good_season = c(8, 10, 10, 9, 8),
                                      hdds_good_season_bought = c(2, 5, 4, 5, 3),
                                      hdds_good_season_farm = c(6, 7, 6, 4, 5),
                                      hdds_bad_season = c(4, 8, 6, 6, 5),
                                      hdds_bad_season_bought = c(4, 5, 6, 6, 5),
                                      hdds_bad_season_farm = c(0, 3, 0, 0, 0),
                                      hdds_last_month = as.numeric(c(NA, NA, NA, NA, NA))),
                                 row.names = c(NA, -5L), class = c("tbl_df", "tbl", "data.frame"))


  expect_equal(actual_result, expected_result)

  actual_result <-hdds_calc(hdds_14_test)
  expected_result <- structure(list(hdds_bad_season = c(0, 4, 0, 5, 5),
                                    hdds_bad_season_bought = c(0, 4, 0, 3, 4),
                                    hdds_bad_season_farm = c(0, 0, 0, 4, 4),
                                    hdds_good_season = c(0, 5, 0, 4, 8),
                                    hdds_good_season_bought = c(0, 5, 0, 2, 7),
                                    hdds_good_season_farm = c(0, 0, 0, 3, 5),
                                    hdds_last_month = c(9, 6, 5, 4, 7),
                                    hdds_last_month_bought = c(6, 0, 3, 0, 0),
                                    hdds_last_month_farm = c(5, 0, 5, 0, 0),
                                    hdds_last_24hr = as.numeric(c(NA,NA,NA,NA,NA))),
                               row.names = c(NA, -5L), class = c("tbl_df", "tbl", "data.frame"))

  expect_equal(actual_result, expected_result)
})
