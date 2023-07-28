library(testthat)
library(tibble)


testthat::test_that("Checking can convert household roster to categories", {

    data <- tibble::as_tibble(list(hh_pop_rep_num_1=c(1,1,1,1),
                           person_gender_1=c("M","F","M",NA),
                           head_person_1=c("Y","N","Y",NA),
                           person_age_1=c(25,46,33,45),
                           person_work_away_1=c("stay","away","stay","stay"),


                           hh_pop_rep_num_2=c(2,2,2,2),
                           person_gender_2=c("F","F","F","M"),
                           head_person_2=c("N","N","N","Y"),
                           person_age_2=c(32,12,3,81),
                           person_work_away_2=c("away","away","away","away"),


                           hh_pop_rep_num_3=c(3,3,3,3),
                           person_gender_3=c("M","M",NA,"F"),
                           head_person_3=c(NA,NA,"Y",NA),
                           person_age_3=c(12,21,45,NA),
                           person_work_away_3=c("stay","stay",NA,NA)
    ))



    expected_result <- tibble::as_tibble(list(household_person_category_1=c("males_25to50","females_25to50","males_25to50",NA),
                                      household_person_category_2=c("females_25to50","females_11to24","children_under4","males_50plus"),
                                      household_person_category_3=c("males_11to24","males_11to24",NA,NA)))

    actual_result <- household_roster_to_categories(data)

    expect_equal(actual_result, expected_result)
})

testthat::test_that("Can map household roster into traditional category format",{
    data <- tibble::as_tibble(list(hh_pop_rep_num_1=c(1,1,1,1),
                           person_gender_1=c("M","F","M",NA),
                           head_person_1=c("Y","N","Y",NA),
                           person_age_1=c(25,46,33,45),
                           person_work_away_1=c("stay","away","stay","stay"),


                           hh_pop_rep_num_2=c(2,2,2,2),
                           person_gender_2=c("F","F","F","M"),
                           head_person_2=c("N","N","N","Y"),
                           person_age_2=c(32,12,3,81),
                           person_work_away_2=c("away","away","away","away"),


                           hh_pop_rep_num_3=c(3,3,3,3),
                           person_gender_3=c("M","M",NA,"F"),
                           head_person_3=c(NA,NA,"Y",NA),
                           person_age_3=c(25,21,45,NA),
                           person_work_away_3=c("stay","stay",NA,NA)
    ))

    expected_result <- structure(list(children_under4 = c(0, 0, 1, 0),
                                      children_4to10 = c(0, 0, 0, 0),
                                      females_11to24 = c(0, 1, 0, 0),
                                      females_25to50 = c(1, 1, 0, 0),
                                      females_50plus = c(0, 0, 0, 0),
                                      males_11to24 = c(0, 1, 0, 0),
                                      males_25to50 = c(2, 0, 1, 0),
                                      males_50plus = c(0, 0, 0, 1)
                                      
    ),
                                 row.names = c(NA, -4L), class = c("tbl_df", "tbl", "data.frame"))

    actual_result <- household_roster_to_wide(data)

    expect_equal(actual_result,expected_result)
})


testthat::test_that("Can calculate MAE score",{

    # Testing data in its original format
    data <- tibble::as_tibble(list(children_under_4=c(1,0,0,0),
                           children_4to10=c(2,3,3,3),
                           males11to24=c(1,2,3,4),
                           females11to24=c(4,3,2,1),
                           males25to50=c(2,2,2,2),
                           female_25_to_50=c(1,1,1,1),
                           male_50_plus=c(0,1,1,3),
                           female_50_plus=c(0,3,2,1)))
    expected_result <- c(8.71, 11.59, 11.09, 12.05)

    actual_result <- calculate_MAE(data)

    expect_equal(actual_result, expected_result)

    # Testing with data in the household roster format
    data <- tibble::as_tibble(list(hh_pop_rep_num_1=c(1,1,1,1),
                           person_gender_1=c("M","F","M",NA),
                           head_person_1=c("Y","N","Y",NA),
                           person_age_1=c(25,46,33,45),
                           person_work_away_1=c("stay","away","stay","stay"),


                           hh_pop_rep_num_2=c(2,2,2,2),
                           person_gender_2=c("F","F","F","M"),
                           head_person_2=c("N","N","N","Y"),
                           person_age_2=c(32,12,3,81),
                           person_work_away_2=c("away","away","away","away"),


                           hh_pop_rep_num_3=c(3,3,3,3),
                           person_gender_3=c("M","M",NA,"F"),
                           head_person_3=c(NA,NA,"Y",NA),
                           person_age_3=c(25,21,45,NA),
                           person_work_away_3=c("stay","stay",NA,NA)
    ))

    actual_result <- calculate_MAE(data)
    expected_result <- c(2.86, 2.46, 1.5, 0.73)

    expect_equal(actual_result, expected_result)

})

testthat::test_that("Can calculate household size in terms of members",{
    data <- tibble::as_tibble(list(children_under4=c(1,0,0,0),
                           children_4to10=c(2,3,3,3),
                           males_11to24=c(1,2,3,4),
                           females_11to24=c(4,3,2,1),
                           males_25to50=c(2,2,2,2),
                           females_25to50=c(1,1,1,1),
                           males_50plus=c(0,1,1,3),
                           females_50plus=c(0,3,2,1)))
    expected_result <- c(11, 15, 14, 15)

    actual_result <- calculate_household_size_members(data)

    expect_equal(actual_result, expected_result)

    # Testing with data in the household roster format
    data <- tibble::as_tibble(list(hh_pop_rep_num_1=c(1,1,1,1),
                           person_gender_1=c("M","F","M",NA),
                           head_person_1=c("Y","N","Y",NA),
                           person_age_1=c(25,46,33,45),
                           person_work_away_1=c("stay","away","stay","stay"),


                           hh_pop_rep_num_2=c(2,2,2,2),
                           person_gender_2=c("F","F","F","M"),
                           head_person_2=c("N","N","N","Y"),
                           person_age_2=c(32,12,3,81),
                           person_work_away_2=c("away","away","away","away"),


                           hh_pop_rep_num_3=c(3,3,3,3),
                           person_gender_3=c("M","M",NA,"F"),
                           head_person_3=c(NA,NA,"Y",NA),
                           person_age_3=c(25,21,45,NA),
                           person_work_away_3=c("stay","stay",NA,NA)
    ))

    actual_result <- calculate_household_size_members(data)
    expected_result <- c(3, 3, 2, 1)

    expect_equal(actual_result, expected_result)
})


