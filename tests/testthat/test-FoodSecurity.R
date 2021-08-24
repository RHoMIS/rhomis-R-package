library(testthat)
library(tibble)


testthat::test_that("Calculate FIES score", {

    test_data <- tibble::as_tibble(list("fies_1"=c("n","n","y",NA),
                                "fies_2"=c("y","n","y",NA),
                                "fies_3"=c("n","y","y",NA),
                                "fies_4"=c("n","n","y",NA),
                                "fies_5"=c("n","n","y",NA),
                                "fies_6"=c("n","n","y",NA),
                                "fies_7"=c("n","n","n",NA),
                                "fies_8"=c("y","y","y",NA),
                                "randomextracolumn"=c("bla","bla","bla",NA)))

    actual_result <- fies_score(test_data)
    expected_result <- c(2, 2, 7, NA)

    expect_equal(actual_result, expected_result)
})

testthat::test_that("Can calculate HFIAS",{
    data <- tibble::as_tibble(list(hfias_1=c("daily",NA,"daily","weekly","weekly","daily"),
                           hfias_2=c("weekly",NA,"daily","weekly","weekly","daily"),
                           hfias_3=c("weekly",NA,NA,"weekly","daily","daily"),
                           hfias_4=c(NA,NA,NA,"weekly","weekly","daily"),
                           hfias_5=c(NA,NA,NA,"weekly","daily","daily"),
                           hfias_6=c(NA,NA,NA,"weekly","daily","daily"),
                           hfias_7=c(NA,NA,NA,"weekly","weekly","daily"),
                           hfias_8=c(NA,NA,NA,NA,"daily","daily"),
                           hfias_9=c(NA,NA,NA,NA,NA,"daily")))

    expected_result <-c("moderately_fi", "food_secure", "mildly_fi","severely_fi","severely_fi", "severely_fi")
    actual_result <- hfias_score(data)

    expect_equal(actual_result, expected_result)
})


testthat::test_that("Can do both types of food security calculation",{

    # Testing it can take HFIAS
    data <- tibble::as_tibble(list(hfias_1=c("daily",NA,"daily","weekly","weekly","daily"),
                           hfias_2=c("weekly",NA,"daily","weekly","weekly","daily"),
                           hfias_3=c("weekly",NA,NA,"weekly","daily","daily"),
                           hfias_4=c(NA,NA,NA,"weekly","weekly","daily"),
                           hfias_5=c(NA,NA,NA,"weekly","daily","daily"),
                           hfias_6=c(NA,NA,NA,"weekly","daily","daily"),
                           hfias_7=c(NA,NA,NA,"weekly","weekly","daily"),
                           hfias_8=c(NA,NA,NA,NA,"daily","daily"),
                           hfias_9=c(NA,NA,NA,NA,NA,"daily")))

    expected_result <-tibble::as_tibble(list(hfias_status=c("moderately_fi", "food_secure", "mildly_fi","severely_fi","severely_fi", "severely_fi")))
    actual_result <- food_security_calculations(data)
    expect_equal(actual_result, expected_result)

    # Testing can take FIES
    test_data <- tibble::as_tibble(list("fies_1"=c("n","n","y",NA),
                                "fies_2"=c("y","n","y",NA),
                                "fies_3"=c("n","y","y",NA),
                                "fies_4"=c("n","n","y",NA),
                                "fies_5"=c("n","n","y",NA),
                                "fies_6"=c("n","n","y",NA),
                                "fies_7"=c("n","n","n",NA),
                                "fies_8"=c("y","y","y",NA),
                                "randomextracolumn"=c("bla","bla","bla",NA)))

    actual_result <- food_security_calculations(test_data)
    expected_result <- tibble::as_tibble(list(fies_score=c(2, 2, 7, NA)))

    expect_equal(actual_result, expected_result)

})
