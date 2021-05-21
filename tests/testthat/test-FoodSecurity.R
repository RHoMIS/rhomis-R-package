library(testthat)
library(tibble)


testthat::test_that("Calculate FIES score", {

    test_data <- tibble::as_tibble(list("FIES_1"=c("N","N","Y",NA),
                                "FIES_2"=c("Y","N","Y",NA),
                                "FIES_3"=c("N","Y","Y",NA),
                                "FIES_4"=c("N","N","Y",NA),
                                "FIES_5"=c("N","N","Y",NA),
                                "FIES_6"=c("N","N","Y",NA),
                                "FIES_7"=c("N","N","N",NA),
                                "FIES_8"=c("Y","Y","Y",NA),
                                "randomextracolumn"=c("bla","bla","bla",NA)))

    actual_result <- fies_score(test_data)
    expected_result <- c(2, 2, 7, NA)

    expect_equal(actual_result, expected_result)
})

testthat::test_that("Can calculate HFIAS",{
    data <- tibble::as_tibble(list(HFIAS_1=c("daily",NA,"daily","weekly","weekly","daily"),
                           HFIAS_2=c("weekly",NA,"daily","weekly","weekly","daily"),
                           HFIAS_3=c("weekly",NA,NA,"weekly","daily","daily"),
                           HFIAS_4=c(NA,NA,NA,"weekly","weekly","daily"),
                           HFIAS_5=c(NA,NA,NA,"weekly","daily","daily"),
                           HFIAS_6=c(NA,NA,NA,"weekly","daily","daily"),
                           HFIAS_7=c(NA,NA,NA,"weekly","weekly","daily"),
                           HFIAS_8=c(NA,NA,NA,NA,"daily","daily"),
                           HFIAS_9=c(NA,NA,NA,NA,NA,"daily")))

    expected_result <-c("ModeratelyFI", "FoodSecure", "MildlyFI","SeverelyFI","SeverelyFI", "SeverelyFI")
    actual_result <- hfias_score(data)

    expect_equal(actual_result, expected_result)
})


testthat::test_that("Can do both types of sec",{

    # Testing it can take HFIAS
    data <- tibble::as_tibble(list(HFIAS_1=c("daily",NA,"daily","weekly","weekly","daily"),
                           HFIAS_2=c("weekly",NA,"daily","weekly","weekly","daily"),
                           HFIAS_3=c("weekly",NA,NA,"weekly","daily","daily"),
                           HFIAS_4=c(NA,NA,NA,"weekly","weekly","daily"),
                           HFIAS_5=c(NA,NA,NA,"weekly","daily","daily"),
                           HFIAS_6=c(NA,NA,NA,"weekly","daily","daily"),
                           HFIAS_7=c(NA,NA,NA,"weekly","weekly","daily"),
                           HFIAS_8=c(NA,NA,NA,NA,"daily","daily"),
                           HFIAS_9=c(NA,NA,NA,NA,NA,"daily")))

    expected_result <-tibble::as_tibble(list(HFIAS_status=c("ModeratelyFI", "FoodSecure", "MildlyFI","SeverelyFI","SeverelyFI", "SeverelyFI")))
    actual_result <- food_security_calculations(data)
    expect_equal(actual_result, expected_result)

    # Testing can take FIES
    test_data <- tibble::as_tibble(list("FIES_1"=c("N","N","Y",NA),
                                "FIES_2"=c("Y","N","Y",NA),
                                "FIES_3"=c("N","Y","Y",NA),
                                "FIES_4"=c("N","N","Y",NA),
                                "FIES_5"=c("N","N","Y",NA),
                                "FIES_6"=c("N","N","Y",NA),
                                "FIES_7"=c("N","N","N",NA),
                                "FIES_8"=c("Y","Y","Y",NA),
                                "randomextracolumn"=c("bla","bla","bla",NA)))

    actual_result <- food_security_calculations(test_data)
    expected_result <- tibble::as_tibble(list(FIES_score=c(2, 2, 7, NA)))

    expect_equal(actual_result, expected_result)

})
