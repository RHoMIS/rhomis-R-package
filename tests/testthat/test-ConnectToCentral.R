library(testthat)
library(tibble)
library(dplyr)
testthat::test_that("Test that central results can be converted to a tibble", {
    input <- list(list("id"=1,"name"="name1","email"="email1"),
                  list("id"=2,"name"="name2","email"="email2"),
                  list("id"=3,"name"="name3","email"="email3"))

    expected_result <- tibble::as_tibble(list(id=c(1,2,3),
                                              name=c("name1","name2","name3"),
                                              email=c("email1","email2","email3")))
    expected_result <- expected_result %>% dplyr::mutate_all(as.character)
    actual_result <- central_results_to_df(list(list("id"=1,"name"="name1","email"="email1"),
                                                list("id"=2,"name"="name2","email"="email2"),
                                                list("id"=3,"name"="name3","email"="email3")))

    expect_equal(actual_result,expected_result)
})

testthat::test_that("Test that can convert an individual central result to a tibble",{
    central_results <- list(list("id"=1,"name"="name1","email"="email1"),
                            list("id"=2,"name"="name2","email"="email2"),
                            list("id"=3,"name"="name3","email"="email3"))
    column_headers <- unique(names(unlist(central_results)))
    individual_central_result <- central_results[2]

    expected_result <- tibble::as_tibble(list(id=c(2),
                                              name=c("name2"),
                                              email=c("email2")))
    expected_result <- expected_result %>% dplyr::mutate_all(as.character)

    actual_result <- widen_individual_result(individual_central_result,column_headers)

    expect_equal(actual_result,expected_result)

})


testthat::test_that("Can delete extra columns added to central",{
    data <- tibble::as_tibble(list(crop_name_1=c(1,2,3),
                                   livestock_name_1=c(1,3,2),
                                   crop_no1_1=c(NA,NA,NA),
                                   ls_no22_1=c(NA,NA,NA),
                                   off_farm_income_no3_1=c(NA,NA,NA)))

    actual_result <- remove_extra_central_columns(data)

    expected_result <- tibble::as_tibble(list(crop_name_1=c(1,2,3),
                                              livestock_name_1=c(1,3,2)))

    testthat::expect_equal(actual_result,expected_result)

})


httptest::with_mock_api({
    # 7f096f-POST.json
    testthat::test_that("Can connect to ODK central",{
        token <- get_email_token(central_url = "test-central-url.com",
                                 central_email = "test-email.com",
                                 central_password = "test-central-password"
        )

        # print(token)

        expect_equal(token,
                     "lSpAIeksRu1CNZs7!qjAot2T17dPzkrw9B4iTtpj7OoIJBmXvnHM8z8Ka4QPEjR7")
    })


    testthat::test_that("Can get central data",{


        # use_mock_api()
        file_destination = "./tests/testthat/test-central-url.com/v1/projects/test-project/forms/test-user/temp_submissions.zip"
        testing_dir <- getwd()
            if (grepl("tests/testthat", testing_dir)){
                file_destination <- "./test-central-url.com/v1/projects/test-project/forms/test-user/temp_submissions.zip"

            }

           data <-  get_submission_data(
                central_url = "https://test-central-url.com",
                central_email = "test-email.com",
                central_password = "test-central-password",
                projectID = "test-project",
                formID = "test-user",
                draft = F,
                file_destination = file_destination
            )

           testthat::expect_equal(!is.null(data), T)
    })
})







