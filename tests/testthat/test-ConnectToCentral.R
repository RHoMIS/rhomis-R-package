library(testthat)
test_that("Test that central results can be converted to a tibble", {
  input <- list(list("id"=1,"name"="name1","email"="email1"),
                list("id"=2,"name"="name2","email"="email2"),
                list("id"=3,"name"="name3","email"="email3"))

  expected_result <- as_tibble(list(id=c(1,2,3),
                                    name=c("name1","name2","name3"),
                                    email=c("email1","email2","email3")))
  expected_result <- expected_result %>% mutate_all(as.character)
  actual_result <- central_results_to_df(list(list("id"=1,"name"="name1","email"="email1"),
                                              list("id"=2,"name"="name2","email"="email2"),
                                              list("id"=3,"name"="name3","email"="email3")))

  expect_equal(actual_result,expected_result)
})

test_that("Test that can convert an individual central result to a tibble",{
    central_results <- list(list("id"=1,"name"="name1","email"="email1"),
                            list("id"=2,"name"="name2","email"="email2"),
                            list("id"=3,"name"="name3","email"="email3"))
    column_headers <- unique(names(unlist(central_results)))
    individual_central_result <- central_results[2]

    expected_result <- as_tibble(list(id=c(2),
                                      name=c("name2"),
                                      email=c("email2")))
    expected_result <- expected_result %>% mutate_all(as.character)

    actual_result <- widen_individual_result(individual_central_result,column_headers)

    expect_equal(actual_result,expected_result)

})
