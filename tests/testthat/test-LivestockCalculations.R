library(testthat)
library(tibble)

test_that("Can calculate livestock_prices", {

  data <- tibble::as_tibble(list(random_other_col=c("x","y","z"),
                                 livestock_sold_1=c(3, NA, 2),
                                 livestock_sale_income_1=c(200,NA,150),
                                 livestock_sold_2=c(1,5,4),
                                 livestock_sale_income_2=c(100,350,NA),
                                 livestock_sold_3=c(NA,NA,5),
                                 livestock_sale_income_3=c(NA,NA,50)
  ))

  expected_result <- tibble::as_tibble(list(random_other_col=c("x","y","z"),
                                            livestock_sold_1=c(3, NA, 2),
                                            livestock_sale_income_1=c(200,NA,150),
                                            livestock_price_per_animal_1=c(66.667,NA,75),
                                            livestock_sold_2=c(1,5,4),
                                            livestock_sale_income_2=c(100,350,NA),
                                            livestock_price_per_animal_2=c(100,70,NA),
                                            livestock_sold_3=c(NA,NA,5),
                                            livestock_sale_income_3=c(NA,NA,50),
                                            livestock_price_per_animal_3=c(NA,NA,10)
  ))

  actual_result <- price_per_livestock(data)


  expect_equal(actual_result, expected_result, tolerance = 0.001)
})


testthat::test_that("Can calculate kg of meat collected",{
  data <- tibble::as_tibble(list("livestock_name_1"=c("cattle","chicken","pigs"),
                                 "killed_for_meat_1"=c(2,12,3),
                                 "livestock_name_2"=c("chicken",NA,"cattle"),
                                 "killed_for_meat_2"=c(2,NA,3),
                                 "random_other_column"=c(NA,NA,NA)))

  expected_result <- tibble::as_tibble(list("livestock_name_1"=c("cattle","chicken","pigs"),
                                            "killed_for_meat_1"=c(2,12,3),
                                            "meat_kg_per_year_1"=c(500,12,450),
                                            "livestock_name_2"=c("chicken",NA,"cattle"),
                                            "killed_for_meat_2"=c(2,NA,3),
                                            "meat_kg_per_year_2"=c(2,NA,750),
                                            "random_other_column"=c(NA,NA,NA)))

  actual_result <- meat_amount_calculation(data)


  expect_equal(actual_result, expected_result)


})


testthat::test_that("Can calculate proportions of meat sold and consumed",{
  data <- tibble::as_tibble(list("livestock_name_1"=c("cattle","chicken","pigs"),
                                 "killed_for_meat_1"=c(2,12,3),
                                 "meat_kg_per_year_1"=c(500,12,450),
                                 "meat_use_1"=c("eat sell","eat sell","eat"),
                                 "meat_sell_amount_1"=c("little","half",NA),
                                 "meat_consumed_amount_1"=c("most","half",NA),

                                 "livestock_name_2"=c("chicken",NA,"cattle"),
                                 "killed_for_meat_2"=c(2,NA,3),
                                 "meat_kg_per_year_2"=c(2,NA,750),
                                 "random_other_column"=c(NA,NA,NA),
                                 "meat_use_2"=c("sell",NA,"eat sell"),
                                 "meat_sell_amount_2"=c(NA,NA,"underhalf"),
                                 "meat_consumed_amount_2"=c(NA,NA,"most")))

  expected_result <- tibble::as_tibble(list("livestock_name_1"=c("cattle","chicken","pigs"),
                                            "killed_for_meat_1"=c(2,12,3),
                                            "meat_kg_per_year_1"=c(500,12,450),
                                            "meat_use_1"=c("eat sell","eat sell","eat"),
                                            "meat_sell_amount_1"=c("little","half",NA),
                                            "meat_sold_props_numeric_1"=c(0.1,0.5,NA),
                                            "meat_consumed_amount_1"=c("most","half",NA),
                                            "meat_consumed_props_numeric_1"=c(0.7,0.5,1),

                                            "livestock_name_2"=c("chicken",NA,"cattle"),
                                            "killed_for_meat_2"=c(2,NA,3),
                                            "meat_kg_per_year_2"=c(2,NA,750),
                                            "random_other_column"=c(NA,NA,NA),
                                            "meat_use_2"=c("sell",NA,"eat sell"),
                                            "meat_sell_amount_2"=c(NA,NA,"underhalf"),
                                            "meat_sold_props_numeric_2"=c(1,NA,0.2),
                                            "meat_consumed_amount_2"=c(NA,NA,"most"),
                                            "meat_consumed_props_numeric_2"=c(NA,NA,0.7)
  ))


  actual_result <-meat_uses(data)
  expect_equal(actual_result, expected_result)

})
