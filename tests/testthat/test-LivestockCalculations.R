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

testthat::test_that("Can calculate kg values for meat sold and consumed",{
  data <- tibble::as_tibble(list("livestock_name_1"=c("cattle","chicken","pigs"),
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

  expected_result <- tibble::as_tibble(list("livestock_name_1"=c("cattle","chicken","pigs"),
                                            "killed_for_meat_1"=c(2,12,3),
                                            "meat_kg_per_year_1"=c(500,12,450),
                                            "meat_use_1"=c("eat sell","eat sell","eat"),
                                            "meat_sell_amount_1"=c("little","half",NA),
                                            "meat_sold_props_numeric_1"=c(0.1,0.5,NA),
                                            "meat_sold_kg_per_year_1"=c(50,6,NA),
                                            "meat_consumed_amount_1"=c("most","half",NA),
                                            "meat_consumed_props_numeric_1"=c(0.7,0.5,1),
                                            "meat_consumed_kg_per_year_1"=c(350,6,450),

                                            "livestock_name_2"=c("chicken",NA,"cattle"),
                                            "killed_for_meat_2"=c(2,NA,3),
                                            "meat_kg_per_year_2"=c(2,NA,750),
                                            "random_other_column"=c(NA,NA,NA),
                                            "meat_use_2"=c("sell",NA,"eat sell"),
                                            "meat_sell_amount_2"=c(NA,NA,"underhalf"),
                                            "meat_sold_props_numeric_2"=c(1,NA,0.2),
                                            "meat_sold_kg_per_year_2"=c(2,NA,150),
                                            "meat_consumed_amount_2"=c(NA,NA,"most"),
                                            "meat_consumed_props_numeric_2"=c(NA,NA,0.7),
                                            "meat_consumed_kg_per_year_2"=c(NA,NA,525)
  ))

  actual_result <- meat_sold_and_consumed_calculation(data)

  testthat::expect_equal(actual_result,expected_result)
})

testthat::test_that("Can calculate meat prices",{
  data <- tibble::as_tibble(list("livestock_name_1"=c("cattle","chicken","pigs"),
                                 "killed_for_meat_1"=c(2,12,3),
                                 "meat_kg_per_year_1"=c(500,12,450),
                                 "meat_use_1"=c("eat sell","eat sell","eat"),
                                 "meat_sell_amount_1"=c("little","half",NA),
                                 "meat_sold_props_numeric_1"=c(0.1,0.5,NA),
                                 "meat_sold_kg_per_year_1"=c(50,6,NA),
                                 "meat_sold_income_1"=c(20,5,NA),
                                 "meat_consumed_amount_1"=c("most","half",NA),
                                 "meat_consumed_props_numeric_1"=c(0.7,0.5,1),
                                 "meat_consumed_kg_per_year_1"=c(350,6,450),

                                 "livestock_name_2"=c("chicken",NA,"cattle"),
                                 "killed_for_meat_2"=c(2,NA,3),
                                 "meat_kg_per_year_2"=c(2,NA,750),
                                 "random_other_column"=c(NA,NA,NA),
                                 "meat_use_2"=c("sell",NA,"eat sell"),
                                 "meat_sell_amount_2"=c(NA,NA,"underhalf"),
                                 "meat_sold_props_numeric_2"=c(1,NA,0.2),
                                 "meat_sold_kg_per_year_2"=c(2,NA,150),
                                 "meat_sold_income_2"=c(3,NA,200),
                                 "meat_consumed_amount_2"=c(NA,NA,"most"),
                                 "meat_consumed_props_numeric_2"=c(NA,NA,0.7),
                                 "meat_consumed_kg_per_year_2"=c(NA,NA,525)
  ))

  expected_result <- tibble::as_tibble(list("livestock_name_1"=c("cattle","chicken","pigs"),
                                            "killed_for_meat_1"=c(2,12,3),
                                            "meat_kg_per_year_1"=c(500,12,450),
                                            "meat_use_1"=c("eat sell","eat sell","eat"),
                                            "meat_sell_amount_1"=c("little","half",NA),
                                            "meat_sold_props_numeric_1"=c(0.1,0.5,NA),
                                            "meat_sold_kg_per_year_1"=c(50,6,NA),
                                            "meat_sold_income_1"=c(20,5,NA),
                                            "meat_price_per_kg_1"=c(0.4,0.8333333,NA),
                                            "meat_consumed_amount_1"=c("most","half",NA),
                                            "meat_consumed_props_numeric_1"=c(0.7,0.5,1),
                                            "meat_consumed_kg_per_year_1"=c(350,6,450),

                                            "livestock_name_2"=c("chicken",NA,"cattle"),
                                            "killed_for_meat_2"=c(2,NA,3),
                                            "meat_kg_per_year_2"=c(2,NA,750),
                                            "random_other_column"=c(NA,NA,NA),
                                            "meat_use_2"=c("sell",NA,"eat sell"),
                                            "meat_sell_amount_2"=c(NA,NA,"underhalf"),
                                            "meat_sold_props_numeric_2"=c(1,NA,0.2),
                                            "meat_sold_kg_per_year_2"=c(2,NA,150),
                                            "meat_sold_income_2"=c(3,NA,200),
                                            "meat_price_per_kg_2"=c(1.5,NA,1.333333),

                                            "meat_consumed_amount_2"=c(NA,NA,"most"),
                                            "meat_consumed_props_numeric_2"=c(NA,NA,0.7),
                                            "meat_consumed_kg_per_year_2"=c(NA,NA,525)))

  actual_result <- meat_prices(data)


  testthat::expect_equal(actual_result,expected_result,tolerance = 0.001)
})

testthat::test_that("Can swap milk yield per animal units",{
  data <- tibble::as_tibble(list(milk_units=c("l/day","per animal per week", "0.3l/animal/day", "l/animal/day"),
                                 number_animals_milked=c(2,3,4,NA)))

  expected_result <- c("l/day", "156.43", "438",NA)

  actual_result <- milk_swap_per_animal_units(units_column=data$milk_units,
                                              number_of_animals_milked_column=data$number_animals_milked)


  expect_equal(actual_result,expected_result, tolerance = 0.001)

})

testthat::test_that("Can correctly calculate average harvests for good and bad season",{
  good_season_amount <- c(2,5,NA)
  bad_season_amount <- c(1,NA,NA)

  expected_result <- c(1.5, 5, NA)

  actual_result <- average_good_and_bad_season(good_season_amount=good_season_amount,
                                               bad_season_amount=bad_season_amount)


  expect_equal(actual_result, expected_result)
})


testthat::test_that("Can calculate the amounts of milk collected",{
  data <- tibble::as_tibble(list("milk_amount_good_season_1"=c(0.5,2,1),
                                 "milk_units_1"=c("l/day","per animal per week","0.3l/animal/day"),
                                 "milk_amount_bad_season_1"=c(0.25,NA,0.75),
                                 "milk_number_animals_milked_1"=c(3,2,4),

                                 "milk_amount_good_season_2"=c(NA,NA,1.2),
                                 "milk_units_2"=c(NA,NA,"0.3l/day"),
                                 "milk_amount_bad_season_2"=c(NA,NA,1.5),
                                 "milk_number_animals_milked_2"=c(NA,NA,1.25)))

  expected_result <- tibble::as_tibble(list("milk_amount_good_season_1"=c(0.5,2,1),
                                            "milk_amount_good_season_litres_per_year_1"=c(182.5,208.5714,438),
                                            "milk_units_1"=c("l/day","per animal per week","0.3l/animal/day"),
                                            "milk_amount_units_numeric_1"=c(365,2*365/7,0.3*4*365),
                                            "milk_amount_bad_season_1"=c(0.25,NA,0.75),
                                            "milk_amount_bad_season_litres_per_year_1"=c(91.25,NA,328.5),
                                            "milk_number_animals_milked_1"=c(3,2,4),
                                            "milk_collected_litres_per_year_1"=c(136.875,208.5714,383.25),

                                            "milk_amount_good_season_2"=c(NA,NA,1.2),
                                            "milk_amount_good_season_litres_per_year_2"=c(NA,NA,131.4),
                                            "milk_units_2"=c(NA,NA,"0.3l/day"),
                                            "milk_amount_units_numeric_2"=c(NA,NA,0.3*365),

                                            "milk_amount_bad_season_2"=c(NA,NA,1.5),
                                            "milk_amount_bad_season_litres_per_year_2"=c(NA,NA,164.25),
                                            "milk_number_animals_milked_2"=c(NA,NA,1.25),
                                            "milk_collected_litres_per_year_2"=c(NA,NA,147.825)))

  actual_result <- milk_amount_calculations(data)

  testthat::expect_equal(actual_result, expected_result, tolerance = 0.01)
})

testthat::test_that("Can calculate proportions of milk sold and consumed",{
  data <- tibble::as_tibble(list("milk_amount_good_season_1"=c(0.5,2,1),
                                 "milk_amount_good_season_litres_per_year_1"=c(182.5,208.5714,438),
                                 "milk_units_1"=c("l/day","per animal per week","0.3l/animal/day"),
                                 "milk_amount_bad_season_1"=c(0.25,NA,0.75),
                                 "milk_amount_bad_season_litres_per_year_1"=c(91.25,NA,328.5),
                                 "milk_number_animals_milked_1"=c(3,2,4),
                                 "milk_collected_litres_per_year_1"=c(136.875,208.5714,383.25),
                                 "milk_use_1"=c("use","use sell","sell"),
                                 "milk_consumed_amount_1"=c(NA,"little",NA),
                                 "milk_sell_amount_1"=c(NA,"most",NA),

                                 "milk_amount_good_season_2"=c(NA,NA,1.2),
                                 "milk_amount_good_season_litres_per_year_2"=c(NA,NA,131.4),
                                 "milk_units_2"=c(NA,NA,"0.3l/day"),
                                 "milk_amount_bad_season_2"=c(NA,NA,1.5),
                                 "milk_amount_bad_season_litres_per_year_2"=c(NA,NA,164.25),
                                 "milk_number_animals_milked_2"=c(NA,NA,1.25),
                                 "milk_collected_litres_per_year_2"=c(NA,NA,147.825),
                                 "milk_use_2"=c(NA,NA,"sell use"),
                                 "milk_consumed_amount_2"=c(NA,NA,"half"),
                                 "milk_sell_amount_2"=c(NA,NA,"half")))

  expected_result <- tibble::as_tibble(list("milk_amount_good_season_1"=c(0.5,2,1),
                                            "milk_amount_good_season_litres_per_year_1"=c(182.5,208.5714,438),
                                            "milk_units_1"=c("l/day","per animal per week","0.3l/animal/day"),
                                            "milk_amount_bad_season_1"=c(0.25,NA,0.75),
                                            "milk_amount_bad_season_litres_per_year_1"=c(91.25,NA,328.5),
                                            "milk_number_animals_milked_1"=c(3,2,4),
                                            "milk_collected_litres_per_year_1"=c(136.875,208.5714,383.25),
                                            "milk_use_1"=c("use","use sell","sell"),
                                            "milk_consumed_amount_1"=c(NA,"little",NA),
                                            "milk_consumed_prop_numeric_1"=c(1,0.1,NA),
                                            "milk_sell_amount_1"=c(NA,"most",NA),
                                            "milk_sold_prop_numeric_1"=c(NA,0.7,1),

                                            "milk_amount_good_season_2"=c(NA,NA,1.2),
                                            "milk_amount_good_season_litres_per_year_2"=c(NA,NA,131.4),
                                            "milk_units_2"=c(NA,NA,"0.3l/day"),
                                            "milk_amount_bad_season_2"=c(NA,NA,1.5),
                                            "milk_amount_bad_season_litres_per_year_2"=c(NA,NA,164.25),
                                            "milk_number_animals_milked_2"=c(NA,NA,1.25),
                                            "milk_collected_litres_per_year_2"=c(NA,NA,147.825),
                                            "milk_use_2"=c(NA,NA,"sell use"),
                                            "milk_consumed_amount_2"=c(NA,NA,"half"),
                                            "milk_consumed_prop_numeric_2"=c(NA,NA,0.5),
                                            "milk_sell_amount_2"=c(NA,NA,"half"),
                                            "milk_sold_prop_numeric_2"=c(NA,NA,0.5)))

  actual_result <- milk_proportions_all(data)

  expect_equal(actual_result, expected_result, tolerance=0.01)


})


testthat::test_that("Can calculate litre values of milk sold and consumed",{

  data <- tibble::as_tibble(list("milk_amount_good_season_1"=c(0.5,2,1),
                                 "milk_amount_good_season_litres_per_year_1"=c(182.5,208.5714,438),
                                 "milk_units_1"=c("l/day","per animal per week","0.3l/animal/day"),
                                 "milk_amount_bad_season_1"=c(0.25,NA,0.75),
                                 "milk_amount_bad_season_litres_per_year_1"=c(91.25,NA,328.5),
                                 "milk_number_animals_milked_1"=c(3,2,4),
                                 "milk_collected_litres_per_year_1"=c(136.875,208.5714,383.25),
                                 "milk_use_1"=c("use","use sell","sell"),
                                 "milk_consumed_amount_1"=c(NA,"little",NA),
                                 "milk_consumed_prop_numeric_1"=c(1,0.1,NA),
                                 "milk_sell_amount_1"=c(NA,"most",NA),
                                 "milk_sold_prop_numeric_1"=c(NA,0.7,1),

                                 "milk_amount_good_season_2"=c(NA,NA,1.2),
                                 "milk_amount_good_season_litres_per_year_2"=c(NA,NA,131.4),
                                 "milk_units_2"=c(NA,NA,"0.3l/day"),
                                 "milk_amount_bad_season_2"=c(NA,NA,1.5),
                                 "milk_amount_bad_season_litres_per_year_2"=c(NA,NA,164.25),
                                 "milk_number_animals_milked_2"=c(NA,NA,1.25),
                                 "milk_collected_litres_per_year_2"=c(NA,NA,147.825),
                                 "milk_use_2"=c(NA,NA,"sell use"),
                                 "milk_consumed_amount_2"=c(NA,NA,"half"),
                                 "milk_consumed_prop_numeric_2"=c(NA,NA,0.5),
                                 "milk_sell_amount_2"=c(NA,NA,"half"),
                                 "milk_sold_prop_numeric_2"=c(NA,NA,0.5)))

  expected_result <- tibble::as_tibble(list("milk_amount_good_season_1"=c(0.5,2,1),
                                            "milk_amount_good_season_litres_per_year_1"=c(182.5,208.5714,438),
                                            "milk_units_1"=c("l/day","per animal per week","0.3l/animal/day"),
                                            "milk_amount_bad_season_1"=c(0.25,NA,0.75),
                                            "milk_amount_bad_season_litres_per_year_1"=c(91.25,NA,328.5),
                                            "milk_number_animals_milked_1"=c(3,2,4),
                                            "milk_collected_litres_per_year_1"=c(136.875,208.5714,383.25),
                                            "milk_use_1"=c("use","use sell","sell"),
                                            "milk_consumed_amount_1"=c(NA,"little",NA),
                                            "milk_consumed_prop_numeric_1"=c(1,0.1,NA),
                                            "milk_consumed_litres_per_year_1"=c(136.875,20.85714,NA),
                                            "milk_sell_amount_1"=c(NA,"most",NA),
                                            "milk_sold_prop_numeric_1"=c(NA,0.7,1),
                                            "milk_sold_litres_per_year_1"=c(NA,146,383.25),

                                            "milk_amount_good_season_2"=c(NA,NA,1.2),
                                            "milk_amount_good_season_litres_per_year_2"=c(NA,NA,131.4),
                                            "milk_units_2"=c(NA,NA,"0.3l/day"),
                                            "milk_amount_bad_season_2"=c(NA,NA,1.5),
                                            "milk_amount_bad_season_litres_per_year_2"=c(NA,NA,164.25),
                                            "milk_number_animals_milked_2"=c(NA,NA,1.25),
                                            "milk_collected_litres_per_year_2"=c(NA,NA,147.825),
                                            "milk_use_2"=c(NA,NA,"sell use"),
                                            "milk_consumed_amount_2"=c(NA,NA,"half"),
                                            "milk_consumed_prop_numeric_2"=c(NA,NA,0.5),
                                            "milk_consumed_litres_per_year_2"=c(NA,NA,73.9125),
                                            "milk_sell_amount_2"=c(NA,NA,"half"),
                                            "milk_sold_prop_numeric_2"=c(NA,NA,0.5),
                                            "milk_sold_litres_per_year_2"=c(NA,NA,73.9125)))

  actual_result <- milk_sold_and_consumed_calculations(data)

  testthat::expect_equal(actual_result, expected_result, tolerance = 0.001)
})


testthat::test_that("Can correctly convert income units for milk",{
  units_column <-c("litre","0.3l","week",NA,"month")
  sold_amount_column <- c(20,100,10,NA,1000)

  expected_result <- c(20, 333.333333, 365/7, NA, 365/28)

  actual_result <- milk_price_time_units_conversion(units_column= units_column,
                                                    sold_amount_column=sold_amount_column,
                                                    units=milk_price_time_units$unit,
                                                    unit_conversions= milk_price_time_units$conversion_factor)

  testthat::expect_equal(actual_result, expected_result, tolerance = 0.001)

})

testthat::test_that("Can correctly calculate milk incomes and prices",{
  data <- tibble::as_tibble(list("milk_amount_good_season_1"=c(0.5,2,1),
                                 "milk_amount_good_season_litres_per_year_1"=c(182.5,208.5714,438),
                                 "milk_units_1"=c("l/day","per animal per week","0.3l/animal/day"),
                                 "milk_amount_bad_season_1"=c(0.25,NA,0.75),
                                 "milk_amount_bad_season_litres_per_year_1"=c(91.25,NA,328.5),
                                 "milk_number_animals_milked_1"=c(3,2,4),
                                 "milk_collected_litres_per_year_1"=c(136.875,208.5714,383.25),
                                 "milk_use_1"=c("use","use sell","sell"),
                                 "milk_consumed_amount_1"=c(NA,"little",NA),
                                 "milk_consumed_prop_numeric_1"=c(1,0.1,NA),
                                 "milk_consumed_litres_per_year_1"=c(136.875,20.85714,NA),
                                 "milk_sell_amount_1"=c(NA,"most",NA),
                                 "milk_sold_prop_numeric_1"=c(NA,0.7,1),
                                 "milk_sold_litres_per_year_1"=c(NA,146,383.25),
                                 "milk_sold_income_1"=c(NA,10,30),
                                 "milk_sold_price_timeunits_1"=c(NA,"week","litre"),

                                 "milk_amount_good_season_2"=c(NA,NA,1.2),
                                 "milk_amount_good_season_litres_per_year_2"=c(NA,NA,131.4),
                                 "milk_units_2"=c(NA,NA,"0.3l/day"),
                                 "milk_amount_bad_season_2"=c(NA,NA,1.5),
                                 "milk_amount_bad_season_litres_per_year_2"=c(NA,NA,164.25),
                                 "milk_number_animals_milked_2"=c(NA,NA,1.25),
                                 "milk_collected_litres_per_year_2"=c(NA,NA,147.825),
                                 "milk_use_2"=c(NA,NA,"sell use"),
                                 "milk_consumed_amount_2"=c(NA,NA,"half"),
                                 "milk_consumed_prop_numeric_2"=c(NA,NA,0.5),
                                 "milk_consumed_litres_per_year_2"=c(NA,NA,73.9125),
                                 "milk_sell_amount_2"=c(NA,NA,"half"),
                                 "milk_sold_prop_numeric_2"=c(NA,NA,0.5),
                                 "milk_sold_litres_per_year_2"=c(NA,NA,73.9125),
                                 "milk_sold_income_2"=c(NA,NA,20),
                                 "milk_sold_price_timeunits_2"=c(NA,NA,"month")))

  expected_result <- tibble::as_tibble(list("milk_amount_good_season_1"=c(0.5,2,1),
                                            "milk_amount_good_season_litres_per_year_1"=c(182.5,208.5714,438),
                                            "milk_units_1"=c("l/day","per animal per week","0.3l/animal/day"),
                                            "milk_amount_bad_season_1"=c(0.25,NA,0.75),
                                            "milk_amount_bad_season_litres_per_year_1"=c(91.25,NA,328.5),
                                            "milk_number_animals_milked_1"=c(3,2,4),
                                            "milk_collected_litres_per_year_1"=c(136.875,208.5714,383.25),
                                            "milk_use_1"=c("use","use sell","sell"),
                                            "milk_consumed_amount_1"=c(NA,"little",NA),
                                            "milk_consumed_prop_numeric_1"=c(1,0.1,NA),
                                            "milk_consumed_litres_per_year_1"=c(136.875,20.85714,NA),
                                            "milk_sell_amount_1"=c(NA,"most",NA),
                                            "milk_sold_prop_numeric_1"=c(NA,0.7,1),
                                            "milk_sold_litres_per_year_1"=c(NA,146,383.25),
                                            "milk_sold_income_1"=c(NA,10,30),
                                            "milk_sold_income_per_year_1"=c(NA,10*365/7,30*383.25),
                                            "milk_price_per_litre_1"=c(NA,10*365/(7*146),30*383.25/383.25),
                                            "milk_sold_price_timeunits_1"=c(NA,"week","litre"),

                                            "milk_amount_good_season_2"=c(NA,NA,1.2),
                                            "milk_amount_good_season_litres_per_year_2"=c(NA,NA,131.4),
                                            "milk_units_2"=c(NA,NA,"0.3l/day"),
                                            "milk_amount_bad_season_2"=c(NA,NA,1.5),
                                            "milk_amount_bad_season_litres_per_year_2"=c(NA,NA,164.25),
                                            "milk_number_animals_milked_2"=c(NA,NA,1.25),
                                            "milk_collected_litres_per_year_2"=c(NA,NA,147.825),
                                            "milk_use_2"=c(NA,NA,"sell use"),
                                            "milk_consumed_amount_2"=c(NA,NA,"half"),
                                            "milk_consumed_prop_numeric_2"=c(NA,NA,0.5),
                                            "milk_consumed_litres_per_year_2"=c(NA,NA,73.9125),
                                            "milk_sell_amount_2"=c(NA,NA,"half"),
                                            "milk_sold_prop_numeric_2"=c(NA,NA,0.5),
                                            "milk_sold_litres_per_year_2"=c(NA,NA,73.9125),
                                            "milk_sold_income_2"=c(NA,NA,20),
                                            "milk_sold_income_per_year_2"=c(NA,NA,20*365/28),
                                            "milk_price_per_litre_2"=c(NA,NA,20*365/(28*73.9125)),
                                            "milk_sold_price_timeunits_2"=c(NA,NA,"month")
  ))

  actual_result <-milk_income_calculations(data)

  expect_equal(actual_result, expected_result, tolerance = 0.001)

})



testthat::test_that("Can match livestock heads to livestock names in loops",{
  livestock_name_column <- c("chicken",
                             "duck",
                             "otherpoultry",
                             "duck",
                             "random_animal")

  livestock_heads_df <- tibble::as_tibble(list("livestock_heads_cattle"=c(1,NA,NA,4,NA),
                                               "livestock_heads_chicken"=c(4,NA,NA,NA,2),
                                               "livestock_heads_duck"=c(3,6,NA,NA,5),
                                               "livestock_heads_otherpoultry"=c(7,NA,2,NA,1)))


  expected_result <- c(4,6,2,NA,NA)

  actual_result <-identify_number_of_heads_for_livestock_loops(livestock_name_column,livestock_heads_df)

  testthat::expect_equal(actual_result, expected_result)
})


testthat::test_that("Can convert from pieces/animal/day egg unit to numeric conversion",{
  units_column <- c("365",
                    "pieces/animal/day",
                    "pieces/animal/day",
                    NA )
  livestock_name_column <- c("chicken",
                             "duck",
                             "otherpoultry",
                             "duck")

  livestock_heads_df <- tibble::as_tibble(list("livestock_heads_cattle"=c(1,NA,NA,4),
                                               "livestock_heads_chicken"=c(4,NA,NA,NA),
                                               "livestock_heads_duck"=c(3,6,NA,NA),
                                               "livestock_heads_otherpoultry"=c(7,NA,2,NA)))
  expected_result <- c("365","2190","730",NA)
  actual_result <- eggs_swap_per_animal_units(units_column = units_column,
                             livestock_name_column = livestock_name_column,
                             livestock_heads_df = livestock_heads_df)

  testthat::expect_equal(actual_result, expected_result)


})

testthat::test_that("Can correctly calculate the amount of eggs collected",{
  egg_weight_kg <- 0.0496
  data <- tibble::as_tibble(list("livestock_heads_cattle"=c(1,2,3),
                                 "livestock_heads_chicken"=c(4,NA,5),
                                 "livestock_heads_duck"=c(2,3,1),

                                 "livestock_name_1"=c("chicken","duck","otherpoultry"),
                                 "eggs_amount_good_1"=c(3,4,2),
                                 "eggs_units_1"=c("pieces/day","pieces/animal/day","pieces/animal/day"),
                                 "eggs_amount_bad_1"=c(2,NA,1),
                                 "eggs_use_1"=c("use sell","use","sell"),
                                 "eggs_consumed_amount_1"=c("little",NA,NA),
                                 "eggs_sell_amount_1"=c("most",NA,NA),
                                 "eggs_sold_income_1"=c(25,NA,20),
                                 "eggs_sold_price_timeunits_1"=c("per_egg",NA,"day"),

                                 "livestock_name_2"=c("cattle","chicken","duck"),
                                 "eggs_amount_good_2"=c(NA,5,2),
                                 "eggs_units_2"=c(NA,"otherrandomeunit","pieces/animal/day"),
                                 "eggs_amount_bad_2"=c(NA,3,1),
                                 "eggs_use_2"=c(NA,"sell use","use sell"),
                                 "eggs_consumed_amount_2"=c(NA,"little","underhalf"),
                                 "eggs_sell_amount_2"=c(NA,"most","most"),
                                 "eggs_sold_income_2"=c(NA,40,50),
                                 "eggs_sold_price_timeunits_2"=c(NA,"month","week")))

  expected_result <- tibble::as_tibble(list("livestock_heads_cattle"=c(1,2,3),
                                            "livestock_heads_chicken"=c(4,NA,5),
                                            "livestock_heads_duck"=c(2,3,1),

                                            "livestock_name_1"=c("chicken","duck","otherpoultry"),
                                            "eggs_amount_good_1"=c(3,4,2),
                                            "eggs_amount_good_season_kg_per_year_1"=c(egg_weight_kg*3*365,egg_weight_kg*3*4*365,NA),
                                            "eggs_units_1"=c("pieces/day","pieces/animal/day","pieces/animal/day"),
                                            "eggs_amount_bad_1"=c(2,NA,1),
                                            "eggs_amount_bad_season_kg_per_year_1"=c(egg_weight_kg*2*365,NA,NA),
                                            "eggs_collected_kg_per_year_1"=c((egg_weight_kg*3*365+egg_weight_kg*2*365)/2,egg_weight_kg*3*4*365,NA),
                                            "eggs_use_1"=c("use sell","use","sell"),
                                            "eggs_consumed_amount_1"=c("little",NA,NA),
                                            "eggs_sell_amount_1"=c("most",NA,NA),
                                            "eggs_sold_income_1"=c(25,NA,20),
                                            "eggs_sold_price_timeunits_1"=c("per_egg",NA,"day"),

                                            "livestock_name_2"=c("cattle","chicken","duck"),
                                            "eggs_amount_good_2"=c(NA,5,2),
                                            "eggs_amount_good_season_kg_per_year_2"=c(NA,NA,egg_weight_kg*1*2*365),
                                            "eggs_units_2"=c(NA,"otherrandomeunit","pieces/animal/day"),
                                            "eggs_amount_bad_2"=c(NA,3,1),
                                            "eggs_amount_bad_season_kg_per_year_2"=c(NA,NA,egg_weight_kg*1*1*365),
                                            "eggs_collected_kg_per_year_2"=c(NA,NA,(egg_weight_kg*1*2*365+egg_weight_kg*1*1*365)/2),
                                            "eggs_use_2"=c(NA,"sell use","use sell"),
                                            "eggs_consumed_amount_2"=c(NA,"little","underhalf"),
                                            "eggs_sell_amount_2"=c(NA,"most","most"),
                                            "eggs_sold_income_2"=c(NA,40,50),
                                            "eggs_sold_price_timeunits_2"=c(NA,"month","week")))


  actual_result <- eggs_amount_calculations(data = data)

  testthat::expect_equal(actual_result,expected_result)

})


testthat::test_that("Can calculate proportions for eggs sold and consumed",{
  egg_weight_kg <- 0.0496

  data <- tibble::as_tibble(list("livestock_heads_cattle"=c(1,2,3),
                                 "livestock_heads_chicken"=c(4,NA,5),
                                 "livestock_heads_duck"=c(2,3,1),

                                 "livestock_name_1"=c("chicken","duck","otherpoultry"),
                                 "eggs_amount_good_1"=c(3,4,2),
                                 "eggs_amount_good_season_kg_per_year_1"=c(egg_weight_kg*3*365,egg_weight_kg*3*4*365,NA),
                                 "eggs_units_1"=c("pieces/day","pieces/animal/day","pieces/animal/day"),
                                 "eggs_amount_bad_1"=c(2,NA,1),
                                 "eggs_amount_bad_season_kg_per_year_1"=c(egg_weight_kg*2*365,NA,NA),
                                 "eggs_collected_kg_per_year_1"=c((egg_weight_kg*3*365*egg_weight_kg*2*365)/2,egg_weight_kg*3*4*365,NA),
                                 "eggs_use_1"=c("use sell","use","sell"),
                                 "eggs_consumed_amount_1"=c("little",NA,NA),
                                 "eggs_sell_amount_1"=c("most",NA,NA),
                                 "eggs_sold_income_1"=c(25,NA,20),
                                 "eggs_sold_price_timeunits_1"=c("per_egg",NA,"day"),

                                 "livestock_name_2"=c("cattle","chicken","duck"),
                                 "eggs_amount_good_2"=c(NA,5,2),
                                 "eggs_amount_good_season_kg_per_year_2"=c(NA,NA,egg_weight_kg*1*2*365),
                                 "eggs_units_2"=c(NA,"otherrandomeunit","pieces/animal/day"),
                                 "eggs_amount_bad_2"=c(NA,3,1),
                                 "eggs_amount_bad_season_kg_per_year_2"=c(NA,NA,egg_weight_kg*1*1*365),
                                 "eggs_collected_kg_per_year_2"=c(NA,NA,(egg_weight_kg*1*2*365*egg_weight_kg*1*1*365)/2),
                                 "eggs_use_2"=c(NA,"sell use","use sell"),
                                 "eggs_consumed_amount_2"=c(NA,"little","underhalf"),
                                 "eggs_sell_amount_2"=c(NA,"most","most"),
                                 "eggs_sold_income_2"=c(NA,40,50),
                                 "eggs_sold_price_timeunits_2"=c(NA,"month","week")))

  expected_result <- tibble::as_tibble(list("livestock_heads_cattle"=c(1,2,3),
                                            "livestock_heads_chicken"=c(4,NA,5),
                                            "livestock_heads_duck"=c(2,3,1),

                                            "livestock_name_1"=c("chicken","duck","otherpoultry"),
                                            "eggs_amount_good_1"=c(3,4,2),
                                            "eggs_amount_good_season_kg_per_year_1"=c(egg_weight_kg*3*365,egg_weight_kg*3*4*365,NA),
                                            "eggs_units_1"=c("pieces/day","pieces/animal/day","pieces/animal/day"),
                                            "eggs_amount_bad_1"=c(2,NA,1),
                                            "eggs_amount_bad_season_kg_per_year_1"=c(egg_weight_kg*2*365,NA,NA),
                                            "eggs_collected_kg_per_year_1"=c((egg_weight_kg*3*365*egg_weight_kg*2*365)/2,egg_weight_kg*3*4*365,NA),
                                            "eggs_use_1"=c("use sell","use","sell"),
                                            "eggs_consumed_amount_1"=c("little",NA,NA),
                                            "eggs_consumed_prop_numeric_1"=c(0.1,1,NA),
                                            "eggs_sell_amount_1"=c("most",NA,NA),
                                            "eggs_sold_prop_numeric_1"=c(0.7,NA,1),
                                            "eggs_sold_income_1"=c(25,NA,20),
                                            "eggs_sold_price_timeunits_1"=c("per_egg",NA,"day"),

                                            "livestock_name_2"=c("cattle","chicken","duck"),
                                            "eggs_amount_good_2"=c(NA,5,2),
                                            "eggs_amount_good_season_kg_per_year_2"=c(NA,NA,egg_weight_kg*1*2*365),
                                            "eggs_units_2"=c(NA,"otherrandomeunit","pieces/animal/day"),
                                            "eggs_amount_bad_2"=c(NA,3,1),
                                            "eggs_amount_bad_season_kg_per_year_2"=c(NA,NA,egg_weight_kg*1*1*365),
                                            "eggs_collected_kg_per_year_2"=c(NA,NA,(egg_weight_kg*1*2*365*egg_weight_kg*1*1*365)/2),
                                            "eggs_use_2"=c(NA,"sell use","use sell"),
                                            "eggs_consumed_amount_2"=c(NA,"little","underhalf"),
                                            "eggs_consumed_prop_numeric_2"=c(NA,0.1,0.2),
                                            "eggs_sell_amount_2"=c(NA,"most","most"),
                                            "eggs_sold_prop_numeric_2"=c(NA,0.7,0.7),
                                            "eggs_sold_income_2"=c(NA,40,50),
                                            "eggs_sold_price_timeunits_2"=c(NA,"month","week")))


actual_result <- eggs_proportions_all(data)

testthat::expect_equal(actual_result, expected_result)
})


testthat::test_that("Can calculate the amounts of eggs sold and consumed in kg",{
  egg_weight_kg <- 0.0496

  data <- tibble::as_tibble(list("livestock_heads_cattle"=c(1,2,3),
                                            "livestock_heads_chicken"=c(4,NA,5),
                                            "livestock_heads_duck"=c(2,3,1),

                                            "livestock_name_1"=c("chicken","duck","otherpoultry"),
                                            "eggs_amount_good_1"=c(3,4,2),
                                            "eggs_amount_good_season_kg_per_year_1"=c(egg_weight_kg*3*365,egg_weight_kg*3*4*365,NA),
                                            "eggs_units_1"=c("pieces/day","pieces/animal/day","pieces/animal/day"),
                                            "eggs_amount_bad_1"=c(2,NA,1),
                                            "eggs_amount_bad_season_kg_per_year_1"=c(egg_weight_kg*2*365,NA,NA),
                                            "eggs_collected_kg_per_year_1"=c((egg_weight_kg*3*365*egg_weight_kg*2*365)/2,egg_weight_kg*3*4*365,NA),
                                            "eggs_use_1"=c("use sell","use","sell"),
                                            "eggs_consumed_amount_1"=c("little",NA,NA),
                                            "eggs_consumed_prop_numeric_1"=c(0.1,1,NA),
                                            "eggs_sell_amount_1"=c("most",NA,NA),
                                            "eggs_sold_prop_numeric_1"=c(0.7,NA,1),
                                            "eggs_sold_income_1"=c(25,NA,20),
                                            "eggs_sold_price_timeunits_1"=c("per_egg",NA,"day"),

                                            "livestock_name_2"=c("cattle","chicken","duck"),
                                            "eggs_amount_good_2"=c(NA,5,2),
                                            "eggs_amount_good_season_kg_per_year_2"=c(NA,NA,egg_weight_kg*1*2*365),
                                            "eggs_units_2"=c(NA,"otherrandomeunit","pieces/animal/day"),
                                            "eggs_amount_bad_2"=c(NA,3,1),
                                            "eggs_amount_bad_season_kg_per_year_2"=c(NA,NA,egg_weight_kg*1*1*365),
                                            "eggs_collected_kg_per_year_2"=c(NA,NA,(egg_weight_kg*1*2*365*egg_weight_kg*1*1*365)/2),
                                            "eggs_use_2"=c(NA,"sell use","use sell"),
                                            "eggs_consumed_amount_2"=c(NA,"little","underhalf"),
                                            "eggs_consumed_prop_numeric_2"=c(NA,0.1,0.2),
                                            "eggs_sell_amount_2"=c(NA,"most","most"),
                                            "eggs_sold_prop_numeric_2"=c(NA,0.7,0.7),
                                            "eggs_sold_income_2"=c(NA,40,50),
                                            "eggs_sold_price_timeunits_2"=c(NA,"month","week")))

  expected_result <- tibble::as_tibble(list("livestock_heads_cattle"=c(1,2,3),
                                 "livestock_heads_chicken"=c(4,NA,5),
                                 "livestock_heads_duck"=c(2,3,1),

                                 "livestock_name_1"=c("chicken","duck","otherpoultry"),
                                 "eggs_amount_good_1"=c(3,4,2),
                                 "eggs_amount_good_season_kg_per_year_1"=c(egg_weight_kg*3*365,egg_weight_kg*3*4*365,NA),
                                 "eggs_units_1"=c("pieces/day","pieces/animal/day","pieces/animal/day"),
                                 "eggs_amount_bad_1"=c(2,NA,1),
                                 "eggs_amount_bad_season_kg_per_year_1"=c(egg_weight_kg*2*365,NA,NA),
                                 "eggs_collected_kg_per_year_1"=c((egg_weight_kg*3*365*egg_weight_kg*2*365)/2,egg_weight_kg*3*4*365,NA),
                                 "eggs_use_1"=c("use sell","use","sell"),
                                 "eggs_consumed_amount_1"=c("little",NA,NA),
                                 "eggs_consumed_prop_numeric_1"=c(0.1,1,NA),
                                 "eggs_consumed_kg_per_year_1"=c((0.1*egg_weight_kg*3*365*egg_weight_kg*2*365)/2,egg_weight_kg*3*4*365,NA),
                                 "eggs_sell_amount_1"=c("most",NA,NA),
                                 "eggs_sold_prop_numeric_1"=c(0.7,NA,1),
                                 "eggs_sold_kg_per_year_1"=c((0.7*egg_weight_kg*3*365*egg_weight_kg*2*365)/2,NA,NA),
                                 "eggs_sold_income_1"=c(25,NA,20),
                                 "eggs_sold_price_timeunits_1"=c("per_egg",NA,"day"),

                                 "livestock_name_2"=c("cattle","chicken","duck"),
                                 "eggs_amount_good_2"=c(NA,5,2),
                                 "eggs_amount_good_season_kg_per_year_2"=c(NA,NA,egg_weight_kg*1*2*365),
                                 "eggs_units_2"=c(NA,"otherrandomeunit","pieces/animal/day"),
                                 "eggs_amount_bad_2"=c(NA,3,1),
                                 "eggs_amount_bad_season_kg_per_year_2"=c(NA,NA,egg_weight_kg*1*1*365),
                                 "eggs_collected_kg_per_year_2"=c(NA,NA,(egg_weight_kg*1*2*365*egg_weight_kg*365)/2),
                                 "eggs_use_2"=c(NA,"sell use","use sell"),
                                 "eggs_consumed_amount_2"=c(NA,"little","underhalf"),
                                 "eggs_consumed_prop_numeric_2"=c(NA,0.1,0.2),
                                 "eggs_consumed_kg_per_year_2"=c(NA,NA,0.2*(egg_weight_kg*1*2*365*egg_weight_kg*1*1*365)/2),
                                 "eggs_sell_amount_2"=c(NA,"most","most"),
                                 "eggs_sold_prop_numeric_2"=c(NA,0.7,0.7),
                                 "eggs_sold_kg_per_year_2"=c(NA,NA,(0.7*egg_weight_kg*1*2*365*egg_weight_kg*365)/2),
                                 "eggs_sold_income_2"=c(NA,40,50),
                                 "eggs_sold_price_timeunits_2"=c(NA,"month","week")))

  actual_result <- eggs_sold_and_consumed_calculations(data)

  testthat::expect_equal(actual_result, expected_result)

})


testthat::test_that("Can convert from price per egg to numeric conversion factor",{
  egg_weight_kg <- 0.0496

  units_column <- c("per_egg", NA, "365","per_egg")
  amount_sold_column <- c(650,25,NA, 23)

  expected_result <- c(650/egg_weight_kg, NA, 365, 23/egg_weight_kg)
  expected_result <- as.character(expected_result)
  actual_result <- eggs_price_per_egg_to_numeric(units_column,amount_sold_column)
  testthat::expect_equal(actual_result, expected_result)

                    })

testthat::test_that("Can calculate income and prices for eggs",{
  egg_weight_kg <- 0.0496

  data <- tibble::as_tibble(list("livestock_heads_cattle"=c(1,2,3),
                                            "livestock_heads_chicken"=c(4,NA,5),
                                            "livestock_heads_duck"=c(2,3,1),

                                            "livestock_name_1"=c("chicken","duck","otherpoultry"),
                                            "eggs_amount_good_1"=c(3,4,2),
                                            "eggs_amount_good_season_kg_per_year_1"=c(egg_weight_kg*3*365,egg_weight_kg*3*4*365,NA),
                                            "eggs_units_1"=c("pieces/day","pieces/animal/day","pieces/animal/day"),
                                            "eggs_amount_bad_1"=c(2,NA,1),
                                            "eggs_amount_bad_season_kg_per_year_1"=c(egg_weight_kg*2*365,NA,NA),
                                            "eggs_collected_kg_per_year_1"=c((egg_weight_kg*3*365+egg_weight_kg*2*365)/2,egg_weight_kg*3*4*365,NA),
                                            "eggs_use_1"=c("use sell","use","sell"),
                                            "eggs_consumed_amount_1"=c("little",NA,NA),
                                            "eggs_consumed_prop_numeric_1"=c(0.1,1,NA),
                                            "eggs_consumed_kg_per_year_1"=c((0.1*egg_weight_kg*3*365+egg_weight_kg*2*365)/2,egg_weight_kg*3*4*365,NA),
                                            "eggs_sell_amount_1"=c("most",NA,NA),
                                            "eggs_sold_prop_numeric_1"=c(0.7,NA,1),
                                            "eggs_sold_kg_per_year_1"=c((0.7*egg_weight_kg*3*365+egg_weight_kg*2*365)/2,NA,NA),
                                            "eggs_sold_income_1"=c(25,NA,20),
                                            "eggs_sold_price_timeunits_1"=c("per_egg",NA,"day"),

                                            "livestock_name_2"=c("cattle","chicken","duck"),
                                            "eggs_amount_good_2"=c(NA,5,2),
                                            "eggs_amount_good_season_kg_per_year_2"=c(NA,NA,egg_weight_kg*1*2*365),
                                            "eggs_units_2"=c(NA,"otherrandomeunit","pieces/animal/day"),
                                            "eggs_amount_bad_2"=c(NA,3,1),
                                            "eggs_amount_bad_season_kg_per_year_2"=c(NA,NA,egg_weight_kg*1*1*365),
                                            "eggs_collected_kg_per_year_2"=c(NA,NA,(egg_weight_kg*1*2*365+egg_weight_kg*365)/2),
                                            "eggs_use_2"=c(NA,"sell use","use sell"),
                                            "eggs_consumed_amount_2"=c(NA,"little","underhalf"),
                                            "eggs_consumed_prop_numeric_2"=c(NA,0.1,0.2),
                                            "eggs_consumed_kg_per_year_2"=c(NA,NA,0.2*(egg_weight_kg*1*2*365+egg_weight_kg*365)/2),
                                            "eggs_sell_amount_2"=c(NA,"most","most"),
                                            "eggs_sold_prop_numeric_2"=c(NA,0.7,0.7),
                                            "eggs_sold_kg_per_year_2"=c(NA,NA,(0.7*egg_weight_kg*1*2*365+egg_weight_kg*365)/2),
                                            "eggs_sold_income_2"=c(NA,40,50),
                                            "eggs_sold_price_timeunits_2"=c(NA,"month","week")))

  expected_result <- tibble::as_tibble(list("livestock_heads_cattle"=c(1,2,3),
                                 "livestock_heads_chicken"=c(4,NA,5),
                                 "livestock_heads_duck"=c(2,3,1),

                                 "livestock_name_1"=c("chicken","duck","otherpoultry"),
                                 "eggs_amount_good_1"=c(3,4,2),
                                 "eggs_amount_good_season_kg_per_year_1"=c(egg_weight_kg*3*365,egg_weight_kg*3*4*365,NA),
                                 "eggs_units_1"=c("pieces/day","pieces/animal/day","pieces/animal/day"),
                                 "eggs_amount_bad_1"=c(2,NA,1),
                                 "eggs_amount_bad_season_kg_per_year_1"=c(egg_weight_kg*2*365,NA,NA),
                                 "eggs_collected_kg_per_year_1"=c((egg_weight_kg*3*365+egg_weight_kg*2*365)/2,egg_weight_kg*3*4*365,NA),
                                 "eggs_use_1"=c("use sell","use","sell"),
                                 "eggs_consumed_amount_1"=c("little",NA,NA),
                                 "eggs_consumed_prop_numeric_1"=c(0.1,1,NA),
                                 "eggs_consumed_kg_per_year_1"=c((0.1*egg_weight_kg*3*365+egg_weight_kg*2*365)/2,egg_weight_kg*3*4*365,NA),
                                 "eggs_sell_amount_1"=c("most",NA,NA),
                                 "eggs_sold_prop_numeric_1"=c(0.7,NA,1),
                                 "eggs_sold_kg_per_year_1"=c((0.7*egg_weight_kg*3*365+egg_weight_kg*2*365)/2,NA,NA),
                                 "eggs_sold_income_1"=c(25,NA,20),
                                 "eggs_sold_price_timeunits_1"=c("per_egg",NA,"day"),
                                 "eggs_income_per_year_1"=c(25*((0.7*egg_weight_kg*3*365+egg_weight_kg*2*365)/2)/egg_weight_kg,NA,20*365),
                                 "eggs_price_per_kg_1"=c(25/egg_weight_kg,NA,NA),

                                 "livestock_name_2"=c("cattle","chicken","duck"),
                                 "eggs_amount_good_2"=c(NA,5,2),
                                 "eggs_amount_good_season_kg_per_year_2"=c(NA,NA,egg_weight_kg*1*2*365),
                                 "eggs_units_2"=c(NA,"otherrandomeunit","pieces/animal/day"),
                                 "eggs_amount_bad_2"=c(NA,3,1),
                                 "eggs_amount_bad_season_kg_per_year_2"=c(NA,NA,egg_weight_kg*1*1*365),
                                 "eggs_collected_kg_per_year_2"=c(NA,NA,(egg_weight_kg*1*2*365+egg_weight_kg*365)/2),
                                 "eggs_use_2"=c(NA,"sell use","use sell"),
                                 "eggs_consumed_amount_2"=c(NA,"little","underhalf"),
                                 "eggs_consumed_prop_numeric_2"=c(NA,0.1,0.2),
                                 "eggs_consumed_kg_per_year_2"=c(NA,NA,0.2*(egg_weight_kg*1*2*365+egg_weight_kg*1*1*365)/2),
                                 "eggs_sell_amount_2"=c(NA,"most","most"),
                                 "eggs_sold_prop_numeric_2"=c(NA,0.7,0.7),
                                 "eggs_sold_kg_per_year_2"=c(NA,NA,(0.7*egg_weight_kg*1*2*365+egg_weight_kg*365)/2),
                                 "eggs_sold_income_2"=c(NA,40,50),
                                 "eggs_sold_price_timeunits_2"=c(NA,"month","week"),
                                 "eggs_income_per_year_2"=c(NA,40*365/28,50*365/7),
                                 "eggs_price_per_kg_2"=c(NA,NA,(50*365/7)/((0.7*egg_weight_kg*1*2*365+egg_weight_kg*365)/2))))

  actual_result <- egg_income_calculations(data)
  testthat::expect_equal(actual_result, expected_result)


  })


