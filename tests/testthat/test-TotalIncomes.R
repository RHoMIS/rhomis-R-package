library(tibble)

testthat::test_that("Can calculate total livestock incomes", {


    data <- tibble::as_tibble(list(livestock_name_1=c("w","x","y","z"),
                                    livestock_sale_income_1=c(10,NA,NA,15),
                                   meat_sold_income_1=c(NA,NA,10,20),
                                   milk_sold_income_per_year_1=c(NA,NA,50,40),
                                   eggs_income_per_year_1=c(NA,NA,10,30),
                                   bees_honey_sold_income_1=c(NA,NA,NA,10),

                                   livestock_name_2=c("a","b","e","c"),
                                   livestock_sale_income_2=c(NA,NA,30,12),
                                   meat_sold_income_2=c(NA,NA,NA,34),
                                   milk_sold_income_per_year_2=c(NA,NA,NA,50),
                                   eggs_income_per_year_2=c(NA,NA,NA,200),
                                   bees_honey_sold_income_2=c(NA,NA,NA,NA)))

    expected_result <- c(10,NA,100,411)

    actual_result <- total_livestock_income(data)

    expect_equal(actual_result, expected_result)
})


testthat::test_that("Can calculate total crop incomes",{
    data <- tibble::as_tibble(list(crop_name_1=c("a","b","c","d"),
                                   crop_yield_1=c(12,123,NA,789),
                                   crop_yield_units_1=c(NA,"seraf",NA,"fweaf"),
                                   crop_sold_price_quantityunits_1=c(NA,"few",NA,"dqwaed"),
                                   crop_income_per_year_1=c(NA,20,NA,60),

                                   crop_name_2=c("e","f","g","h"),
                                   crop_yield_2=c(NA,NA,"gsrdg","gsrd"),
                                   crop_yield_units_2=c(NA,NA,"serf","grwse"),
                                   crop_sold_price_quantityunits_2=c(NA,NA,"gwresg","gwresg"),
                                   crop_income_per_year_2=c(NA,NA,40,40)))

    expected_result <- c(NA,20,40,100)

    actual_result <- total_crop_income(data)

    expect_equal(actual_result, expected_result)
})

testthat::test_that("Can calculate total and off-farm incomes",{
    data <- tibble::as_tibble(list(offfarm_income_proportion=c("little","half",NA,NA,"most")))
    total_crop_income <- c(20,10,NA,NA,5)
    total_livestock_income <- c(NA,20,12,NA,10)

    expected_result <- tibble::as_tibble(list(off_farm_income=c(2.22222,30,NA,NA,35),
                                              total_income=c(22.22222,60,12,NA,50)))
    actual_result <- total_and_off_farm_incomes(data,total_crop_income,total_livestock_income)
    expect_equal(actual_result, expected_result, tolerance = 0.001)


})

testthat::test_that("Can calculate total off-farm income control",{
    data <- tibble::as_tibble(list(offfarm_income_name_1=c("own_business","shop","labour",NA),
                                   offfarm_who_control_revenue_1=c("male_adult female_adult","female_youth","male_adult",NA),

                                   offfarm_income_name_2=c("labour","labour",NA,NA),
                                   offfarm_who_control_revenue_2=c("female_youth","male_adult",NA,NA),

                                   offfarm_income_name_3=c("shop","otherbusiness",NA,NA),
                                   offfarm_who_control_revenue_3=c("female_adult female_youth","female_adult",NA,NA),

                                   offfarm_income_name_4=c("otherbusiness",NA,NA,NA),
                                   offfarm_who_control_revenue_4=c("female_adult",NA,NA,NA)))


    expected_result<-tibble::as_tibble(list(female_youth_off_farm_control_total=c(0.375,0.3333333,0,NA),
                                            male_youth_off_farm_control_total=c(0,0,0,NA),
                                            female_adult_off_farm_control_total=c(0.5,0.3333333,0,NA),
                                            male_adult_off_farm_control_total=c(0.125,0.3333333,1,NA)))


    actual_result <- gendered_off_farm_income_indicator(data)
    expect_equal(actual_result, expected_result, tolerance = 0.001)
})



