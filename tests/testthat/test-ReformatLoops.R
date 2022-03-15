library(testthat)
library(dplyr)
testthat::test_that("Test that can identify number of loops", {

    name_column <- "crop_name"
    data <- tibble::as_tibble(list(crop_name_1=c("banana", "cassava", NA, "millet"),
                                   crop_name_2=c("cassava",NA, "melon", "maize"),
                                   random_crop_name_2=c("blue", "green",  "red",NA),
                                   crop_name=c("orange", "purple", NA, "black")))

    actual_result <- find_number_of_loops(data,name_column)
    expected_result <- 2

    expect_equal(actual_result, expected_result)

})

testthat::test_that("Test can find unique values for the specific name column",{
    name_column <- "crop_name"
    data <- tibble::as_tibble(list(crop_name_1=c("banana", "cassava", NA, "millet"),
                                   crop_name_2=c("cassava",NA, "melon", "maize"),
                                   random_crop_name_2=c("blue", "green",  "red",NA),
                                   crop_name=c("orange", "purple", NA, "black")))

    expected_result <- c("banana","cassava","millet","melon","maize")
    actual_result <- find_unique_case(data, name_column)
    expect_equal(actual_result,expected_result)

    # Checking for case with no expected values
    data <- tibble::as_tibble(list(crop_name_1=c(NA, NA, NA, NA),
                                   crop_name_2=c(NA,NA, NA, NA),
                                   random_crop_name_2=c("blue", "green",  "red",NA),
                                   crop_name=c("orange", "purple", NA, "black")))

    name_column <- "crop_name"
    expected_result <- c("none")
    actual_result <- find_unique_case(data, name_column)
    expect_equal(actual_result,expected_result)
})

testthat::test_that("Individual loop variables can be properly formatted",{
    name_column <- "crop_name"
    variable_to_convert <- "crop_variable"
    data <- tibble::as_tibble(list(crop_name_1=c("banana", "cassava", NA, "millet"),
                                   crop_name_2=c("cassava",NA, "melon", "maize"),
                                   random_crop_name_2=c("blue", "green",  "red",NA),
                                   crop_name=c("orange", "purple", NA, "black"),
                                   crop_variable_1=c("ex1","ex2",NA,NA),
                                   crop_variable_2=c("ex3",NA,"ex4","ex5")))

    expected_result <- tibble::as_tibble(list(banana=c("ex1", NA, NA, NA),
                                              cassava=c("ex3", "ex2", NA, NA),
                                              millet=c(NA, NA, NA, NA),
                                              melon=c(NA, NA, "ex4", NA),
                                              maize=c(NA, NA, NA, "ex5")))


    expected_result <- tibble::as_tibble(list(banana=c("ex1", NA, NA, NA),
                                              cassava=c("ex3", "ex2", NA, NA),
                                              millet=c(NA, NA, NA, NA),
                                              melon=c(NA, NA, "ex4", NA),
                                              maize=c(NA, NA, NA, "ex5")))
    expected_result<- expected_result %>% dplyr::mutate_all(as.character)


    actual_result <- loop_to_column_conversion(data, name_column, variable_to_convert, type="chr")

    expect_equal(actual_result,expected_result)


    # Checking case for no values
    name_column <- "crop_name"
    variable_to_convert <- "crop_variable"
    data <- tibble::as_tibble(list(crop_name_1=c(NA, NA, NA, NA),
                                   crop_name_2=c(NA,NA, NA, NA),
                                   random_crop_name_2=c("blue", "green",  "red",NA),
                                   crop_name=c("orange", "purple", NA, "black"),
                                   crop_variable_1=c(NA,NA,NA,NA),
                                   crop_variable_2=c(NA,NA,NA,NA)))

    expected_result <- tibble::as_tibble(list(none=c(NA, NA, NA, NA)))
    expected_result<- expected_result %>% dplyr::mutate_all(as.character)
    actual_result <- loop_to_column_conversion(data, name_column, variable_to_convert, type="chr")

    expect_equal(actual_result,expected_result)

})


testthat::test_that("Test that we can convert multiple columns into the wide format",{

    name_column <- "crop_name"
    column_prefixes <- c("crop_variable", "crop_unit", "crop_price")
    types <- c("chr", "chr", "chr")

    data <- tibble::as_tibble(list(crop_name_1=c("banana", "cassava", NA, "millet"),
                                   crop_name_2=c("cassava",NA, "melon", "maize"),
                                   random_crop_name_2=c("blue", "green",  "red",NA),
                                   crop_name=c("orange", "purple", NA, "black"),
                                   crop_variable_1=c("ex1","ex2",NA,NA),
                                   crop_variable_2=c("ex3",NA,"ex4","ex5"),
                                   crop_unit_1=c("unit1","unit2","unit3",NA),
                                   crop_unit_2=c(NA,NA,"unit4","unit5"),
                                   crop_price_1=c(NA,NA,NA,NA),
                                   crop_price_2=c(NA,NA,NA,NA)))
    actual_result <- map_to_wide_format(data, name_column, column_prefixes, types)
    # Expected result
    crop_variable <- tibble::as_tibble(list(banana=c("ex1",NA,NA,NA),
                                            cassava=c("ex3","ex2",NA,NA),
                                            millet=c(NA,NA,NA,NA),
                                            melon=c(NA,NA,"ex4",NA),
                                            maize=c(NA,NA,NA,"ex5")))
    crop_variable<- crop_variable %>% dplyr::mutate_all(as.character)
    crop_unit <- tibble::as_tibble(list(banana=c("unit1",NA,NA,NA),
                                        cassava=c(NA,"unit2",NA,NA),
                                        millet=c(NA,NA,NA,NA),
                                        melon=c(NA,NA,"unit4",NA),
                                        maize=c(NA,NA,NA,"unit5")))
    crop_unit<- crop_unit %>% dplyr::mutate_all(as.character)
    crop_price <- tibble::as_tibble(list(banana=c(NA,NA,NA,NA),
                                         cassava=c(NA,NA,NA,NA),
                                         millet=c(NA,NA,NA,NA),
                                         melon=c(NA,NA,NA,NA),
                                         maize=c(NA,NA,NA,NA)))

    actual_result <- map_to_wide_format(data, name_column, column_prefixes, types)
    # Expected result
    crop_variable <- tibble::as_tibble(list(banana=c("ex1",NA,NA,NA),
                                            cassava=c("ex3","ex2",NA,NA),
                                            millet=c(NA,NA,NA,NA),
                                            melon=c(NA,NA,"ex4",NA),
                                            maize=c(NA,NA,NA,"ex5")))
    crop_variable<- crop_variable %>% dplyr::mutate_all(as.character)
    crop_unit <- tibble::as_tibble(list(banana=c("unit1",NA,NA,NA),
                                        cassava=c(NA,"unit2",NA,NA),
                                        millet=c(NA,NA,NA,NA),
                                        melon=c(NA,NA,"unit4",NA),
                                        maize=c(NA,NA,NA,"unit5")))
    crop_unit<- crop_unit %>% dplyr::mutate_all(as.character)
    crop_price <- tibble::as_tibble(list(banana=c(NA,NA,NA,NA),
                                         cassava=c(NA,NA,NA,NA),
                                         millet=c(NA,NA,NA,NA),
                                         melon=c(NA,NA,NA,NA),
                                         maize=c(NA,NA,NA,NA)))
    crop_price<- crop_price %>% dplyr::mutate_all(as.character)
    expected_result <- list(crop_variable=crop_variable,
                            crop_unit=crop_unit,
                            crop_price=crop_price)

    expect_equal(actual_result,expected_result)
})


testthat::test_that("Gender splitting of information works",{
    data <- tibble::as_tibble(list(crop_name_1=c("banana", "cassava", NA, "millet"),
                                   crop_name_2=c("cassava",NA, "melon", "maize"),
                                   random_crop_name_2=c("blue", "green",  "red",NA),
                                   crop_name=c("orange", "purple", NA, "black"),
                                   crop_control_1=c("male_adult female_adult","male_adult",NA,"female_youth"),
                                   crop_control_2=c("male_youth",NA,"female_youth","female_adult")))


    wide_data <- map_to_wide_format(data, "crop_name", c("crop_control"), c("chr"))
    gender_data <- wide_data$crop_control

    female_youth <- tibble::as_tibble(list(banana=c(0,NA,NA,NA),

                                           cassava=c(0,0,NA,NA),
                                           millet=c(NA,NA,NA,1),
                                           melon=c(NA,NA,1,NA),
                                           maize=c(NA,NA,NA,0)))
    female_adult <- tibble::as_tibble(list(banana=c(0.5,NA,NA,NA),
                                           cassava=c(0,0,NA,NA),
                                           millet=c(NA,NA,NA,0),
                                           melon=c(NA,NA,0,NA),
                                           maize=c(NA,NA,NA,1)))
    male_youth <- tibble::as_tibble(list(banana=c(0,NA,NA,NA),
                                         cassava=c(1,0,NA,NA),
                                         millet=c(NA,NA,NA,0),
                                         melon=c(NA,NA,0,NA),
                                         maize=c(NA,NA,NA,0)))
    male_adult <- tibble::as_tibble(list(banana=c(0.5,NA,NA,NA),
                                         cassava=c(0,1,NA,NA),
                                         millet=c(NA,NA,NA,0),
                                         melon=c(NA,NA,0,NA),
                                         maize=c(NA,NA,NA,0)))

    female_adult <- tibble::as_tibble(list(banana=c(0.5,NA,NA,NA),
                                           cassava=c(0,0,NA,NA),
                                           millet=c(NA,NA,NA,0),
                                           melon=c(NA,NA,0,NA),
                                           maize=c(NA,NA,NA,1)))
    male_youth <- tibble::as_tibble(list(banana=c(0,NA,NA,NA),
                                         cassava=c(1,0,NA,NA),
                                         millet=c(NA,NA,NA,0),
                                         melon=c(NA,NA,0,NA),
                                         maize=c(NA,NA,NA,0)))
    male_adult <- tibble::as_tibble(list(banana=c(0.5,NA,NA,NA),
                                         cassava=c(0,1,NA,NA),
                                         millet=c(NA,NA,NA,0),
                                         melon=c(NA,NA,0,NA),
                                         maize=c(NA,NA,NA,0)))
    expected_result <- list(female_youth=female_youth,
                            female_adult=female_adult,
                            male_youth=male_youth,
                            male_adult=male_adult)

    actual_result <- split_gender_data(gender_data)

    expect_equal(actual_result,expected_result)
})


testthat::test_that("Test that can work out gender proportion scores for a single column",{
    column <- c("male_adult female_adult","male_adult",NA,"female_youth")
    actual_result <- proportion_control_per_person(column)

    expected_result <- c(0.5,1,NA,1)
    expect_equal(actual_result,expected_result)

})

testthat::test_that("Test that can correctly identify the categories in a column", {
    item <- c("male_adult female_adult","male_adult",NA,"female_youth")
    category <- "female_youth"
    actual_result <-check_val_in_list(item, category)
    expected_result <- c(0,0,0,1)
    expect_equal(actual_result,expected_result)
    item <- c("male_adult female_adult","male_adult",NA,"female_youth")
    category <- "male_adult"
    actual_result <-check_val_in_list(item, category)
    expected_result <- c(1,1,0,0)
    expect_equal(actual_result,expected_result)
})

testthat::test_that("Test that can identify gender control proportions for individual categories",{

    data <- tibble::as_tibble(list(crop_name_1=c("banana", "cassava", NA, "millet"),
                                   crop_name_2=c("cassava",NA, "melon", "maize"),
                                   random_crop_name_2=c("blue", "green",  "red",NA),
                                   crop_name=c("orange", "purple", NA, "black"),
                                   crop_control_1=c("male_adult female_adult","male_adult",NA,"female_youth"),
                                   crop_control_2=c("male_youth",NA,"female_youth","female_adult")))

    # Converting the dataset to the wide format (i.e crops as header, and genders controlling for each crop)
    wide_data <- map_to_wide_format(data, "crop_name", c("crop_control"), c("chr"))
    genderdf <- wide_data$crop_control

    numberControllingDF <- genderdf %>% dplyr::mutate(across(.cols=everything(),~proportion_control_per_person(.x)))
    category <- "male_adult"
    actual_result <- gender_control_props(genderdf, numberControllingDF, category)

    expected_result <- tibble::as_tibble(list(banana=c(0.5,NA,NA,NA),
                                              cassava=c(0,1,NA,NA),
                                              millet=c(NA,NA,NA,0),
                                              melon=c(NA,NA,0,NA),
                                              maize=c(NA,NA,NA,0)))


    expect_equal(actual_result,expected_result)


})



testthat::test_that("Gender split for individual column works",{

    data <- c("male_adult","male_adult female_adult",NA,"female_youth male_adult female_adult")
    actual_result <- split_gender_columns(data)
    expected_result <- structure(list(female_youth = c(0, 0, NA, 0.333333333333333),
                                      female_adult = c(0, 0.5, NA, 0.333333333333333),
                                      male_youth = c(0, 0, NA, 0),
                                      male_adult = c(1, 0.5, NA, 0.333333333333333)),
                                 row.names = c(NA, -4L),
                                 class = c("tbl_df", "tbl", "data.frame"))
    expect_equal(actual_result,expected_result)


})


testthat::test_that("Can reformat ODK central loops into RHoMIS format",{

    central_core <- tibble::as_tibble(list("random_col_1"=c("x","y"),
                                           "random_col_2"=c("a","b"),
                                           "KEY"=c("uuid:3dbc6436-45a5-438e-9213-e13fefa40eac",
                                                   "uuid:076bb740-3542-49ab-a6b2-e5efb70d76eb")))

    loop_data <- tibble::as_tibble(list("hh_pop_rep"=c(1,2,3,4,5,1,2),
                                        "hh_pop_repeat_grp-hh_intro"=c("","","","","","",""),
                                        "hh_pop_repeat_grp-person_number"=c("","","","","","",""),
                                        "hh_pop_repeat_grp-person_gender"=c("M","F","F","F","M","M","F"),
                                        "hh_pop_repeat_grp-person_age"=c(58,25,12,45,12,56,22),
                                        "hh_pop_repeat_grp-head_person"=c("Y","N","N","N","N","Y","N"),
                                        "PARENT_KEY"=c("uuid:076bb740-3542-49ab-a6b2-e5efb70d76eb",
                                                       "uuid:076bb740-3542-49ab-a6b2-e5efb70d76eb",
                                                       "uuid:076bb740-3542-49ab-a6b2-e5efb70d76eb",
                                                       "uuid:076bb740-3542-49ab-a6b2-e5efb70d76eb",
                                                       "uuid:076bb740-3542-49ab-a6b2-e5efb70d76eb",
                                                       "uuid:3dbc6436-45a5-438e-9213-e13fefa40eac",
                                                       "uuid:3dbc6436-45a5-438e-9213-e13fefa40eac"),
                                        "KEY"=c("uuid:076bb740-3542-49ab-a6b2-e5efb70d76eb/survey_grp/SECTION_HOUSEHOLD_INFO/householdpopulation/hhpoprep[1]",
                                                "uuid:076bb740-3542-49ab-a6b2-e5efb70d76eb/survey_grp/SECTION_HOUSEHOLD_INFO/householdpopulation/hhpoprep[2]",
                                                "uuid:076bb740-3542-49ab-a6b2-e5efb70d76eb/survey_grp/SECTION_HOUSEHOLD_INFO/householdpopulation/hhpoprep[3]",
                                                "uuid:076bb740-3542-49ab-a6b2-e5efb70d76eb/survey_grp/SECTION_HOUSEHOLD_INFO/householdpopulation/hhpoprep[4]",
                                                "uuid:076bb740-3542-49ab-a6b2-e5efb70d76eb/survey_grp/SECTION_HOUSEHOLD_INFO/householdpopulation/hhpoprep[5]",
                                                "uuid:3dbc6436-45a5-438e-9213-e13fefa40eac/survey_grp/SECTION_HOUSEHOLD_INFO/householdpopulation/hhpoprep[1]",
                                                "uuid:3dbc6436-45a5-438e-9213-e13fefa40eac/survey_grp/SECTION_HOUSEHOLD_INFO/householdpopulation/hhpoprep[2]")))
    expected_result <- tibble::as_tibble(list("random_col_1"=c("x","y"),
                                              "random_col_2"=c("a","b"),
                                              "KEY"=c("uuid:3dbc6436-45a5-438e-9213-e13fefa40eac",
                                                      "uuid:076bb740-3542-49ab-a6b2-e5efb70d76eb"),
                                              "hh_pop_rep_1"=c(1,1),
                                              "hh_pop_repeat_grp-hh_intro_1"=c("",""),
                                              "hh_pop_repeat_grp-person_number_1"=c("",""),
                                              "hh_pop_repeat_grp-person_gender_1"=c("M","M"),
                                              "hh_pop_repeat_grp-person_age_1"=c(56,58),
                                              "hh_pop_repeat_grp-head_person_1"=c("Y","Y"),

                                              "hh_pop_rep_2"=c(2,2),
                                              "hh_pop_repeat_grp-hh_intro_2"=c("",""),
                                              "hh_pop_repeat_grp-person_number_2"=c("",""),
                                              "hh_pop_repeat_grp-person_gender_2"=c("F","F"),
                                              "hh_pop_repeat_grp-person_age_2"=c(22,25),
                                              "hh_pop_repeat_grp-head_person_2"=c("N","N"),

                                              "hh_pop_rep_3"=c(NA,3),
                                              "hh_pop_repeat_grp-hh_intro_3"=c(NA,""),
                                              "hh_pop_repeat_grp-person_number_3"=c(NA,""),
                                              "hh_pop_repeat_grp-person_gender_3"=c(NA,"F"),
                                              "hh_pop_repeat_grp-person_age_3"=c(NA,12),
                                              "hh_pop_repeat_grp-head_person_3"=c(NA,"N"),

                                              "hh_pop_rep_4"=c(NA,4),
                                              "hh_pop_repeat_grp-hh_intro_4"=c(NA,""),
                                              "hh_pop_repeat_grp-person_number_4"=c(NA,""),
                                              "hh_pop_repeat_grp-person_gender_4"=c(NA,"F"),
                                              "hh_pop_repeat_grp-person_age_4"=c(NA,45),
                                              "hh_pop_repeat_grp-head_person_4"=c(NA,"N"),

                                              "hh_pop_rep_5"=c(NA,5),
                                              "hh_pop_repeat_grp-hh_intro_5"=c(NA,""),
                                              "hh_pop_repeat_grp-person_number_5"=c(NA,""),
                                              "hh_pop_repeat_grp-person_gender_5"=c(NA,"M"),
                                              "hh_pop_repeat_grp-person_age_5"=c(NA,12),
                                              "hh_pop_repeat_grp-head_person_5"=c(NA,"N")
    ))


    actual_result <- central_loops_to_rhomis_loops(central_core, loop_data)

    expect_equal(actual_result,expected_result)


})


testthat::test_that("Gendered split of cropping information", {
    data <- structure(list(crop_name_1 = c("maize", "cassava", "wheat"),
                           crop_yield_1 = c(50, 20, 10),
                           crop_yield_units_1 = c("kg", "sacks_100kg", "tonnes"),
                           crop_harvest_kg_per_year_1 = c(50, 2000, 10000),
                           crop_use_1 = c("eat", "eat sell", "eat sell feed_livestock"),
                           crop_consumed_prop_1 = c(NA, "most", "half"),
                           crop_consumed_prop_numeric_1 = c(1, 0.7, 0.5),
                           crop_consumed_kg_per_year_1 = c(50, 1400, 5000),
                           crop_sold_prop_1 = c(NA, "little", "little"),
                           crop_sold_prop_numeric_1 = c(NA, 0.1, 0.1),
                           crop_sold_kg_per_year_1 = c(NA,  200, 1000),
                           crop_sold_income_1 = c(NA, 500, 800),
                           crop_sold_price_quantityunits_1 = c("other bla_unir", "price_per_bag_50kg", "price_per_quintal"),
                           crop_sold_units_numeric_1 = c(NA, "0.02", "0.01"),
                           crop_income_per_year_1 = c(NA, 2000, 8000),
                           crop_price_1 = c(NA, 10, 8),
                           crop_who_control_revenue_1=c(NA,"female_adult male_adult","female_youth male_youth"),
                           crop_consume_control_1=c("male_adult female_youth female_adult","female_adult","female_youth"),


                           crop_name_2 = c("wheat", "maize", "millet"),
                           crop_yield_2 = c(12, 8, NA),
                           crop_yield_units_2 = c("kg", "other_randome_unit", "unit_not_registered"),
                           crop_harvest_kg_per_year_2 = c(12, NA, NA),
                           crop_use_2 = c("eat sell", "eat sell",  "eat sell"),
                           crop_consumed_prop_2 = c("most", "half", "underhalf"),
                           crop_consumed_prop_numeric_2 = c(0.7, 0.5, 0.2),
                           crop_consumed_kg_per_year_2 = c(8.4, NA, NA),
                           crop_sold_prop_2 = c("little", "half", "most"),
                           crop_sold_prop_numeric_2 = c(0.1, 0.5, 0.7),
                           crop_sold_kg_per_year_2 = c(1.2, NA, NA),
                           crop_sold_income_2 = c(800, 1000, 800),
                           crop_sold_price_quantityunits_2 = c("total_income_per_year", "price_per_kg", "price_per_bag_100kg" ),
                           crop_sold_units_numeric_2 = c("total_income_per_year", "1", "0.01"),
                           crop_income_per_year_2 = c(800, NA, NA),
                           crop_price_2 = c(666.666666666667, NA, NA),
                           crop_who_control_revenue_2=c("male_adult female_adult",NA,NA),
                           crop_consume_control_2=c("male_adult female_youth",NA,NA),

                           crop_name_3 = c("rice", "other_vegetables", NA),
                           crop_yield_3 = c(25, 100, NA),
                           crop_yield_units_3 = c("kg", "cart_225kg", NA),
                           crop_harvest_kg_per_year_3 = c(25, 22500, NA),
                           crop_use_3 = c("sell", "eat", NA),
                           crop_consumed_prop_3 = c(NA, NA, NA),
                           crop_consumed_prop_numeric_3 = c(NA, 1, NA),
                           crop_consumed_kg_per_year_3 = c(NA, 22500, NA),
                           crop_sold_prop_3 = c(NA, NA, NA),
                           crop_sold_prop_numeric_3 = c(1, NA, NA),
                           crop_sold_kg_per_year_3 = c(25,  NA, NA),
                           crop_sold_income_3 = c(800, 600, NA),
                           crop_sold_price_quantityunits_3 = c("total_income_per_year", "price_per_bag_45kg", NA),
                           crop_sold_units_numeric_3 = c("total_income_per_year", "0.022", NA),
                           crop_income_per_year_3 = c(800, NA, NA),
                           crop_price_3 = c(32, NA, NA),
                           crop_who_control_revenue_3=c("female_adult",NA,NA),
                           crop_consume_control_3=c(NA,"male_adult",NA)),
                      row.names = c(NA, -3L),
                      class = c("tbl_df", "tbl", "data.frame"))

    expected_result <- structure(list(crop_name_1 = c("maize", "cassava", "wheat"),
                                      crop_yield_1 = c(50, 20, 10),
                                      crop_yield_units_1 = c("kg", "sacks_100kg", "tonnes"),
                                      crop_harvest_kg_per_year_1 = c(50, 2000, 10000),
                                      crop_use_1 = c("eat", "eat sell", "eat sell feed_livestock"),
                                      crop_consumed_prop_1 = c(NA, "most", "half"),
                                      crop_consumed_prop_numeric_1 = c(1, 0.7, 0.5),
                                      crop_consumed_kg_per_year_1 = c(50, 1400, 5000),
                                      crop_sold_prop_1 = c(NA, "little", "little"),
                                      crop_sold_prop_numeric_1 = c(NA, 0.1, 0.1),
                                      crop_sold_kg_per_year_1 = c(NA,  200, 1000),
                                      crop_sold_income_1 = c(NA, 500, 800),
                                      crop_sold_price_quantityunits_1 = c("other bla_unir", "price_per_bag_50kg", "price_per_quintal"),
                                      crop_sold_units_numeric_1 = c(NA, "0.02", "0.01"),
                                      crop_income_per_year_1 = c(NA, 2000, 8000),
                                      crop_price_1 = c(NA, 10, 8),
                                      crop_who_control_revenue_1=c(NA,"female_adult male_adult","female_youth male_youth"),
                                      male_adult_crop_sold_kg_per_year_1=c(NA,100,0),
                                      male_youth_crop_sold_kg_per_year_1=c(NA,0,500),
                                      female_adult_crop_sold_kg_per_year_1=c(NA,100,0),
                                      female_youth_crop_sold_kg_per_year_1=c(NA,0,500),
                                      crop_consume_control_1=c("male_adult female_youth female_adult","female_adult","female_youth"),
                                      male_adult_crop_consumed_kg_per_year_1=c(16.66667,0,0),
                                      male_youth_crop_consumed_kg_per_year_1=c(0,0,0),
                                      female_adult_crop_consumed_kg_per_year_1=c(16.66667,1400,0),
                                      female_youth_crop_consumed_kg_per_year_1=c(16.66667,0,5000),


                                      crop_name_2 = c("wheat", "maize", "millet"),
                                      crop_yield_2 = c(12, 8, NA),
                                      crop_yield_units_2 = c("kg", "other_randome_unit", "unit_not_registered"),
                                      crop_harvest_kg_per_year_2 = c(12, NA, NA),
                                      crop_use_2 = c("eat sell", "eat sell",  "eat sell"),
                                      crop_consumed_prop_2 = c("most", "half", "underhalf"),
                                      crop_consumed_prop_numeric_2 = c(0.7, 0.5, 0.2),
                                      crop_consumed_kg_per_year_2 = c(8.4, NA, NA),
                                      crop_sold_prop_2 = c("little", "half", "most"),
                                      crop_sold_prop_numeric_2 = c(0.1, 0.5, 0.7),
                                      crop_sold_kg_per_year_2 = c(1.2, NA, NA),
                                      crop_sold_income_2 = c(800, 1000, 800),
                                      crop_sold_price_quantityunits_2 = c("total_income_per_year", "price_per_kg", "price_per_bag_100kg" ),
                                      crop_sold_units_numeric_2 = c("total_income_per_year", "1", "0.01"),
                                      crop_income_per_year_2 = c(800, NA, NA),
                                      crop_price_2 = c(666.666666666667, NA, NA),
                                      crop_who_control_revenue_2=c("male_adult female_adult",NA,NA),
                                      male_adult_crop_sold_kg_per_year_2=c(0.6,NA,NA),
                                      male_youth_crop_sold_kg_per_year_2=c(0,NA,NA),
                                      female_adult_crop_sold_kg_per_year_2=c(0.6,NA,NA),
                                      female_youth_crop_sold_kg_per_year_2=c(0,NA,NA),
                                      crop_consume_control_2=c("male_adult female_youth",NA,NA),
                                      male_adult_crop_consumed_kg_per_year_2=c(4.2,NA,NA),
                                      male_youth_crop_consumed_kg_per_year_2=c(0,NA,NA),
                                      female_adult_crop_consumed_kg_per_year_2=c(0,NA,NA),
                                      female_youth_crop_consumed_kg_per_year_2=c(4.2,NA,NA),

                                      crop_name_3 = c("rice", "other_vegetables", NA),
                                      crop_yield_3 = c(25, 100, NA),
                                      crop_yield_units_3 = c("kg", "cart_225kg", NA),
                                      crop_harvest_kg_per_year_3 = c(25, 22500, NA),
                                      crop_use_3 = c("sell", "eat", NA),
                                      crop_consumed_prop_3 = c(NA, NA, NA),
                                      crop_consumed_prop_numeric_3 = c(NA, 1, NA),
                                      crop_consumed_kg_per_year_3 = c(NA, 22500, NA),
                                      crop_sold_prop_3 = c(NA, NA, NA),
                                      crop_sold_prop_numeric_3 = c(1, NA, NA),
                                      crop_sold_kg_per_year_3 = c(25,  NA, NA),
                                      crop_sold_income_3 = c(800, 600, NA),
                                      crop_sold_price_quantityunits_3 = c("total_income_per_year", "price_per_bag_45kg", NA),
                                      crop_sold_units_numeric_3 = c("total_income_per_year", "0.022", NA),
                                      crop_income_per_year_3 = c(800, NA, NA),
                                      crop_price_3 = c(32, NA, NA),
                                      crop_who_control_revenue_3=c("female_adult",NA,NA),
                                      male_adult_crop_sold_kg_per_year_3=c(0,NA,NA),
                                      male_youth_crop_sold_kg_per_year_3=c(0,NA,NA),
                                      female_adult_crop_sold_kg_per_year_3=c(25,NA,NA),
                                      female_youth_crop_sold_kg_per_year_3=c(0,NA,NA),
                                      crop_consume_control_3=c(NA,"male_adult",NA),
                                      male_adult_crop_consumed_kg_per_year_3=c(NA,22500,NA),
                                 male_youth_crop_consumed_kg_per_year_3=c(NA,0,NA),
                                 female_adult_crop_consumed_kg_per_year_3=c(NA,0,NA),
                                 female_youth_crop_consumed_kg_per_year_3=c(NA,0,NA)),
                                 row.names = c(NA, -3L),
                                 class = c("tbl_df", "tbl", "data.frame"))

    actual_result<-insert_gender_columns_in_core_data(data, original_column="crop_consumed_kg_per_year", control_column="crop_consume_control", loop_structure=T)
    actual_result<-insert_gender_columns_in_core_data(actual_result, original_column="crop_sold_kg_per_year", control_column="crop_who_control_revenue", loop_structure=T)

    expect_equal(actual_result, expected_result, tolerance=0.001)
})

