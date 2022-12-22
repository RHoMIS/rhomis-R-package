library(testthat)
library(tibble)


testthat::test_that("Extract unique values from loop column", {
    column_pattern <- "crop_name"
    number_of_loops <- 2
    data <- tibble::tibble(
        crop_name_1 = c("maize", "cassava", "millet"),
        random_column = c("blue", "red", "green"),
        crop_name_2 = c("potato", NA, "cucumber")
    )
    expected_result <- c("maize", "cassava", "millet", "potato", "cucumber")
    result <- extract_new_values(data, loop_or_individual_column = "loop", column_pattern = "crop_name", number_of_loops = 2)

    expect_equal(result, expected_result)
})

testthat::test_that("Extract unique values from individual column", {
    column_name <- "random_column"
    data <- tibble::tibble(
        crop_name_1 = c("maize", NA, "cassava", NA, "millet"),
        random_column = c("blue", "purple", "red", NA, "green"),
        crop_name_2 = c(NA, "potato", NA, "cucumber", NA)
    )
    expected_result <- c("blue", "purple", "red", "green")
    result <- extract_new_values(data, loop_or_individual_column = "column", column_name = "random_column")

    expect_equal(result, expected_result)
})

testthat::test_that("Can extract unique values when don't know the number of loops", {
    column_pattern <- "crop_name"
    data <- tibble::tibble(
        crop_name_1 = c("maize", NA, "cassava", NA, "millet"),
        random_column = c("blue", "purple", "red", NA, "green"),
        crop_name_2 = c(NA, "apple", NA, "orange", NA),
        crop_name = c(NA, "potato", NA, "cucumber", NA),
        crop_name_3 = c(NA, "banana", NA, "grape", NA),
        my_crop_name_1 = c(NA, "coconut", NA, "mango", NA)
    )
    expected_result <- c("maize", "cassava", "millet", "apple", "orange", "banana", "grape")
    result <- find_loop_number_and_extract_values(data = data, column_pattern = column_pattern)

    expect_equal(result, expected_result)
})

testthat::test_that("Can extract units for core RHoMIS data", {
    data <- tibble::tibble(
        country = c("vietnam", "new-zealand"),
        crop_name_1 = c("maize", "cassava"),
        crop_name_2 = c(NA, "cucumber"),
        random_crop_name_2 = c("blue", "green"),
        livestock_name_1 = c("cow", "chicken"),
        livestock_name_2 = c("pig", NA),
        crop_yield_units_1 = c("kg", "sacks"),
        crop_yield_units_2 = c("wheelbarrow", NA),
        crop_yield_units_other_1 = c("other1", NA),
        crop_yield_units_other_2 = c("other2", "other3"),
        crop_sold_price_quantityunits_1 = c(NA, "price1"),
        crop_sold_price_quantityunits_2 = c("price2", "price3"),
        crop_price_quantityunits_other_1 = c(NA, "crop_price_1"),
        crop_price_quantityunits_other_2 = c(NA, "crop_price_2"),
        crop_price_quantityunits_other_3 = c(NA, "crop_price_3"),
        crop_price_quantityunits_other_4 = c(NA, NA),
        livestock_heads_weird_animal = c(NA, NA),
        unitland = c("acre", NA),
        areaunits_other = c("area1", "area2"),
        unitland_owned = c("unitland1", "unitland2"),
        unitland_rentin = c("renty1", "renty2"),
        unitland_rentout = c("rent1", "rent5"),
        milk_units_1 = c("milk1", NA),
        milk_units_2 = c("milk4", "milk5"),
        milk_amount_units_other_1 = c("milkoth1", "milkoth2"),
        milk_amount_units_other_2 = c(NA, "milkoth3"),
        milk_sold_price_timeunits_1 = c("mspt1", "mspt3"),
        milk_sold_price_timeunits_2 = c(NA, "mspt5"),
        milk_amount_time_units_other_1 = c("mspto1", NA),
        milk_amount_time_units_other_2 = c("mspto2", "mspto3"),
        bees_honey_production_units_1 = c("hnyprod1", "hnyprod2"),
        bees_honey_production_units_2 = c(NA, "hnyprod3"),
        bees_honey_production_units_other_1 = c("hnyprodoth1", NA),
        bees_honey_production_units_other_2 = c("hnyprodoth2", "hnyprodoth3"),
        eggs_units_1 = c("egg1", NA),
        eggs_units_2 = c("egg2", "egg3"),
        eggs_amount_units_other_1 = c("eggoth1", NA),
        eggs_amount_units_other_2 = c("eggoth2", "eggoth3"),
        eggs_sold_price_timeunits_1 = c("eggtime1", "eggtime2"),
        eggs_sold_price_timeunits_2 = c("eggtime3", NA),
        eggs_sold_price_timeunits_3 = c(NA, NA),
        eggs_sold_price_timeunits_other_1 = c("eggtimeoth1", NA),
        eggs_sold_price_timeunits_other_2 = c("eggtimeoth2", "eggtimeoth3"),
        fertiliser_units = c("fert1", "fert2"),
        fertiliser_units_other = c("fertoth1", "fertoth2")
    )


    expected_result <- c(list(
        "country_to_iso2" = c("vietnam", "new-zealand"),
        "crop_name_to_std" = c("maize", "cassava", "cucumber", NA),
        "livestock_name_to_std" = c("cow", "chicken", "pig", NA, "weird_animal"),
        "crop_amount_to_kg" = c("kg", "sacks", "wheelbarrow"),
        "crop_price_to_lcu_per_kg" = c("price1", "price2", "price3", "crop_price_1", "crop_price_2", "crop_price_3"),
        "land_area_to_ha" = c("acre", "unitland1", "unitland2", "renty1", "renty2", "rent1", "rent5", NA, "area1", "area2"),
        "milk_amount_to_l" = c("milk1", "milk4", "milk5", "milkoth1", "milkoth2", "milkoth3"),
        "milk_price_to_lcu_per_l" = c("mspt1", "mspt3", "mspt5", "mspto1", "mspto2", "mspto3"),
        "honey_amount_to_l" = c("hnyprod1", "hnyprod2", "hnyprod3", "hnyprodoth1", "hnyprodoth2", "hnyprodoth3"),
        "eggs_amount_to_pieces_per_year" = c("egg1", "egg2", "egg3", "eggoth1", "eggoth2", "eggoth3"),
        "eggs_price_to_lcu_per_year" = c("eggtime1", "eggtime2", "eggtime3", "eggtimeoth1", "eggtimeoth2", "eggtimeoth3"),
        "fertiliser_amount_to_kg" = c("fert1", "fert2", "fertoth1", "fertoth2")


    ))
    actual_result <- extract_new_core_units(data)
    expect_equal(actual_result, expected_result)
})




testthat::test_that("Can correctly merge categories for new unique values", {
    list_of_unique_core_values <- c(list(
        crop_sold_price_quantityunits = c("cropPriceA", "cropPriceB", "cropPriceC"),
        crop_price_quantityunits_other = c("cropPriceD", "cropPriceE", "cropPriceF"),
        random_unimportant_column = c("bla", "bla", "bla")
    ))
    main_item <- "crop_sold_price_quantityunits"
    categories_to_merge <- list(
        crop_name = c("crop_name"),
        livestock_name = c("livestock_name"),
        crop_yield_units = c("crop_yield_units_other"),
        crop_sold_price_quantityunits = c("crop_price_quantityunits_other"),
        unitland = c("areaunits_other", "unitland_owned", "unitland_rentin", "unitland_rentout"),
        milk_units = c("milk_amount_units_other"),
        milk_sold_price_timeunits = c("milk_amount_time_units_other"),
        bees_honey_production_units = c("bees_honey_production_units_other"),
        eggs_units = c("eggs_amount_units_other"),
        eggs_sold_price_timeunits = c("eggs_sold_price_timeunits_other"),
        fertiliser_units = c("fertiliser_units_other")
    )
    expected_result <- c("cropPriceA", "cropPriceB", "cropPriceC", "cropPriceD", "cropPriceE", "cropPriceF")
    actual_result <- merge_and_simplify_core_values(list_of_unique_core_values, main_item, categories_to_merge)

    expect_equal(actual_result, expected_result)
})

testthat::test_that("Test that new values can be correctly converted to tibble", {
    new_values <- c("egg1", "egg2", "egg3")
    expected_result <- tibble::as_tibble(list("survey_value" = c("egg1", "egg2", "egg3"), "conversion" = c(NA, NA, NA)))

    actual_result <- convert_new_values_to_tibble(new_values)
    expect_equal(actual_result, expected_result)
})



testthat::test_that("Can extract units from individual projects", {
    test_data <- tibble::tibble(
        id_rhomis_dataset = c("project_1", "project_2", "project_2", "project_3"),
        country = c("vietnam", "new-zealand", "new-zealand", "kenya"),
        crop_name_1 = c("maize", "cassava", "cucumber", "coconut"),
        crop_name_2 = c(NA, "cucumber", "sdgrfsedzrf", "wearsefd")
    )

    expected_result <- list(
        country_to_iso2 = tibble::as_tibble(
            list(
                survey_value = c("vietnam", "new-zealand", "kenya"),
                conversion = c(NA, NA, NA),
                unit_type = c("country_to_iso2", "country_to_iso2", "country_to_iso2"),
                id_rhomis_dataset = c("project_1", "project_2", "project_3")
            )
        ) %>% dplyr::mutate_all(as.character()),
        crop_name_to_std = tibble::as_tibble(
            list(
                survey_value = c("maize", NA, "cassava", "cucumber", "sdgrfsedzrf", NA, "coconut", "wearsefd", NA),
                conversion = c(NA, NA, NA, NA, NA, NA, NA, NA, NA),
                unit_type = c("crop_name_to_std", "crop_name_to_std", "crop_name_to_std", "crop_name_to_std", "crop_name_to_std", "crop_name_to_std", "crop_name_to_std", "crop_name_to_std", "crop_name_to_std"),
                id_rhomis_dataset = c("project_1", "project_1", "project_2", "project_2", "project_2", "project_2", "project_3", "project_3", "project_3")
            )
        ),
        livestock_name_to_std = tibble::as_tibble(
            list(
                survey_value = as.character(c(NA, NA, NA)),
                conversion = c(NA, NA, NA),
                unit_type = c("livestock_name_to_std", "livestock_name_to_std", "livestock_name_to_std"),
                id_rhomis_dataset = c("project_1", "project_2", "project_3")
            )
        ),
        crop_amount_to_kg = tibble::as_tibble(
            list(
                survey_value = as.character(c(NA, NA, NA)),
                conversion = c(NA, NA, NA),
                unit_type = c("crop_amount_to_kg", "crop_amount_to_kg", "crop_amount_to_kg"),
                id_rhomis_dataset = c("project_1", "project_2", "project_3")
            )
        ),
        crop_price_to_lcu_per_kg = tibble::as_tibble(
            list(
                survey_value = as.character(c(NA, NA, NA)),
                conversion = c(NA, NA, NA),
                unit_type = c("crop_price_to_lcu_per_kg", "crop_price_to_lcu_per_kg", "crop_price_to_lcu_per_kg"),
                id_rhomis_dataset = c("project_1", "project_2", "project_3")
            )
        ),
        land_area_to_ha = tibble::as_tibble(
            list(
                survey_value = as.character(c(NA, NA, NA)),
                conversion = c(NA, NA, NA),
                unit_type = c("land_area_to_ha", "land_area_to_ha", "land_area_to_ha"),
                id_rhomis_dataset = c("project_1", "project_2", "project_3")
            )
        ),
        milk_amount_to_l = tibble::as_tibble(
            list(
                survey_value = as.character(c(NA, NA, NA)),
                conversion = c(NA, NA, NA),
                unit_type = c("milk_amount_to_l", "milk_amount_to_l", "milk_amount_to_l"),
                id_rhomis_dataset = c("project_1", "project_2", "project_3")
            )
        ),
        milk_price_to_lcu_per_l = tibble::as_tibble(
            list(
                survey_value = as.character(c(NA, NA, NA)),
                conversion = c(NA, NA, NA),
                unit_type = c("milk_price_to_lcu_per_l", "milk_price_to_lcu_per_l", "milk_price_to_lcu_per_l"),
                id_rhomis_dataset = c("project_1", "project_2", "project_3")
            )
        ),
        honey_amount_to_l = tibble::as_tibble(
            list(
                survey_value = as.character(c(NA, NA, NA)),
                conversion = c(NA, NA, NA),
                unit_type = c("honey_amount_to_l", "honey_amount_to_l", "honey_amount_to_l"),
                id_rhomis_dataset = c("project_1", "project_2", "project_3")
            )
        ),
        eggs_amount_to_pieces_per_year = tibble::as_tibble(
            list(
                survey_value = as.character(c(NA, NA, NA)),
                conversion = c(NA, NA, NA),
                unit_type = c("eggs_amount_to_pieces_per_year", "eggs_amount_to_pieces_per_year", "eggs_amount_to_pieces_per_year"),
                id_rhomis_dataset = c("project_1", "project_2", "project_3")
            )
        ),
        eggs_price_to_lcu_per_year = tibble::as_tibble(
            list(
                survey_value = as.character(c(NA, NA, NA)),
                conversion = c(NA, NA, NA),
                unit_type = c("eggs_price_to_lcu_per_year", "eggs_price_to_lcu_per_year", "eggs_price_to_lcu_per_year"),
                id_rhomis_dataset = c("project_1", "project_2", "project_3")
            )
        ),
        fertiliser_amount_to_kg = tibble::as_tibble(
            list(
                survey_value = as.character(c(NA, NA, NA)),
                conversion = c(NA, NA, NA),
                unit_type = c("fertiliser_amount_to_kg", "fertiliser_amount_to_kg", "fertiliser_amount_to_kg"),
                id_rhomis_dataset = c("project_1", "project_2", "project_3")
            )
        )

    )

    result <- extract_values_by_project(test_data)

    expect_equal(result, expected_result)
})

