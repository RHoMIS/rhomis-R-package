library(testthat)
# Test for shorten_individual_column_name
testthat::test_that("Raw column names successfully shorten", {
    long_name <- "xxxx/xxxxx/xxxxx/important_name"
    seperator <- "/"
    shortened_name <- shorten_individual_column_name(column_name = long_name, seperator)
    expect_equal(shortened_name, "important_name")
})

# Test for shorten_multiple_column_names
testthat::test_that("Multiple column names switched properly", {
    long_names <- c(
        "xxxx/xxxxx/xxxxx/important_name_1",
        "xxxx/xxxxx/xxxxx/important_name_2",
        "xxxx/xxxxx/xxxxx/important_name_3"
    )
    seperator <- "/"
    actual_result <- shorten_multiple_column_names(long_names, seperator)
    expected_result <- c("important_name_1", "important_name_2", "important_name_3")
    expect_equal(actual_result, expected_result)
})

# Test for modify_loop_name
testthat::test_that("Repeat loop single can be changed", {
    column_name <- "SECTION_Crop_Productivity/crop_repeat[2]/crop_name"
    loop_type <- "crop_repeat"
    modified_name <- modify_loop_name(column_name, loop_type)

    expect_equal(modified_name, "SECTION_Crop_Productivity/crop_repeat[2]/crop_name_2")
})

# Test for modifying multiple column names
test_that("Multiple repeat loops changed correctly", {
    loop_type <- "crop_repeat"
    original_names <- c(
        "SECTION_Crop_Productivity/crop_repeat[1]/crop_name",
        "SECTION_Crop_Productivity/crop_repeat[2]/crop_name",
        "SECTION_Crop_Productivity/crop_repeat[3]/crop_name",
        "SECTION_Crop_Productivity/crop_repeat/crop_name",
        "SECTION_Crop_Productivity/crop_repeat[4]/crop_name",
        "SECTION_Crop_Productivity/crop_repeat[5]/crop_name",
        "SECTION_Crop_Productivity/crop_repeat[20]/crop_name",
        "SECTION_Crop_Productivity/crop_repeat[100]/crop_name",
        "SECTION_Crop_Productivity/crop_repeat/crop_name"
    )
    expected_result <- c(result <- c(
        "SECTION_Crop_Productivity/crop_repeat[1]/crop_name_1",
        "SECTION_Crop_Productivity/crop_repeat[2]/crop_name_2",
        "SECTION_Crop_Productivity/crop_repeat[3]/crop_name_3",
        "SECTION_Crop_Productivity/crop_repeat/crop_name",
        "SECTION_Crop_Productivity/crop_repeat[4]/crop_name_4",
        "SECTION_Crop_Productivity/crop_repeat[5]/crop_name_5",
        "SECTION_Crop_Productivity/crop_repeat[20]/crop_name_20",
        "SECTION_Crop_Productivity/crop_repeat[100]/crop_name_100",
        "SECTION_Crop_Productivity/crop_repeat/crop_name"
    ))
    actual_result <- modify_loop_column_names(original_names, loop_type)
    expect_equal(actual_result, expected_result)
})

# Testing that all types of repeat loop can be modified
testthat::test_that("All types of repeat loop can be specified and modified", {
    repeat_columns <- c("crop_repeat", "livestock_repeat", "offfarm_repeat", "hh_rep")
    column_names <- c(
        "xxx/crop_repeat[1]/crop_name",
        "xxx/livestock_repeat[2]/livestock_name",
        "xxx/crop_repeat[3]/crop_name",
        "xx/crop_repeat/crop_name",
        "x/offfarm_repeat[4]/offfarm_name",
        "y/hh_rep[5]/person_name",
        "z/crop_repeat/crop_name"
    )
    expected_result <- c(
        "xxx/crop_repeat[1]/crop_name_1",
        "xxx/livestock_repeat[2]/livestock_name_2",
        "xxx/crop_repeat[3]/crop_name_3",
        "xx/crop_repeat/crop_name",
        "x/offfarm_repeat[4]/offfarm_name_4",
        "y/hh_rep[5]/person_name_5",
        "z/crop_repeat/crop_name"
    )
    actual_result <- modify_all_loop_column_names(column_names, repeat_columns)

    expect_equal(actual_result, expected_result)
})


# Final test to make sure that all column names are properly changed
testthat::test_that("All column names properly cleaned", {


    column_names <- c(
        "xxx/crop_repeat[1]/crop_name",
        "xxx/livestock_repeat[2]/livestock_name",
        "xxx/crop_repeat[3]/crop_name",
        "xx/crop_repeat/crop_name",
        "x/offfarm_repeat[4]/offfarm_name",
        "y/hh_rep[5]/person_name",
        "z/crop_repeat/crop_name",
        "xx/yy/dryseason_animal_graze_shortage/jan",
        "xx/yy/dryseason_animal_graze_shortage/feb",
        "xx/yy/wetseason_animal_graze_shortage/jan",
        "xx/yy/wetseason_animal_graze_shortage/feb",
        "zz/foodshortagetime_months_which/jan",
        "ww/Wild_Foods_details/collect_when/jan",
        "ww/Fruits_details/collect_when/jan"
    )

    expected_result <- c(
        "crop_name_1",
        "livestock_name_2",
        "crop_name_3",
        "xx-crop_repeat-crop_name",
        "offfarm_name_4",
        "person_name_5",
        "z-crop_repeat-crop_name",
        "dryseason_animal_graze_shortage-jan",
        "dryseason_animal_graze_shortage-feb",
        "wetseason_animal_graze_shortage-jan",
        "wetseason_animal_graze_shortage-feb",
        "foodshortagetime_months_which-jan",
        "wild_foods_details-collect_when-jan",
        "fruits_details-collect_when-jan"
    )

    expect_warning(clean_column_names(column_names))

    actual_result <- suppressWarnings(clean_column_names(column_names))
    expect_equal(actual_result, expected_result)
})

# Additional test
testthat::test_that("All column names are simplified without duplicates", {

  column_names <- c(
    "xx/yy/livestock/other1",
    "xx/yy/livestock_intro/livestock/other1",
    "xx/zz/livestock_intro/livestock_other1",
    "xx/zz/livestock_intro/livestock_other2"
  )

  expected_result <- c(
    "yy-livestock-other1",
    "livestock_intro-livestock-other1",
    "livestock_other1",
    "livestock_other2"
  )

  expect_warning(clean_column_names(column_names))

  actual_result <- suppressWarnings(clean_column_names(column_names))
  expect_equal(actual_result, expected_result)
})


testthat::test_that("Can identify general repeat column names", {

    column_names <- c(
        "xxx/crop_repeat[12]/crop_name",
        "xxx/livestock_repeat[12]/livestock_name",
        "xxx.other_repeat.10000..crop_name",
        "xx/crop_repeat/crop_name",
        "x/offfarm_repeat[4]/offfarm_name",
        "y/hh_rep[5]/person_name",
        "z/crop_repeat/crop_name"

    )
    expected_result <- c(
        "crop_repeat",
        "livestock_repeat",
        "other_repeat",
        "offfarm_repeat",
        "hh_rep"
    )
    actual_result <- identify_repeat_columns(column_names)
    expect_equal(actual_result, expected_result)


})
