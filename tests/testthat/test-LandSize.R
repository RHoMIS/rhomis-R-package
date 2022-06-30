test_that("Can calculate land areas", {
  data <- tibble::as_tibble(list(
    id_rhomis_dataset = c("project_1", "project_2", "project_3"),
    landcultivated = c(2, 5, 4),
    unitland = c("hectare", "m2", "other_unit"),
    landowned = c(4, 2, 1)
  ))

  expected_result <- tibble::as_tibble(list(
    land_cultivated = c(2, 0.0005, NA),
    land_owned = c(4, 0.0002, NA)
  ))
  actual_result <- land_size_calculation(data)
  testthat::expect_equal(actual_result, expected_result)
})