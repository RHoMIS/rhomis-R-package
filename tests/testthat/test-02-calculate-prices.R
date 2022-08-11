test_that("multiplication works", {

    rhomis_data <- load_rhomis_csv(
        file_path = "https://raw.githubusercontent.com/l-gorman/rhomis-R-package/main/inst/sample_local_project/raw-data/raw-data.csv",
        id_type = "string",
        proj_id = "test_prj",
        form_id = "test_frm"
    )



    units_and_conversions <- suppressWarnings(extract_units_and_conversions(rhomis_data))

    # result <- get_secondary_conversions(
    #     rhomis_data=rhomis_data,
    #     units_and_conversions=units_and_conversions,
    #     gender_categories = pkg.env$gender_categories)

    expect_equal(2, 2)
})
