test_that("can create ID columns", {
    data <- tibble::as_tibble(list(
        "proj_id_column_test"=c("proj_1", "proj_2", "proj_1"),
        "form_id_column_test"=c("form_1", "form_1", "form_1"),
        "country_column_test"=c("country_x", "country_y", "country_x"),
        "unique_column_test"=c("a","b","c"),
        "hh_id_col_test"=c("a1","b1","t1"),
        "random_other_column"=c("srrfe","serasefdae","sresf")
    ))


    result <- make_id_columns(data=data,
                    country_column = "country_column_test",
                    unique_id_col = "unique_column_test",
                    hh_id_col = "hh_id_col_test",
                    id_type = "string",
                    proj_id = "proj_test",
                    form_id = "form_test"
                        )

    expected_result <- tibble::as_tibble(list(
        "id_unique"=c("a","b","c"),
        "id_hh"=c("50f0e25fb99189d87ff4d25b97f9347d", "362fe4f998ad34cce3a4bedea3228fef", "c522cd7db2c96587c2b9286f0b418736"),
        "id_rhomis_dataset"=c("8d1dd15d89377c8c0395d035630f04c6","b0c69e2a0ff049f17fb4d31a3eb0e56e","8d1dd15d89377c8c0395d035630f04c6"),
        "id_form"=c("form_test", "form_test", "form_test"),
        "id_proj"=c("proj_test", "proj_test", "proj_test"),
        "proj_id_column_test"=c("proj_1", "proj_2", "proj_1"),
        "form_id_column_test"=c("form_1", "form_1", "form_1"),
        "country_column_test"=c("country_x", "country_y", "country_x"),
        "unique_column_test"=c("a","b","c"),
        "hh_id_col_test"=c("a1","b1","t1"),
        "random_other_column"=c("srrfe","serasefdae","sresf")
    ))

    testthat::expect_equal(result, expected_result)


    result <- make_id_columns(data=data,
                              country_column = "country_column_test",
                              unique_id_col = "unique_column_test",
                              hh_id_col = "hh_id_col_test",
                              id_type = "column",
                              proj_id = "proj_id_column_test",
                              form_id = "form_id_column_test"
    )

    expected_result <- tibble::as_tibble(list(
        "id_unique"=c("a","b","c"),
        "id_hh"=c("50f0e25fb99189d87ff4d25b97f9347d", "362fe4f998ad34cce3a4bedea3228fef", "c522cd7db2c96587c2b9286f0b418736"),
        "id_rhomis_dataset"=c("111925a12bfd39fad140e104ae37da10","98b7efeacbd63306a37d0de4f84f4613","111925a12bfd39fad140e104ae37da10"),
        "id_form"=c("form_1", "form_1", "form_1"),
        "id_proj"=c("proj_1", "proj_2", "proj_1"),
        "proj_id_column_test"=c("proj_1", "proj_2", "proj_1"),
        "form_id_column_test"=c("form_1", "form_1", "form_1"),
        "country_column_test"=c("country_x", "country_y", "country_x"),
        "unique_column_test"=c("a","b","c"),
        "hh_id_col_test"=c("a1","b1","t1"),
        "random_other_column"=c("srrfe","serasefdae","sresf")
    ))

    testthat::expect_equal(result, expected_result)







})


testthat::test_that("Can process a whole CSV dataset",{

    test_data <- readr::read_csv("https://raw.githubusercontent.com/l-gorman/rhomis-R-package/dev/inst/sample_local_project/raw-data/raw-data.csv")
})


