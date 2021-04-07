library(testthat)

test_that("Raw column names successfully shorten", {
    long_name <- "xxxx/xxxxx/xxxxx/important_name"
    seperator <- "/"
    shortened_name <- shorten_individual_column_name(column_name=long_name, seperator)
    expect_equal(shortened_name, "important_name")
})

test_that("Multiple column names switched properly",{
    long_names <- c("xxxx/xxxxx/xxxxx/important_name_1",
                    "xxxx/xxxxx/xxxxx/important_name_2",
                    "xxxx/xxxxx/xxxxx/important_name_3")
    seperator <- "/"
    split_list <- unlist(lapply(long_names,function(name) shorten_individual_column_name(name,seperator)))
    result_equals_expectation <- all(split_list==c("important_name_1","important_name_2","important_name_3"))
    expect_equal(result_equals_expectation,TRUE)
})
