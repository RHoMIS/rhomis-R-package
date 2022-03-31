#' File to write a mock central
#' response zip file, which is then read
#' when the ODK test endpoint is hit.


testing_dir <- getwd()

if (grepl("tests/testthat", testing_dir)) {
    test_file_origin <- "./sample-central-project/sample-central-data.csv.zip"
    test_file_destination <- "./test-central-url.com/v1/projects/test-project/forms/test-user/temp_submissions.zip"
}

if (grepl("tests/testthat", testing_dir) == F) {
    test_file_origin <- "./tests/testthat/sample-central-project/sample-central-data.csv.zip"
    test_file_destination <- "./tests/testthat/test-central-url.com/v1/projects/test-project/forms/test-user/temp_submissions.zip"
}

# Copying file from inst directory into the endpoint for testing.
file.copy(
    test_file_origin,
    test_file_destination
)


# Adding time for file to write before sending response
Sys.sleep(1)

# Sending a fake response as httr expects
structure(list(
    url = "test-central-url.com/v1/projects/test-project/forms/test-user/submissions.csv.zip",
    status_code = 200L
))