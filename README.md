# RHoMIS R-package for Data Processing
<!-- badges: start -->
[![R-CMD-check](https://github.com/l-gorman/rhomis-R-package/workflows/R-CMD-check/badge.svg)](https://github.com/l-gorman/rhomis-R-package/actions)
<!-- badges: end -->

## Summary

This R-package is designed to help users process RHoMIS datasets.
It is also being used to process data on the RHoMIS 2.0 [server](https://github.com/l-gorman/rhomis-api), using the scripts found [here](https://github.com/l-gorman/rhomis-)
Initial package for RHoMIS 2.0. The package can:
* Import data from ODK central
* Clean raw data from ODK central
* Extract new crop values and units from the core RHoMIS survey
* Calculate key RHoMIS indicators
* Load data into a MongoDB database

## ToDos

See the [issues](https://github.com/l-gorman/rhomis-R-package/issues) page for notes on what still needs to be done. Please feel free to add an issue for any new functionality you would like to see!

## Installation

Ensure that you have devtools installed and loaded. To sinstall the RHoMIS package you can use the command:

`devtools::install_github("git@github.com:l-gorman/rhomis-R-package.git")`

To get started, there is a sample script which shows how data is processed end-to-end in the `inst/scripts/rhomis_process_pipeline.R` file. I will be working vignettes to demonstrate how to use the package shortly.

If building the package for development, you can install via the command line using the command, if you are working from the `rhomis-R-package` directory:

`R CMD INSTALL --no-multiarch --with-keep.source ./`



## Getting Started with Development

The two major uses of this package are: developing RHoMIS datasets locally; developing RHoMIS datasets stored on an ODK central server. There are two pre-prepared examples for these major use cases.

To process a sample dataset locally, see the example found in the `inst/sample_local_project/` directory. To process a dataset as if it were stored on an ODK central server, see the example found in the `inst/sample_central_project` directory. Both of these directories contain their own README to help get started.

Run the command `devtools::test()` in the console to run all tests. `devtools::document()` to rebuild documentation. `devtools::check()` will run a CRAN check on the package, this is the same check that is implemented on `git push` and for any pull requests.


