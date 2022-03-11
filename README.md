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

Ensure that you have devtools installed and loaded. To install the RHoMIS package you can use the command:

`devtools::install_github("git@github.com:l-gorman/rhomis-R-package.git")`

To get started, there is a sample script which shows how data is processed end-to-end in the `inst/scripts/rhomis_process_pipeline.R` file. I will be working vignettes to demonstrate how to use the package shortly.


## Useful Commands when building the package
* Install Package: `Ctrl + Shift + B`
* Check Package:   `Ctrl + Shift + E`
* Test Package:    `Ctrl + Shift + T`

Run the command `devtools::test()` in the console to run all tests. `devtools::document()` to rebuild documentation. `devtools::check()` will run a CRAN check on the package, this is the same check that is implemented on `git push` and for any pull requests.

An anonymised sample RHoMIS dataset (in its raw form) has been included in the folder `inst/extdata`. A sample script demonstrating how the package can be used has been included in the folder `inst/scripts`.
