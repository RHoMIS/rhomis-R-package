# RHoMIS R-package for data processing
<!-- badges: start -->
[![R-CMD-check](https://github.com/l-gorman/rhomis-R-package/workflows/R-CMD-check/badge.svg)](https://github.com/l-gorman/rhomis-R-package/actions)
<!-- badges: end -->


Initial package for RHoMIS 2.0. Main steps should be:
* Data is downloaded from the RHoMIS ODK central server. 
* Data is cleaned: column names are simplified, new units extracted, data types checked.
*  Data is reformatted, and processed to generate key indicators. 
*  A series of standard plots and outputs are generated.

# Useful Commands when building the package
* Install Package: `Ctrl + Shift + B`
* Check Package:   `Ctrl + Shift + E`
* Test Package:    `Ctrl + Shift + T`

Run the command `devtools::test()` in the console to run all tests. `devtools::document()` to rebuild documentation. `devtools::check()` will run a CRAN check on the package, this is the same check that is implemented on `git push` and for any pull requests.

An anonymised sample RHoMIS dataset (in its raw form) has been included in the folder `inst/extdata`. A sample script demonstrating how the package can be used has been included in the folder `inst/scripts`.
