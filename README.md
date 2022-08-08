# RHoMIS R-package for Data Processing
<!-- ALL-CONTRIBUTORS-BADGE:START - Do not remove or modify this section -->
[![All Contributors](https://img.shields.io/badge/all_contributors-2-orange.svg?style=flat-square)](#contributors-)
<!-- ALL-CONTRIBUTORS-BADGE:END -->
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



## Contributors ✨

Thanks goes to these wonderful people ([emoji key](https://allcontributors.org/docs/en/emoji-key)):

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->
<table>
  <tr>
    <td align="center"><a href="https://github.com/rfrelat"><img src="https://avatars.githubusercontent.com/u/15328277?v=4?s=100" width="100px;" alt=""/><br /><sub><b>Romain Frelat</b></sub></a><br /><a href="https://github.com/l-gorman/rhomis-R-package/issues?q=author%3Arfrelat" title="Bug reports">🐛</a></td>
    <td align="center"><a href="https://github.com/JimHam"><img src="https://avatars.githubusercontent.com/u/8358392?v=4?s=100" width="100px;" alt=""/><br /><sub><b>JimHam</b></sub></a><br /><a href="#projectManagement-JimHam" title="Project Management">📆</a> <a href="#mentoring-JimHam" title="Mentoring">🧑‍🏫</a> <a href="https://github.com/l-gorman/rhomis-R-package/issues?q=author%3AJimHam" title="Bug reports">🐛</a> <a href="#ideas-JimHam" title="Ideas, Planning, & Feedback">🤔</a></td>
  </tr>
</table>

<!-- markdownlint-restore -->
<!-- prettier-ignore-end -->

<!-- ALL-CONTRIBUTORS-LIST:END -->

This project follows the [all-contributors](https://github.com/all-contributors/all-contributors) specification. Contributions of any kind welcome!