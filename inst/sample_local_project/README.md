# Sample Project Processing

An example directory to show how a user would process a local RHoMIS dataset. 

RHoMIS processing occurs in 3 main stages:

1. The user extracts all of the new units, and free text entries, from the core data set. To do this, run the `01-extract-values-and-initial-cleaning.R` script.
2. The user will calculate initial indicators. These are all of the indicators which can be calculated without the user verifying prices or calorie conversion values. To do this run the `02-calculate-initial-indicators-and-prices.R` script.
3. The user finally will want to calculate indicators which draw upon price and calorie conversions. This can be done

If you would like to completely clear the directory and restart the processing, then run the `04-delete-outputs.R` script.
