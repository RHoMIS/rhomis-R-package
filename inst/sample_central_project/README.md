# Sample Project Processing

An example directory to show how a user would process a dataset stored on ODK central,
and how these datasets would be stored

RHoMIS processing occurs in 3 main stages:

1. The user extracts all of the new units, and free text entries, from the core data set. To do this, run the `01-extract-values-and-initial-cleaning.R` script.
2. The user will calculate initial indicators. These are all of the indicators which can be calculated without the user verifying prices or calorie conversion values. To do this run the `02-calculate-initial-indicators-and-prices.R` script.
3. The user finally will want to calculate indicators which draw upon price and calorie conversions. This can be done

If you would like to completely clear the directory and restart the processing, then run the `04-delete-outputs.R` script.

## Dependencies

You will need to have MongoDB installed in order for this to run, instructions to install can be found [here](https://docs.mongodb.com/manual/administration/install-community/).

## Querying MongoDB 

If you would like to see whether the data has been stored in your local
mongoDB instance follow these commands. First start the mongo shell console:

`mongosh`

To view which databases are available, execute the command 

`show databases`

After running the `01-extract-values-and-initial-cleaning.R` script, you should see a database name `rhomis-test`. To examine the rhomis-database, you will need to execute the command:

`use rhomis-test`

To see the available "collections", execute the command:

`show collections`

To see all of the data inside a collection, you will need to include the collection name. For example, to show the data stored in the `units_and_conversions` collection, execute the command:

`db.units_and_conversions.find({})`

For further guidance on executing mongoDB queries, please see their [documentaion](https://docs.mongodb.com/mongodb-shell/run-commands/)
