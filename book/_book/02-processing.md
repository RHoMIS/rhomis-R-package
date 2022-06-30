# Data Processing

## Setup

Firstly, you will want to make sure that you have the most recent
version of the RHoMIS R-package installed, along with the devtools 
library:


```r
install.packages("devtools")
devtools::install_github("https://github.com/l-gorman/rhomis-R-package", force = TRUE)
```

You should then ensure you have a folder containing the data
you would like to process. If using Rstudio interactively, set
your working directory to this folder using the `setwd()` command.
Your directory structure should look something like this:

```
📂 project
   ┗📂 raw-data
     ┗📜 sample_project.csv
```

In this case, your working directory should be the `project` folder.

## Dealing with Units

### Extracting Units

To extract new units from the data you are working with, run the
code below. This code can also be found in the `01-extract-values-and-initial-cleaning.R` file, included 
alongside this Rmarkdown document.


```r
library(rhomis)

new_units <- processData(
   extractUnitsOnly = T, # The stage of data processing

   # Arguments to indicate the type of processing being done (local or on server)
   dataSource = "csv",
   outputType = "csv",

   # Arguments used for processing local data sets
   dataFilePath = "raw-data/sample_project.csv",
   id_type = "string",
   proj_id = "my_project_id",
   form_id = "my_form_id"
)
```

If you now go back to your `project` directory, you should be able
to see that more files have been added:

```
📂 project
   ┣📂 raw-data
   ┃ ┗📜 sample_project.csv
   ┣📂 original_units
   ┃ ┣📜 crop_yield_units.csv
   ┃ ┣📜 country.csv
   ┃ ┗📜 ...
   ┗📂 converted_units
     ┣📜 country.csv
     ┗📜 ...

```

The `original_units` folder includes the units directly calculated from the data.
The `converted_units` folder should will include the units which you will
verify and change.

### Converting Units

__Please note, while you do not _have_ to convert units for the next step in
the calculations to run, uncoverted units will be ignored and will lead to 
unnecessary missing values__

In the `project/converted_units` directory, you will find a series of csv files 
with conversion factors that need to be filled in. Each file will look 
like the table below. The first column indicates a value which was 
found in the survey. The second column shows the conversion factor 
which ought to be used. Where the conversion factor is unknown, the column
will display `NA`. Where the conversion factor is known it will be provided.
Known conversion factors are already embedded in the R-package.


|id_rhomis_dataset |survey_value |conversion |
|:-----------------|:------------|:----------|
|x                 |foo          |NA         |
|x                 |bar          |NA         |

There are a range of conversions to enter and it is important to be aware
which units are needed. Each file is described in more detail below:


| File | Description | Example Survey Value | Example Conversion |
| :--- | :------- | :------- | :------- |
| bees_honey_production_units.csv  | The conversion factor should be used to convert to kilograms/litres. | buckets_12_litre | 12 |
| country.csv | The conversion factor should be used to convert country names to [two-letter ISO country codes](https://www.iban.com/country-codes).  | uganda | UG |
| crop_name.csv | The name of crops entered in the survey. Often enumerators may specify "other" crops in a free-text-entry field. Sometimes these crops can be mispelt, or in a language other than English. Here we correct mispellings and translations into standard forms (__all lower case__)  |  maze | maize  |
| crop_sold_price_quantityunits.csv  |  The price units for crops which were sold. Please note that only one unit will be converted into a string, "total_income_per_year" will be converted to "total_income_per_year" as this is treated as a special case in the analysis scripts. | price_per_50kg_sack | 0.02 |
| crop_yield_units.csv  |  The unit of crops which have been collected. This needs to be converted to kilograms | cart_250kg  | 250 |
| eggs_sold_price_timeunits.csv  | The amount of money made per unit time for selling eggs. This needs to be converted into an income per year  | income for 3 months | 4 |
| eggs_units.csv  | The number of eggs collected per year | pieces/day | 365 |
| fertiliser_units.csv |  The amount of fertiliser in kg |  sacks_25kg | 25  |
| livestock_name.csv  | The names of livestock entered in the survey. As with crop names, enumerators can also enter extra livestock names which are in a different language or mispelt. This conversion is used to standardise livestock names entered in the survey | catel | cattle |
| milk_sold_price_timeunits.csv  |  The amount of money made per unit volume or per unit time. For unit time, the options are "day", "week", "month", "year". These time units must be entered as strings. For the unit volume, numeric conversions must be entered| month | month |
|      |       |  0.3litre | 0.3  |
| milk_units.csv  | The amount of milk  collected in litres per year. There are a number of exceptions which must be kept as text strings, and are dealt with in the analysis scripts (e.g. "per animal per day" and "l/animal/day" |  l/day  | 365 |
| unitland.csv  | The amount of land in hectares |  acres  | 0.4  |


## Processing the Data (Part 1)

As with unit extraction, ensure you are in the project directory.
Then enter the commands below (or run the `02-calculate-initial-indicators.R` file):


```r
library(rhomis)

new_units <- processData(
   extractUnitsOnly = F, # The stage of data processing
   calculateInitialIndicatorsOnly = T, # The stage of data processing

   # Arguments to indicate the type of processing being done (local or on server)
   dataSource = "csv",
   outputType = "csv",

   # Arguments used for processing local data sets
   dataFilePath = "raw-data/sample_project.csv",
   id_type = "string",
   proj_id = "my_project_id",
   form_id = "my_form_id"
)
```

You will now see that the directory structure again looks quite different:

```
📂 project
   ┣📂 raw-data
   ┃ ┗📜 sample_project.csv
   ┣📂 processed_data
   ┃ ┗📜 processed_data.csv
   ┣📂 original_units
   ┃ ┣📜 bees_honey_production_units.csv
   ┃ ┣📜 crop_name.csv
   ┃ ┗📜 ...
   ┗📂 converted_units
   ┃ ┣📜 country.csv
   ┃ ┗📜 ...
   ┣📂 original_calorie_conversions
   ┃ ┣📜 crop_calories.csv
   ┃ ┣📜 eggs_calories.csv
   ┃ ┗📜 ...
   ┗📂 completed_calorie_conversions
   ┃ ┣📜 crop_calories.csv
   ┃ ┗📜 ...
   ┣📂 original_prices
   ┃ ┣📜 crop_calories.csv
   ┃ ┣📜 eggs_calories.csv
   ┃ ┗📜 ...
   ┗📂 converted_prices
   ┃ ┣📜 crop_calories.csv
   ┃ ┗📜 ...
   ┣📂 crop_data
   ┃ ┣📜 crop_consumed_kg_per_year.csv
   ┃ ┣📜 crop_harvest_kg_per_year.csv
   ┃ ┗📜 ...
   ┣📂 indicator_data
   ┃ ┗📜 indicators.csv
   ┣📂 livestock_data
   ┃ ┣📜 crop_consumed_kg_per_year.csv
   ┃ ┣📜 crop_consumed_kg_per_year.csv
   ┃ ┗📜 ...
   ┣📂 original_prices
   ┃ ┣📜 crop_prices.csv
   ┃ ┣📜 livestock_price_per_animal.csv
   ┃ ┗📜 ...
   ┣📂 off_farm_data
   ┃ ┣📜 offfarm_income_name.csv
   ┃ ┣📜 offfarm_who_control_revenue.csv
   ┃ ┗📜 ...
   ┗📂 processed_data
     ┗📜 processed_data.csv
   
   
```

### Converting Units

As with the units which were extracted, you will also need to verify
prices (in the `converted_prices` folder) and calorie values (in the
`completed_calorie_conversions` folder). Prices will be in 
lcu/kg for crops, meat, and eggs (where lcu is local currency units).
Prices will be in lcu/l for milk and honey. And prices will be in 
lcu/animal for whole livestock sales.

For calorie values, conversions will be in kcal/kg or kcal/l.

## Processing the Data (Part 2)

Using the calories and prices you have converted, you will then be able to produce
the final indicators. Either run the code below, or run the `03-calculate-final-indicators.R`
file. 


```r
library(rhomis)

new_units <- processData(
   extractUnitsOnly = F, # The stage of data processing
   calculateFinalIndicatorsOnly = T, # The stage of data processing

   # Arguments to indicate the type of processing being done (local or on server)
   dataSource = "csv",
   outputType = "csv",

   # Arguments used for processing local data sets
   dataFilePath = "raw-data/sample_project.csv",
   id_type = "string",
   proj_id = "my_project_id",
   form_id = "my_form_id"
)
```

You will now see the directory includes some
extra folders:

You will now see that the directory structure again looks quite different:

```
📂 project
   ┣📂 raw-data
   ┃ ┗📜 sample_project.csv
   ┣📂 processed_data
   ┃ ┗📜 processed_data.csv
   ┣📂 original_units
   ┃ ┣📜 bees_honey_production_units.csv
   ┃ ┣📜 crop_name.csv
   ┃ ┗📜 ...
   ┗📂 converted_units
   ┃ ┣📜 country.csv
   ┃ ┗📜 ...
   ┣📂 original_calorie_conversions
   ┃ ┣📜 crop_calories.csv
   ┃ ┣📜 eggs_calories.csv
   ┃ ┗📜 ...
   ┗📂 completed_calorie_conversions
   ┃ ┣📜 crop_calories.csv
   ┃ ┗📜 ...
   ┣📂 original_prices
   ┃ ┣📜 crop_calories.csv
   ┃ ┣📜 eggs_calories.csv
   ┃ ┗📜 ...
   ┗📂 converted_prices
   ┃ ┣📜 crop_calories.csv
   ┃ ┗📜 ...
   ┣📂 crop_data
   ┃ ┣📜 crop_consumed_kg_per_year.csv
   ┃ ┣📜 crop_harvest_kg_per_year.csv
   ┃ ┗📜 ...
   ┣📂 indicator_data
   ┃ ┗📜 indicators.csv
   ┣📂 livestock_data
   ┃ ┣📜 crop_consumed_kg_per_year.csv
   ┃ ┣📜 crop_consumed_kg_per_year.csv
   ┃ ┗📜 ...
   ┣📂 original_prices
   ┃ ┣📜 crop_prices.csv
   ┃ ┣📜 livestock_price_per_animal.csv
   ┃ ┗📜 ...
   ┣📂 off_farm_data
   ┃ ┣📜 offfarm_income_name.csv
   ┃ ┣📜 offfarm_who_control_revenue.csv
   ┃ ┗📜 ...
   ┗📂 processed_data
   ┃  ┗📜 processed_data.csv
   ┣📂 consumption_calorie_values
   ┃ ┣📜 crop_calories_consumed_kcal.csv
   ┃ ┣📜 milk_calories_consumed_kcal.csv
   ┃ ┗📜 ...
   ┣📂 consumption_lcu_values
   ┃ ┣📜 value_crop_consumed_lcu.csv
   ┃ ┣📜 value_eggs_consumed_lcu.csv
   ┃ ┗📜 ...  
   ┗📂 gender_control
     ┣📂 gender_control_amounts_consumed
     ┣📂 gender_control_income
     ┗📂 ... 
```


## Outputs

### Indicators

Indicators calculated whilst the data was being processed. These cover a range of
topics, including incomes, dietary diversity, and household demographics. The indicators
calculated depend on the columns present in the original `raw-data.csv` file.

### Crop Data

Found in the `crop_data` directory, in each of the csvs, a column represents the crops
listed in the survey, each row represents one household (in the same row order as the original dataset)

### Livestock Data

Found in the `livestock_data` directory, in each of the csvs, a column represents the types of livestock listed in the survey,
again each row represents a household.

### Off-farm Data

Found in the `off_farm_data` directory, in each of the csvs, a column represents an off-farm activity, and each row represents 
a household.

### Consumption Calories

Found in the `consumption_calorie_values` directory, in each of the csvs, a column represents the products listed in the survey, and each row represents 
a household.

### Consumption value

Found in the `consumption_lcu_values` directory, in each of the csvs, a column represents products listed in the survey, and each row represents 
a household.

### Gender Control

Found in the `gender_control` directory, in each of the csvs, a column represents products listed in the survey, and each row represents 
a household.


