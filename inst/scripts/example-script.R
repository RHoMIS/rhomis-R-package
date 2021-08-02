library(rhomis)

# Example script for processing the test csv

root_path <- "inst/extdata/"
project_path <-"projects/VN_TST_2020/"

COUNTRY <- VN
PROJECT_CODE <- TST
YEAR <- 2020
ITERATION <- 1


rhomis_data <- read_csv(paste0(root_path,project_path,"data/raw_data.csv"), na=c("","NA","n/a"))

# Cleaning the column names into a more useable format
colnames(rhomis_data) <- clean_column_names(colnames(rhomis_data), seperator="/", repeat_columns=c("crop_repeat", "livestock_repeat", "offfarm_income_repeat", "hh_rep"))

# Manual intervention needed here to convert the values
# Write all of the core values to a file here
write_core_values_to_convert_to_file(rhomis_data, folder=paste0(root_path,project_path,"/cleaning"))

# A list of all of the
extract_units_data_frames(rhomis_data)


crop_name_column <- "crop_name"
crop_loop_columns <- c("crop_intercrop",
                       "crop_land_area",
                       "crop_harvest",
                       "crop_yield",
                       "crop_yield_units",
                       "crop_yield_units_other",
                       "crop_use",
                       "crop_consumed_prop",
                       "crop_sold_prop",
                       "crop_feed_lstk_prop",
                       "crop_seed_saved_prop",
                       "crop_sold_income",
                       "crop_sold_price_quantityunits",
                       "crop_price_quantityunits_other",
                       "crop_who_control_revenue",
                       "crop_consume_control",
                       "crop_residue_use"
                       )
crop_column_types <- c( "char",#"crop_intercrop",
            "chr",#"crop_land_area",
            "chr",#"crop_harvest",
            "num",#"crop_yield",
            "chr",#"crop_yield_units",
            "chr",#"crop_yield_units_other",
            "chr",#"crop_use",
            "chr",#"crop_consumed_prop",
            "chr",#"crop_sold_prop",
            "chr",#"crop_feed_lstk_prop",
            "chr",#"crop_seed_saved_prop",
            "num",#"crop_sold_income",
            "chr",#"crop_sold_price_quantityunits",
            "chr",#"crop_price_quantityunits_other",
            "chr",#"crop_who_control_revenue",
            "chr",#"crop_consume_control",
            "chr"#"crop_residue_use"
           )
crop_details_wide <- map_to_wide_format(rhomis_data,
                                        crop_name_column,
                                        crop_loop_columns,
                                        crop_column_types)

livestock_name_column <- "livestock_name"
livestock_loop_columns <- c("livestock_breeds",
                            #"livestock_count",
                            "livestock_housing",
                            "livestock_bought",
                            "livestock_sold",
                            "livestock_sale_income",
                            "livestock_ownership", # A control column
                            "livestock_who_sells", # A control column
                            "livestock_died",
                            #"mortality_meat_use",
                            "killed_for_meat",
                            "meat_use",
                            "meat_consumed_amount",
                            "meat_sell_amount",
                            "meat_sold_income",
                            "livestock_meat_who_sells",
                            "livestock_meat_who_control_eating",
                            "milk_harvest",
                            "milk_amount_good_season",
                            "milk_units",
                            "milk_amount_bad_season",
                            "milk_number_animals_milked",
                            "milk_amount_units_other",
                            "milk_use",
                            "milk_consumed_amount",
                            "milk_sell_amount",
                            "milk_sold_income",
                            "milk_sold_price_timeunits",
                            "milk_amount_time_units_other",
                            "milk_who_sells",
                            "milk_who_control_eating",
                            "eggs_harvest",
                            "eggs_amount_good",
                            "eggs_units",
                            "eggs_amount_bad",
                            "eggs_amount_units_other",
                            "eggs_use",
                            "eggs_consumed_amount",
                            "eggs_sell_amount",
                            "eggs_sold_income",
                            "eggs_sold_price_timeunits",
                            "eggs_sold_price_timeunits_other",
                            "eggs_who_sells",
                            "eggs_who_control_eating",
                            "bees_honey_production",
                            "bees_honey_production_units",
                            "bees_honey_use",
                            "bees_honey_production_units_other",
                            "bees_honey_consumed_amount",
                            "bees_honey_sell_amount",
                            "bees_honey_sold_income",
                            "bees_who_sells",
                            "bees_who_control_eating")

livestock_column_types <- c("chr",#"livestock_breeds",
                            #"",#"livestock_count",
                            "chr",#"livestock_housing",
                            "num",#"livestock_bought",
                            "num",#"livestock_sold",
                            "num",#"livestock_sale_income",
                            "chr",#"livestock_ownership", # A control column
                            "chr",#"livestock_who_sells", # A control column
                            "num",#"livestock_died",
                            #"chr",#"mortality_meat_use",
                            "num",#"killed_for_meat",
                            "chr",#"meat_use",
                            "chr",#"meat_consumed_amount",
                            "chr",#"meat_sell_amount",
                            "num",#"meat_sold_income",
                            "chr",#"livestock_meat_who_sells",
                            "chr",#"livestock_meat_who_control_eating",
                            "chr",#"milk_harvest",
                            "num",#"milk_amount_good_season",
                            "chr",#"milk_units",
                            "num",#"milk_amount_bad_season",
                            "num",#"milk_number_animals_milked",
                            "chr",#"milk_amount_units_other",
                            "chr",#"milk_use",
                            "chr",#"milk_consumed_amount",
                            "chr",#"milk_sell_amount",
                            "num",#"milk_sold_income",
                            "chr",#"milk_sold_price_timeunits",
                            "chr",#"milk_amount_time_units_other",
                            "chr",#"milk_who_sells",
                            "chr",#"milk_who_control_eating",
                            "chr",#"eggs_harvest",
                            "num",#"eggs_amount_good",
                            "chr",#"eggs_units",
                            "num",#"eggs_amount_bad",
                            "chr",#"eggs_amount_units_other",
                            "chr",#"eggs_use",
                            "chr",#"eggs_consumed_amount",
                            "chr",#"eggs_sell_amount",
                            "num",#"eggs_sold_income",
                            "chr",#"eggs_sold_price_timeunits",
                            "chr",#"eggs_sold_price_timeunits_other",
                            "chr",#"eggs_who_sells",
                            "chr",#"eggs_who_control_eating",
                            "num",#"bees_honey_production",
                            "chr",#"bees_honey_production_units",
                            "chr",#"bees_honey_use",
                            "chr",#"bees_honey_production_units_other",
                            "chr",#"bees_honey_consumed_amount",
                            "chr",#"bees_honey_sell_amount",
                            "num",#"bees_honey_sold_income",
                            "chr",#"bees_who_sells",
                            "chr")#"bees_who_control_eating")

livestock_details_wide <- map_to_wide_format(rhomis_data,
                                             livestock_name_column,
                                             livestock_loop_columns,
                                             livestock_column_types)


off_farm_name_column <- "offfarm_income_name"
off_farm_loop_columns <- c("offfarm_year_round",
                           "offfarm_month",
                           "offfarm_who_control_revenue")
off_farm_column_types <- c("chr",#"offfarm_income_year_round",
                           "chr",#"offfarm_month",
                           "chr")#"offfarm_who_control_revenue")

off_farm_details_wide <- map_to_wide_format(rhomis_data,
                                             off_farm_name_column,
                                             off_farm_loop_columns,
                                             off_farm_column_types)

#' Have also written functions to further reformat these based on gender,
#' need to implement them in this example-script



# Connecting to a MongoDB

# Will only work if you creat a rhomis database called "units
mongodb <- connect_to_db(database="rhomis", url="mongodb://localhost")




