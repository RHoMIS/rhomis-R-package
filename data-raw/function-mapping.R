




function_list <- list()

# function_stack <-get_function_stack(
#     function_list=function_list,
#     function_name = "crop_calculations_all"
# )


# toJSON(function_stack, pretty=T)

# plot_function_dependency(
#     function_list=function_list,
#     function_name = "crop_calculations_all"
# )



#---------------------------------------------------------
# Top Level Function
#---------------------------------------------------------

function_list <- add_function_to_list(
    function_list=function_list,
    function_name="processData",
    called_by=NULL

)

#---------------------------------------------------------
# Functions_called by processData
#---------------------------------------------------------

function_list <- add_function_to_list(
    function_list=function_list,
    function_name="run_preliminary_calculations",
    called_by="processData"
)


function_list <- add_function_to_list(
    function_list=function_list,
    function_name="value_gender_fa_calculations",
    called_by="processData"
)

#---------------------------------------------------------
# Functions_called by value_gender_fa_calculations
#---------------------------------------------------------


function_list <- add_function_to_list(
    function_list=function_list,
    function_name="value_calculations",
    called_by="value_gender_fa_calculations"
)

function_list <- add_function_to_list(
    function_list=function_list,
    function_name="calorie_calculations",
    called_by="value_gender_fa_calculations"
)

function_list <- add_function_to_list(
    function_list=function_list,
    function_name="gender_control_summary",
    called_by="value_gender_fa_calculations"
)


#---------------------------------------------------------
# Functions_called by calorie_calculations
#---------------------------------------------------------



#---------------------------------------------------------
# Functions_called by run_preliminary_calculations
#---------------------------------------------------------

function_list <- add_function_to_list(
    function_list=function_list,
    function_name="crop_calculations_all",
    called_by="run_preliminary_calculations"
)

function_list <- add_function_to_list(
    function_list=function_list,
    function_name="livestock_calculations_all",
    called_by="run_preliminary_calculations"
)

function_list <- add_function_to_list(
    function_list=function_list,
    function_name="livestock_tlu_calculations",
    called_by="run_preliminary_calculations"
)

function_list <- add_function_to_list(
    function_list=function_list,
    function_name="clean_tlu_column_names",
    called_by="run_preliminary_calculations"
)

function_list <- add_function_to_list(
    function_list=function_list,
    function_name="land_size_calculation",
    called_by="run_preliminary_calculations"
)

function_list <- add_function_to_list(
    function_list=function_list,
    function_name="calculate_household_size_members",
    called_by="run_preliminary_calculations"
)

function_list <- add_function_to_list(
    function_list=function_list,
    function_name="food_security_calculations",
    called_by="run_preliminary_calculations"
)


function_list <- add_function_to_list(
    function_list=function_list,
    function_name="hdds_calc",
    called_by="run_preliminary_calculations"
)


function_list <- add_function_to_list(
    function_list=function_list,
    function_name="total_crop_income",
    called_by="run_preliminary_calculations"
)

function_list <- add_function_to_list(
    function_list=function_list,
    function_name="total_livestock_income",
    called_by="run_preliminary_calculations"
)

function_list <- add_function_to_list(
    function_list=function_list,
    function_name="total_and_off_farm_income",
    called_by="run_preliminary_calculations"
)



#---------------------------------------------------------
# Functions called by crop_calculations_all
#---------------------------------------------------------

function_list <- add_function_to_list(
    function_list=function_list,
    function_name="crop_harvest_calculation",
    called_by="crop_calculations_all"
)

function_list <- add_function_to_list(
    function_list=function_list,
    function_name="crop_sold_and_consumed_calculation",
    called_by="crop_calculations_all"

)

function_list <- add_function_to_list(
    function_list=function_list,
    function_name="crop_income_calculations",
    called_by="crop_calculations_all"
)


function_list <- add_function_to_list(
    function_list=function_list,
    function_name="crop_gender_calculations",
    called_by="crop_calculations_all"
)



#---------------------------------------------------------
# Functions called by livestock_calculations_all
#---------------------------------------------------------

function_list <- add_function_to_list(
    function_list=function_list,
    function_name="price_per_livestock",
    called_by="livestock_calculations_all"
)

function_list <- add_function_to_list(
    function_list=function_list,
    function_name="meat_amount_calculation",
    called_by="livestock_calculations_all"
)

function_list <- add_function_to_list(
    function_list=function_list,
    function_name="meat_sold_and_consumed_calculation",
    called_by="livestock_calculations_all"
)

function_list <- add_function_to_list(
    function_list=function_list,
    function_name="meat_prices",
    called_by="livestock_calculations_all"
)

function_list <- add_function_to_list(
    function_list=function_list,
    function_name="milk_amount_calculations",
    called_by="livestock_calculations_all"
)

function_list <- add_function_to_list(
    function_list=function_list,
    function_name="milk_sold_and_consumed_calculations",
    called_by="livestock_calculations_all"
)


function_list <- add_function_to_list(
    function_list=function_list,
    function_name="milk_income_calculations",
    called_by="livestock_calculations_all"
)

function_list <- add_function_to_list(
    function_list=function_list,
    function_name="eggs_amount_calculations",
    called_by="livestock_calculations_all"
)

function_list <- add_function_to_list(
    function_list=function_list,
    function_name="eggs_sold_and_consumed_calculations",
    called_by="livestock_calculations_all"
)



function_list <- add_function_to_list(
    function_list=function_list,
    function_name="egg_income_calculations",
    called_by="livestock_calculations_all"
)

function_list <- add_function_to_list(
    function_list=function_list,
    function_name="honey_amount_sold_and_consumed_calculations",
    called_by="livestock_calculations_all"
)

function_list <- add_function_to_list(
    function_list=function_list,
    function_name="honey_income_calculations",
    called_by="livestock_calculations_all"
)









usethis::use_data(function_list, overwrite = T)
