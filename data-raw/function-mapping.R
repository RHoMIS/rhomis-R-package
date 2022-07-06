




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


#---------------------------------------------------------
# Functions called by crop_calculations_all
#---------------------------------------------------------

function_list <- add_function_to_list(  
    function_list=function_list,
    function_name="crop_harvest_calculationss",
    called_by="crop_calculations_all"
)

function_list <- add_function_to_list(  
    function_list=function_list,
    function_name="crop_sold_and_consumed_calculations",
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
