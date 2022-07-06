

function_list <- list()


# Level 1
function_list <- add_function_to_list(  
    function_list=function_list,
    function_name="processData",
    called_by=NULL

)

# Level 2
function_list <- add_function_to_list(
    function_list=function_list,
    function_name="run_preliminary_calculations",
    called_by="processData"
)




# Level 3

function_list <- add_function_to_list(
    function_list=function_list,
    function_name="crop_calculations_all",
    called_by="run_preliminary_calculations"
)


function_stack <-get_function_stack(
    function_list=function_list,
    function_name = "crop_calculations_all"
)
toJSON(function_stack, pretty=T)

plot_function_dependency(
    function_list=function_list,
    function_name = "crop_calculations_all"
)




# Level 4
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




#
function_list <- add_function_to_list(  
    function_list=function_list,
    function_name=,
    called_by=""

)


