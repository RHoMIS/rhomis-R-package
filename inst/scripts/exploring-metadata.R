library(rhomis)
library(jsonlite)


indicator_list
names(indicator_list)

jsonlite::toJSON(indicator_list, pretty=T)



indicator_list["crop_sold_kg_per_year"]

jsonlite::toJSON(indicator_list["crop_sold_kg_per_year"], pretty=T)


find_nested_dependencies_list(
    indicator_name="land_cultivated_ha",
    indicator_list=indicator_list,
    dependency_required="loop")


find_nested_dependencies_list(
    indicator_name="crop_sold_kg_per_year",
    indicator_list=indicator_list,
    dependency_required="individual")


find_nested_dependencies_list(
    indicator_name="crop_sold_kg_per_year",
    indicator_list=indicator_list,
    dependency_required="loop")


find_nested_dependencies_list(
    indicator_name="crop_income_per_year",
    indicator_list=indicator_list,
    dependency_required="conversion")


plot_dependency_network(
    indicator_name="crop_income_per_year",
    indicator_list=indicator_list
)

plot_dependency_network(
    indicator_name="milk_collected_litres_per_year",
    indicator_list=indicator_list
)

plot_dependency_network(
    indicator_name="total_income_lcu_per_year",
    indicator_list=indicator_list
)



function_of_interest <- indicator_list$crop_sold_kg_per_year$function_calculated

plot_function_dependency(
    function_list,
    function_of_interest
    )

function_list$crop_sold_and_consumed_calculation
function_list$crop_calculations_all
function_list$run_preliminary_calculations

function_list$processData







plot_dependency_network(
    indicator_name="male_adult_control",
    indicator_list=indicator_list
)




