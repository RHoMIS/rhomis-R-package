test_that("Can create indicators", {

    indicator_list <- list()

    indicator_list <- add_indicator(
        indicator_list,
        indicator_name = "iso_country_code",
        output_format="column",
        description="random description",

        individual_columns_required=list("country"),
        loop_columns_required=list(),
        conversion_tables_required=list("country_name_conversions"),
        api_data_required = list(),
        indicators_required=list(),
        function_calculated="run_preliminary_calculations",
        search_term="indicator_search_id_rhomis_dataset"
    )

    indicator_list <- add_indicator(
        indicator_list,
        indicator_name = "year",
        output_format="column",
        description="random description",
        individual_columns_required=list("year", "start_time_user"),
        loop_columns_required=list(),
        conversion_tables_required=list(),
        api_data_required = list(),
        indicators_required=list(),
        function_calculated="run_preliminary_calculations",
        search_term="indicator_search_year")


    indicator_list <- add_indicator(
        indicator_list,
        indicator_name = "currency_conversion_lcu_to_ppp",

        output_format="column",
        description="random description",

        individual_columns_required=list(),
        loop_columns_required=list(),
        conversion_tables_required=list(),
        api_data_required = list(),
        indicators_required=list("year","iso_country_code"),
        function_calculated=list("run_preliminary_calculations"),
        search_term="indicator_search_currency_conversion_lcu_to_ppp")


    expected_output <- list(
        iso_country_code = list(
            indicator_name = "iso_country_code",
            output_format = "column",
            description="random description",
            individual_columns_required = "country",
            loop_columns_required = list(),
            conversion_tables_required = "country_name_conversions",
            api_data_required = list(),
            indicators_required = list(),
            function_calculated = "run_preliminary_calculations",
            search_term = "indicator_search_id_rhomis_dataset"),
        year = list(
            indicator_name = "year",
            output_format = "column",
            description="random description",

            individual_columns_required = c("year", "start_time_user"),
            loop_columns_required = list(),
            conversion_tables_required = list(),
            api_data_required = list(),
            indicators_required = list(),
            function_calculated = "run_preliminary_calculations",
            search_term = "indicator_search_year"),
        currency_conversion_lcu_to_ppp = list(
            indicator_name = "currency_conversion_lcu_to_ppp",
            output_format = "column",
            description="random description",

            individual_columns_required = list(),
            loop_columns_required = list(),
            conversion_tables_required = list(),
            api_data_required = list(),
            indicators_required = c("year", "iso_country_code"),
            function_calculated = "run_preliminary_calculations",
            search_term = "indicator_search_currency_conversion_lcu_to_ppp"))

    expect_equal(indicator_list, expected_output)
})


test_that("Can fetch list of dependencies", {

    indicator_list <- list()

    indicator_list <- add_indicator(
        indicator_list,
        indicator_name = "iso_country_code",
        output_format="column",
        description="random description",

        individual_columns_required=list("country"),
        loop_columns_required=list(),
        conversion_tables_required=list("country_name_conversions"),
        api_data_required = list(),
        indicators_required=list(),
        function_calculated="run_preliminary_calculations",
        search_term="indicator_search_id_rhomis_dataset"
    )

    indicator_list <- add_indicator(
        indicator_list,
        indicator_name = "year",
        output_format="column",
        description="random description",

        individual_columns_required=list("year", "start_time_user"),
        loop_columns_required=list(),
        conversion_tables_required=list(),
        api_data_required = list(),
        indicators_required=list(),
        function_calculated="run_preliminary_calculations",
        search_term="indicator_search_year")


    indicator_list <- add_indicator(
        indicator_list,
        indicator_name = "currency_conversion_lcu_to_ppp",
        output_format="column",
        description="random description",

        individual_columns_required=list(),
        loop_columns_required=list(),
        conversion_tables_required=list(),
        api_data_required = list(),
        indicators_required=list("year","iso_country_code"),
        function_calculated=list("run_preliminary_calculations"),
        search_term="indicator_search_currency_conversion_lcu_to_ppp")

    dependencies <- find_nested_dependencies_list(
        indicator_name="currency_conversion_lcu_to_ppp",
        indicator_list=indicator_list,
        dependency_required="individual")

    expect_equal(dependencies, c("year", "start_time_user", "country"))

    dependencies <- find_nested_dependencies_list(
        indicator_name="currency_conversion_lcu_to_ppp",
        indicator_list=indicator_list,
        dependency_required="loop")

    expect_equal(dependencies, NULL)
})
