
fp_products <- list(

    fruit=list(
        base_name="fruit",
        fp_name = "fp_name",

        #Info needed for fruit calculations
        amount = "fruit_amount",
        amount_units = "fruit_amount_units",
        amount_units_other = "fruit_amount_units_other",

        # Info needed for proportions calculations
        use_column = "fruit_use",
        sold_prop_column = "fruit_sold_prop",
        consumed_column = "fruit_eaten_prop",
        processed_column = "fruit_process_prop",

        income_column = "fruit_sold_income",
        income_frequency = "fruit_sold_frequency",
        sold_frequency_other_column = "fruit_sold_amount_units_other",

        consume_gender="fruit_control_eating",
        sell_gender="fruit_who_sell",
        sell_income_gender="fruit_sold_income_who",


        processed_sold_column = "fruit_process_sold_prop",
        processed_eaten_column = "fruit_process_eaten_prop",

        process_sold_income_column = "fruit_process_sold_income",
        process_sold_frequency_column = "fruit_process_sold_frequency",
        process_sold_frequency_other_column = "fruit_process_sold_amount_units_other"
    ),

    nut=list(
        fp_name = "fp_name",
        base_name="nut",

        #Info needed for nut calculations
        amount = "nut_amount",
        amount_units = "nut_amount_units",
        amount_units_other = "nut_amount_units_other",

        # Info needed for proportions calculations
        use_column = "nut_use",
        sold_prop_column = "nut_sold_prop",
        consumed_column = "nut_eaten_prop",
        processed_column = "nut_process_prop",

        income_column = "nut_sold_income",
        income_frequency = "nut_sold_frequency",
        sold_frequency_other_column = "nut_sold_amount_units_other",

        consume_gender="nut_control_eating",
        sell_gender="nut_who_sell",
        sell_income_gender="nut_sold_income_who",

        processed_sold_column = "nut_process_sold_prop",
        processed_eaten_column = "nut_process_eaten_prop",

        process_sold_income_column = "nut_process_sold_income",
        process_sold_frequency_column = "nut_process_sold_frequency",
        process_sold_frequency_other_column = "nut_process_sold_amount_units_other"
    ),

    # Shea butter seems to be a special case
    shea_butter=list(
        fp_name = "fp_name",
        base_name="shea_butter",

        #Info needed for shea_butter calculations
        amount = "shea_butter_amount",
        amount_units = "shea_butter_amount_units",
        amount_units_other = "shea_butter_amount_units_other",

        # Info needed for proportions calculations
        use_column = "shea_butter_use",
        sold_prop_column = "shea_butter_sold",
        consumed_column = "shea_butter_consume",
        processed_column = "shea_butter_process_prop",

        income_column = "shea_butter_sold_income_per_freq",
        income_frequency = "shea_butter_sold_frequency",
        sold_frequency_other_column = "shea_butter_sold_amount_units_other",

        consume_gender=NULL,
        sell_gender="shea_butter_control_sell",
        sell_income_gender="shea_butter_sold_income_who",

        processed_sold_column = NULL,
        processed_eaten_column = NULL,

        process_sold_income_column = NULL,
        process_sold_frequency_column = NULL,
        process_sold_frequency_other_column = NULL
    ),
    leaves=list(
        fp_name = "fp_name",
        base_name="leaves",

        #Info needed for leaves calculations
        amount = "leaves_amount",
        amount_units = "leaves_amount_units",
        amount_units_other = "leaves_amount_units_other",

        # Info needed for proportions calculations
        use_column = "leaves_use",
        sold_prop_column = "leaves_sold_prop",
        consumed_column = "leaves_consumed_prop",
        processed_column = "leaves_process_prop",

        income_column = "leaves_sold_income",
        income_frequency = "leaves_sold_price_quantityunits",
        sold_frequency_other_column = "leaves_sold_amount_units_other",

        consume_gender="leaves_control_eating",
        sell_gender="leaves_who_sell",
        sell_income_gender="leaves_sold_income_who",

        processed_sold_column = "leaves_process_sold_prop",
        processed_eaten_column = "leaves_process_eaten_prop",

        process_sold_income_column = "leaves_process_income",
        process_sold_frequency_column = "leaves_process_sold_price_quantityunits",
        process_sold_frequency_other_column = "leaves_process_amount_units_other"
    ),
    bark=list(
        fp_name = "fp_name",
        base_name="bark",

        #Info needed for bark calculations
        amount = "bark_amount",
        amount_units = "bark_amount_units",
        amount_units_other = "bark_amount_units_other",

        # Info needed for proportions calculations
        use_column = "bark_use",
        sold_prop_column = "bark_sold_prop",
        consumed_column = "bark_eaten_prop",
        processed_column = "bark_process_prop",

        income_column = "bark_sold_income",
        income_frequency = "bark_sold_price_quantityunits",
        sold_frequency_other_column = "bark_sold_amount_units_other",

        consume_gender="bark_control_eating",
        sell_gender="bark_who_sell",
        sell_income_gender="bark_sold_income_who",

        processed_sold_column = "bark_process_sold_prop",
        processed_eaten_column = "bark_process_eaten_prop",

        process_sold_income_column = "bark_process_income",
        process_sold_frequency_column = "bark_process_sold_price_quantityunits",
        process_sold_frequency_other_column = "bark_process_amount_units_other"
    ),

    roots=list(
        fp_name = "fp_name",
        base_name="roots",

        #Info needed for bark calculations
        amount = "roots_amount",
        amount_units = "roots_amount_units",
        amount_units_other = "roots_amount_units_other",

        # Info needed for proportions calculations
        use_column = "roots_use",
        sold_prop_column = "roots_sold_prop",
        consumed_column = "roots_eaten_prop",
        processed_column = "roots_process_prop",

        income_column = "roots_sold_income",
        income_frequency = "roots_sold_price_quantityunits",
        sold_frequency_other_column = "roots_sold_amount_units_other",

        consume_gender="roots_control_eating",
        sell_gender="roots_who_sell",
        sell_income_gender="roots_sold_income_who",

        processed_sold_column = "roots_process_sold_prop",
        processed_eaten_column = "roots_process_eaten_prop",

        process_sold_income_column = "roots_process_income",
        process_sold_frequency_column = "roots_process_sold_price_quantityunits",
        process_sold_frequency_other_column = "roots_process_amount_units_other"
    ),

    gum=list(
        fp_name = "fp_name",
        base_name="gum",

        #Info needed for bark calculations
        amount = "gum_amount",
        amount_units = "gum_amount_units",
        amount_units_other = "gum_amount_units_other",

        # Info needed for proportions calculations
        use_column = "gum_use",
        sold_prop_column = "gum_sold_prop",
        consumed_column = "gum_eaten_prop",
        processed_column = "gum_process_prop",

        income_column = "gum_sold_income_weekly",
        income_frequency = "gum_sold_price_quantityunits",
        sold_frequency_other_column = "gum_sold_amount_units_other",

        consume_gender="gum_control_eating",
        sell_gender="gum_who_sell",
        sell_income_gender="gum_sold_income_who",

        processed_sold_column = "gum_process_sold_prop",
        processed_eaten_column = "gum_process_eaten_prop",

        process_sold_income_column = "gum_process_income",
        process_sold_frequency_column = "gum_process_sold_price_quantityunits",
        process_sold_frequency_other_column = "gum_process_amount_units_other"
    )




)

usethis::use_data(fp_products, overwrite = T)
