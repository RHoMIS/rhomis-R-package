library(tibble)

country = c(
    "BF"="burkina_faso",
    "BF"="burkina faso",
    "BI"="burundi",
    "BO"="bolivia",
    "CD"="drc",
    "CI"="cote d'ivoire",
    "CR"="costa_rica",
    "CR"="costa rica",
    "EC"="ecuador",
    "ET"="ethiopia",
    "GH"="ghana",
    "GT"="guatemala",
    "HN"="honduras",
    "IN"="india",
    "KE"="kenya",
    "KH"="cambodia",
    "KM"="comoros",
    "LA"="laopdr",
    "LA"="laos",
    "MA"="morocco",
    "ML"="mali",
    "MW"="malawi",
    "NE"="niger",
    "NG"="nigeria",
    "NI"="nicaragua",
    "PE"="peru",
    "SV"="el_salvador",
    "SV"="el salvador",
    "TZ"="tanzania",
    "UG"="uganda",
    "VN"="vietnam",
    "ZA"="south africa",
    "ZA"="south_africa",
    "ZM"="Zambia",
    "RW"="rwanda",
    "PS"="palestine",
    "SN"="senegal",
    "SL"="sierra leone",
    "SL"="sierra_leone",
    "MZ"="mozambique")

country <- tibble::as_tibble(list(
    "survey_value"=country,
    "conversion"=names(country)
    ))

usethis::use_data(country, overwrite = TRUE)

