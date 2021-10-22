library(tibble)

country = c(
    "BF"="Burkina_Faso",
    "BI"="Burundi",
    "BO"="Bolivia",
    "CD"="DRC",
    "CI"="Cote d'Ivoire",
    "CR"="Costa_Rica",
    "EC"="Ecuador",
    "ET"="Ethiopia",
    "GH"="Ghana",
    "GT"="Guatemala",
    "HN"="Honduras",
    "IN"="India",
    "KE"="Kenya",
    "KH"="Cambodia",
    "KM"="Comoros",
    "LA"="LaoPDR",
    "MA"="Morocco",
    "ML"="Mali",
    "MW"="Malawi",
    "NE"="Niger",
    "NG"="Nigeria",
    "NI"="Nicaragua",
    "PE"="Peru",
    "SV"="El_Salvador",
    "TZ"="Tanzania",
    "UG"="Uganda",
    "VN"="Vietnam",
    "ZA"="South Africa",
    "ZM"="Zambia",
    "RW"="Rwanda",
    "PS"="Palestine",
    "SN"="Senegal",
    "SL"="Sierra Leone",
    "MZ"="Mozambique")

country <- tibble::as_tibble(list(
    "survey_value"=country,
    "conversion"=names(country)
    ))

usethis::use_data(country, overwrite = TRUE)

