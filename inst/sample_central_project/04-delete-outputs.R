library(mongolite)

data_collection <- mongolite::mongo(collection = "data",
                                    db = "rhomis-test",
                                    url =  "mongodb://localhost")
data_collection$drop()
data_collection$disconnect()

project_data_collection <- mongolite::mongo(collection = "projectData",
                                            db = "rhomis-test",
                                            url =  "mongodb://localhost")
project_data_collection$drop()
project_data_collection$disconnect()

units_and_conversion_collection <- mongolite::mongo(collection = "units_and_conversions",
                                            db = "rhomis-test",
                                            url =  "mongodb://localhost")
units_and_conversion_collection$drop()
units_and_conversion_collection$disconnect()



print("Database cleared")
