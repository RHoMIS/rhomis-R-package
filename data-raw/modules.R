library(tibble)
library(rhomis)
library(tidyr)

modules <- list(
    create_module(module_name ="metadata",processing_code = NA,module_type = "core",dependencies = NA),
    create_module(module_name ="introduction",processing_code = NA,module_type = "core",dependencies = NA),
    create_module(module_name ="demographics",processing_code = NA,module_type = "core",dependencies = NA),
    create_module(module_name ="land_use",processing_code = NA,module_type = "core",dependencies = NA),
    create_module(module_name ="gender_roles",processing_code = NA,module_type = "core",dependencies = NA),
    create_module(module_name ="crops",processing_code = NA,module_type = "core",dependencies = NA),
    create_module(module_name ="crop_intensification",processing_code = NA,module_type = "core",dependencies = NA),
    create_module(module_name ="crop_intensification_practices",processing_code = NA,module_type = "core",dependencies = NA),
    create_module(module_name ="livestock",processing_code = NA,module_type = "core",dependencies = NA),
    create_module(module_name ="livestock_intensification",processing_code = NA,module_type = "core",dependencies = NA),
    create_module(module_name ="wildfoods",processing_code = NA,module_type = "core",dependencies = NA),
    create_module(module_name ="food_security",processing_code = NA,module_type = "core",dependencies = NA),
    create_module(module_name ="hdds",processing_code = NA,module_type = "core",dependencies = NA),
    create_module(module_name ="debts_and_aid",processing_code = NA,module_type = "core",dependencies = NA),
    create_module(module_name ="off_farm_income",processing_code = NA,module_type = "core",dependencies = NA),
    create_module(module_name ="spending",processing_code = NA,module_type = "core",dependencies = NA),
    create_module(module_name ="ppi",processing_code = NA,module_type = "core",dependencies = NA),
    create_module(module_name ="phone",processing_code = NA,module_type = "core",dependencies = NA)
    )

modules <- dplyr::bind_rows(modules)

usethis::use_data(modules,overwrite = T)
