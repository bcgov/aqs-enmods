

#Set the working environment
env = "test"

source("./utils/config/env_config.R")
source("./utils/config/api_functions.R")
source("./utils/config/preprocessing_delete.R")

source("./utils/config/preprocessing_units_unitsgroups.R")
units_get <- get_profiles(env, "units")


units_4_edt <- select(units_get, id, customId, name)
units_4_edt$edt_unit_xref <- sub(" - .*", "", units_4_edt$name)

units_4_edt <- units_4_edt %>% rename("aqi_units_code" = "customId")
units_4_edt <- units_4_edt %>% rename("edt_unit_guid" = "id")
units_4_edt <- units_4_edt %>% select("edt_unit_guid", "edt_unit_xref", "aqi_units_code")

write.xlsx(units_4_edt, "units_4_edt")
