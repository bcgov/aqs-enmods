source("./utils/config/env_config.R")
source("./utils/config/api_functions.R")
source("./utils/config/preprocessing_units_unitsgroups.R")
unitGroups <- post_profiles("prod", "unitgroups", unitGroups)
unitGroups <- get_profiles("prod", "unitgroups")
units <- post_profiles("prod", "units", units)
units <- get_profiles("prod", "units")
source("./utils/config/preprocessing_observed_properties.R")
observedProperties <- post_profiles("prod", "observedproperties", observedProperties)
observedProperties <- get_profiles("prod", "observedproperties")
source("./utils/config/preprocessing_others.R")
source("./utils/config/preprocessing_sampling_locations.R")


# Call top-level functions here
process_units()
process_observed_properties()
process_sampling_locations()

