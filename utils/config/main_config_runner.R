#Set the working environment
env = "prod"

#main <- function(env){

source("./utils/config/env_config.R")
source("./utils/config/api_functions.R")
source("./utils/config/preprocessing_units_unitsgroups.R")
unit_groups <- post_profiles(env, "unit_groups", unit_groups)
unit_groups <- get_profiles(env, "unit_groups")
units <- post_profiles(env, "units", units)
units <- get_profiles(env, "units")
source("./utils/config/preprocessing_observed_properties.R")
observed_properties <- post_profiles(env, "observed_properties", observed_properties)
observed_properties <- get_profiles(env, "observed_properties")
source("./utils/config/preprocessing_others.R")
result_grades <- put_profiles(env, "result_grades", result_grades)
result_grades <- get_profiles(env, "result_grades")
result_statuses <- put_profiles(env, "result_statuses", result_statuses)
result_statuses <- get_profiles(env, "result_statuses")
taxonomy_levels <- put_profiles(env, "taxonomy_levels", taxonomy_levels)
taxonomy_levels <- get_profiles(env, "taxonomy_levels")
mediums <- put_profiles(env, "mediums", mediums)
mediums <- get_profiles(env, "mediums")
collection_methods <- post_profiles(env, "collection_methods", collection_methods)
collection_methods <- get_profiles(env, "collection_methods")
taxons <- post_profiles(env, "fish_taxonomy", taxons)
taxons <- get_profiles(env, "fish_taxonomy")
extended_attributes <- post_profiles(env, "extended_attributes", extended_attributes)
extended_attributes <- get_profiles(env, "extended_attributes")
detection_conditions <- post_profiles(env, "detection_conditions", detection_conditions)
detection_conditions <- get_profiles(env, "detection_conditions")
projects <- post_profiles(env, "projects", projects)
projects <- get_profiles(env, "projects")
methods <- post_profiles(env, "methods", methods)
methods <- get_profiles(env, "methods")
labs <- post_profiles(env, "labs", labs)
labs <- get_profiles(env, "labs")
location_group_types <- put_profiles(env, "location_group_types", location_group_types)
location_group_types <- get_profiles(env, "location_group_types")
source("./utils/config/preprocessing_sampling_locations.R")
locationTypes <- put_profiles(env, "locationtypes", locationTypes)
locationTypes <- get_profiles(env, "locationtypes")
locationGroups <- post_profiles(env, "locationgroups", locationGroups)
locationGroups <- get_profiles(env, "locationgroups")
#Need to put locations in before putting in Saved Filters
saved_filters <- post_profiles(env, "filters", saved_filters)
saved_filters <- get_profiles(env, "filters")

# }
# 
# main("prod")

# # Call top-level functions here
# process_units()
# process_observed_properties()
# process_sampling_locations()

