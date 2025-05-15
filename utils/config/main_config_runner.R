main <- function(env){

source("./utils/config/env_config.R")
source("./utils/config/api_functions.R")
source("./utils/config/preprocessing_units_unitsgroups.R")
source("./utils/config/preprocessing_observed_properties.R")
source("./utils/config/preprocessing_others.R")
  
post_unit_groups <- post_profiles(env, "unit_groups", unit_groups)
get_unit_groups <- get_profiles(env, "unit_groups")
post_units <- post_profiles(env, "units", units)
get_units <- get_profiles(env, "units")

post_observed_properties <- post_profiles(env, "observed_properties", observed_properties)
get_observed_properties <- get_profiles(env, "observed_properties")

put_result_grades <- put_profiles(env, "result_grades", result_grades)
get_result_grades <- get_profiles(env, "result_grades")
put_result_statuses <- put_profiles(env, "result_statuses", result_statuses)
get_result_statuses <- get_profiles(env, "result_statuses")

put_taxonomy_levels <- put_profiles(env, "taxonomy_levels", taxonomy_levels)
get_taxonomy_levels <- get_profiles(env, "taxonomy_levels")

put_mediums <- put_profiles(env, "mediums", mediums)
get_mediums <- get_profiles(env, "mediums")

post_collection_methods <- post_profiles(env, "collection_methods", collection_methods)
get_collection_methods <- get_profiles(env, "collection_methods")

post_taxons <- post_profiles(env, "fish_taxonomy", taxons)
get_taxons <- get_profiles(env, "fish_taxonomy")

post_extended_attributes <- post_profiles(env, "extended_attributes", extended_attributes)
get_extended_attributes <- get_profiles(env, "extended_attributes")

post_detection_conditions <- post_profiles(env, "detection_conditions", detection_conditions)
get_detection_conditions <- get_profiles(env, "detection_conditions")

post_projects <- post_profiles(env, "projects", projects)
get_projects <- get_profiles(env, "projects")

post_methods <- post_profiles(env, "methods", methods)
get_methods <- get_profiles(env, "methods")

post_labs <- post_profiles(env, "labs", labs)
get_labs <- get_profiles(env, "labs")

put_location_group_types <- put_profiles(env, "location_group_types", location_group_types)
get_location_group_types <- get_profiles(env, "location_group_types")
put_location_types <- put_profiles(env, "location_types", location_types)
get_location_types <- get_profiles(env, "location_types")
source("./utils/config/preprocessing_sampling_locations.R")
#Need to put locations in before putting in Location groups or Saved Filters
post_locationGroups <- post_profiles(env, "locationgroups", locationGroups)
get_locationGroups <- get_profiles(env, "locationgroups")
post_saved_filters <- post_profiles(env, "filters", saved_filters)
get_saved_filters <- get_profiles(env, "filters")

}

#Set the working environment
env = "prod"

main("prod")

# Call delete functions here that otherwise stay commented
del_result_grades <- del_profiles(env, "result_grades")

del_result_statuses <- del_profiles(env, "result_statuses")

del_mediums <- del_profiles(env, "mediums")

del_projects <- del_profiles(env, "projects")

del_locations <- del_profiles(env, "locations")

del_location_types <- del_profiles(env, "location_types")

del_location_groups <- del_profiles(env, "location_groups")

del_location_group_types <- del_profiles(env, "location_group_types")

del_fish_taxonomy <- del_profiles(env, "fish_taxonomy")

del_taxonomy_levels <- del_profiles(env, "taxonomy_levels")

#not working
del_detection_conditions <- del_profiles(env, "detection_conditions")

del_filters <- del_profiles(env, "filters")

del_collection_methods <- del_profiles(env, "collection_methods")

del_labs <- del_profiles(env, "labs")

del_methods <- del_profiles(env, "methods")

del_extended_attributes <- del_profiles(env, "extended_attributes")

#observedProperties use units so have to be deleted first
del_observed_properties <- del_profiles(env, "observed_properties")

del_units <- del_profiles(env, "units")

del_unit_groups <- del_profiles(env, "unit_groups")


