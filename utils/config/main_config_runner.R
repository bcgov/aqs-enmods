main <- function(env){

  #Set the working environment
  env = "prod"
  
  source("./utils/config/env_config.R")
  source("./utils/config/api_functions.R")
  source("./utils/config/preprocessing_delete.R")
  
  # Delete all profiles
  delete_all_profiles(env)

  source("./utils/config/preprocessing_units_unitsgroups.R")
  unit_groups_post <- post_profiles(env, "unit_groups", unit_groups)
  unit_groups_get <- get_profiles(env, "unit_groups")
  units_post <- post_profiles(env, "units", units)
  units_get <- get_profiles(env, "units")
  
  source("./utils/config/preprocessing_observed_properties.R")
  observed_properties_post <- post_profiles(env, "observed_properties", observed_properties)
  observed_properties_get <- get_profiles(env, "observed_properties")
  
  source("./utils/config/preprocessing_others.R")
  result_grades_put <- put_profiles(env, "result_grades", result_grades)
  result_grades_get <- get_profiles(env, "result_grades")
  result_statuses_put <- put_profiles(env, "result_statuses", result_statuses)
  result_statuses_get <- get_profiles(env, "result_statuses")

  taxonomy_levels_put <- put_profiles(env, "taxonomy_levels", taxonomy_levels)
  taxonomy_levels_get <- get_profiles(env, "taxonomy_levels")
  taxons_post <- post_profiles(env, "fish_taxonomy", taxons)
  taxons_get <- get_profiles(env, "fish_taxonomy")

  mediums_put <- put_profiles(env, "mediums", mediums)
  mediums_get <- get_profiles(env, "mediums")

  collection_methods_post <- post_profiles(env, "collection_methods", collection_methods)
  collection_methods_get <- get_profiles(env, "collection_methods")
  
  analytical_groups_get <- get_profiles("test", "analytical_groups")

  extended_attributes_post <- post_profiles(env, "extended_attributes", extended_attributes)
  extended_attributes_get <- get_profiles(env, "extended_attributes")
  
  sampling_agency_get <- get_profiles(env, "sampling_agency")

  detection_conditions_post <- post_profiles(env, "detection_conditions", detection_conditions)
  detection_conditions_get <- get_profiles(env, "detection_conditions")
  
  projects_post <- post_profiles(env, "projects", projects)
  projects_get <- get_profiles(env, "projects")

  methods_post <- post_profiles(env, "methods", methods)
  methods_get <- get_profiles(env, "methods")

  labs_post <- post_profiles(env, "labs", labs)
  labs_get <- get_profiles(env, "labs")

  location_group_types_put <- put_profiles(env, "location_group_types", location_group_types)
  location_group_types_get <- get_profiles(env, "location_group_types")
  location_types_put <- put_profiles(env, "location_types", location_types)
  location_types_get <- get_profiles(env, "location_types")
  source("./utils/config/preprocessing_sampling_locations.R")
  location_groups_post <- post_profiles(env, "location_groups", location_groups)
  location_groups_get <- get_profiles(env, "location_groups")
  #Need to put locations in before putting in Saved Filters
  locations_get <- get_profiles(env, "locations")
  source("./utils/config/preprocessing_saved_filters.R")
  saved_filters_post <- post_profiles(env, "filters", saved_filters)
  saved_filters_get <- get_profiles(env, "filters")
  
  source("./utils/config/postprocessing_qa_qc.R")
  qa_qc_all_profiles()

}

#Set the working environment
env = "prod"

main("prod")

#Things to Do:
#Generate consolidated Collection Methods accounting for the required method
#Do QA/QC for all data types
#Make package
#Do QA/QC for specific functions: Generate message whenever POST/PUT/GET/DELETE

