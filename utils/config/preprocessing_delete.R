delete_all_profiles <- function(env){

  del_result_grades <- del_profiles(env, "result_grades")
  
  del_result_statuses <- del_profiles(env, "result_statuses")
  
  del_mediums <- del_profiles(env, "mediums")
  
  #have to deal fish taxonomy first as it uses taxonomy levels
  del_fish_taxonomy <- del_profiles(env, "fish_taxonomy")
  
  del_taxonomy_levels <- del_profiles(env, "taxonomy_levels")
  
  del_projects <- del_profiles(env, "projects")
  
  #have to delete saved filters before deleting locations
  del_filters <- del_profiles(env, "filters")
  
  del_location_groups <- del_profiles(env, "location_groups")
  
  del_locations <- del_profiles(env, "locations")
  
  del_location_types <- del_profiles(env, "location_types")
  
  del_location_group_types <- del_profiles(env, "location_group_types")
  
  del_detection_conditions <- del_profiles(env, "detection_conditions")
  
  del_collection_methods <- del_profiles(env, "collection_methods")
  
  del_labs <- del_profiles(env, "labs")
  
  del_methods <- del_profiles(env, "methods")
  
  del_extended_attributes <- del_profiles(env, "extended_attributes")
  
  #observedProperties use units so have to be deleted first
  del_observed_properties <- del_profiles(env, "observed_properties")
  
  del_units <- del_profiles(env, "units")
  
  del_unit_groups <- del_profiles(env, "unit_groups")
  
}
