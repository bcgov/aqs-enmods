#Set the working environment
env = "prod"

main <- function(env){

source("./utils/config/env_config.R")
source("./utils/config/api_functions.R")
source("./utils/config/preprocessing_units_unitsgroups.R")
unitGroups <- post_profiles(env, "unitgroups", unitGroups)
unitGroups <- get_profiles(env, "unitgroups")
units <- post_profiles(env, "units", units)
units <- get_profiles(env, "units")
source("./utils/config/preprocessing_observed_properties.R")
observedProperties <- post_profiles(env, "observedproperties", observedProperties)
observedProperties <- get_profiles(env, "observedproperties")
source("./utils/config/preprocessing_others.R")
resultGrades <- put_profiles(env, "resultgrades", resultGrades)
resultGrades <- get_profiles(env, "resultgrades")
resultStatuses <- put_profiles(env, "resultstatuses", resultStatuses)
resultStatuses <- get_profiles(env, "resultstatuses")
taxonomyLevels <- put_profiles(env, "taxonomylevels", taxonomyLevels)
taxonomyLevels <- get_profiles(env, "taxonomylevels")
mediums <- put_profiles(env, "mediums", mediums)
mediums <- get_profiles(env, "mediums")
collectionMethods <- post_profiles(env, "collectionmethods", collectionMethods)
collectionMethods <- get_profiles(env, "collectionmethods")
taxons <- post_profiles(env, "fishtaxonomy", taxons) 
taxons <- get_profiles(env, "fishtaxonomy") 
extendedAttributes <- post_profiles(env, "extendedattributes", extendedAttributes)
extendedAttributes <- get_profiles(env, "extendedattributes")
detectionconditions <- post_profiles(env, "detectionconditions", detectionConditions)
detectionconditions <- get_profiles(env, "detectionconditions")
savedFilters <- post_profiles(env, "filters", savedFilters)
savedFilters <- get_profiles(env, "filters")
projects <- post_profiles(env, "projects", projects)
projects <- get_profiles(env, "projects")
methods <- post_profiles(env, "methods", methods)
methods <- get_profiles(env, "methods")
labs <- post_profiles(env, "labs", labs)
labs <- get_profiles(env, "labs")
locationGroupTypes <- put_profiles(env, "locationgrouptypes", locationGroupTypes)
locationGroupTypes <- get_profiles(env, "locationgrouptypes")
source("./utils/config/preprocessing_sampling_locations.R")
locationTypes <- put_profiles(env, "locationtypes", locationTypes)
locationTypes <- get_profiles(env, "locationtypes")
locationGroups <- post_profiles(env, "locationgroups", locationGroups)
locationGroups <- get_profiles(env, "locationgroups")

}

main("prod")

# # Call top-level functions here
# process_units()
# process_observed_properties()
# process_sampling_locations()

