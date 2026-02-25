main <- function(env) {
  #' Title: main
  #' @description
  #'  Load key environment variables
  #' @param env AQS environment. Takes values "prod" or "test"
  #' @returns the user input, as well as the status of get and post/put to AQI
  #' @import httr jsonlite tidyverse purrr dplyr lubridate stringr bcdata sf tidygeocoder readr readxl writexl openxlsx hunspell tidyr
  #' @importFrom magrittr %>%
  #' @export
  #' @examples

  # source("./R/env_config.R")
  # source("./R/api_functions.R")

  # loads all functions, data, and documentation as if the package was installed
  devtools::load_all()

  library(enmods.tools)

  # set the environment variables to the necessary values
  readRenviron(paste0("C:/Users/SABHANDA/OneDrive - Government of BC/teamdocs/GitHub/aqs-enmods", "/.Renviron"))
  load_aqs_tokens_urls()

  # Set the working environment
  # env <- "test"

  # 20260105: COMMENTED THIS AS DANGEROUS CONSEQUENCES
  # Delete all profiles
  # delete_all_profiles(env)

  # TEST Env fixes that need to be made
  # spelling mistake in Apperance
  # Also two separate units Sys Required Length and Length: they should be the same
  # source("./R/preprocessing_units_unitsgroups.R")
  # set run_init to TRUE when developing a consolidated units file from raw files, and not just updating it
  preprocessing_units_unitsgroups(env, run_init = TRUE)
  unit_groups <- import_files(env, "unit_groups")
  unit_groups_post <- post_profiles(env, "unit_groups", unit_groups)
  # units_groups_get <-  read_excel("./inst/extdata/Reference_Lists/Backup_prod_unit_groups.xlsx")
  # unit_groups_post <- post_profiles(env, "unit_groups", units_groups_get)
  unit_groups_get <- get_profiles(env, "unit_groups")
  unit_groups_check <- unit_groups %>% anti_join(unit_groups_get,
    by = join_by(sample_unit_group == customId)
  )

  units <- import_files(env, "units")
  units_post <- post_profiles(env, "units", units)
  units_get <- get_profiles(env, "units")
  units_check <- units %>% anti_join(units_get,
    by = join_by(sample_unit_customid == customId)
  )

  env <- "prod"
  # source("./R/preprocessing_observed_properties.R")
  # OP Get and Post won't match because we have multiple parm_code for same newnameid
  preprocessing_observed_properties(env, run_init = FALSE)
  observed_properties <- import_files(env, "observed_properties")
  # observed_properties_test <- observed_properties[1, ] #%>%
  # #mutate(across(everything(), ~ ifelse(. == "NA", "", .))) %>%
  # #mutate(across(everything(), ~ str_replace_na(., ""))) %>%
  # dplyr::mutate(analysis_type = case_when(
  #   newnameid == "Biological Sex (cat.)" ~ NA,
  #   newnameid == "Biological Life Stage (cat.)" ~ NA,
  #   #sample_unit_group == "Length" ~ "SYS-REQUIRED - Length",
  #   #sample_unit_group == "Appearance" ~ "Apperance",
  #   #unit == "per mille" ~ "DimensionlessRatio"
  #   .default = analysis_type
  # ))
  # env = "prod"
  observed_properties_get_old <- get_profiles(env, "observed_properties")
  observed_properties_put <- put_profiles(env, "observed_properties", observed_properties)
  observed_properties_post <- post_profiles(env, "observed_properties", observed_properties)
  observed_properties_get <- get_profiles(env, "observed_properties")
  observed_properties_check <- observed_properties %>% anti_join(observed_properties_get,
    by = join_by(newnameid == customId)
  )

  # source("./R/preprocessing_others.R")
  preprocessing_others(env)
  analysis_methods <- import_files(env, "analysis_methods")
  analysis_methods <- put_profiles(env, "analysis_methods", analysis_methods)
  analysis_methods_post <- post_profiles(env, "analysis_methods", analysis_methods)
  analysis_methods_get <- get_profiles(env, "analysis_methods")
  analysis_methods_check <- analysis_methods %>% anti_join(analysis_methods_get,
    by = join_by(method_code == methodId)
  )

  analytical_groups <- import_files(env, "analytical_groups")
  analytical_groups_post <- post_profiles(env, "analytical_groups", analytical_groups)
  analytical_groups_put <- put_profiles(env, "analytical_groups", analytical_groups)
  analytical_groups_get <- get_profiles(env, "analytical_groups")

  is_empty_list <- function(lst) {
    is.list(lst) && length(lst) == 0
  }

  result_grades <- import_files(env, "result_grades")
  result_grades_put <- put_profiles(env, "result_grades", result_grades)
  result_grades_get <- get_profiles(env, "result_grades")
  result_statuses <- import_files(env, "result_statuses")
  result_statuses_put <- put_profiles(env, "result_statuses", result_statuses)
  result_statuses_get <- get_profiles(env, "result_statuses")

  taxonomy_levels <- import_files(env, "taxonomy_levels")
  taxonomy_levels_put <- put_profiles(env, "taxonomy_levels", taxonomy_levels)
  taxonomy_levels_get <- get_profiles(env, "taxonomy_levels")
  taxons <- import_files(env, "taxons")
  taxons_post <- post_profiles(env, "fish_taxonomy", taxons)
  taxons_get <- get_profiles(env, "fish_taxonomy")

  mediums <- import_files(env, "mediums")
  mediums_put <- put_profiles(env, "mediums", mediums)
  mediums_get <- get_profiles(env, "mediums")

  collection_methods <- import_files(env, "collection_methods")
  # creating a put for collection methods as doing POST adds a new unnecessary method automatically from AQS end
  collection_methods_put <- put_profiles(env, "collection_methods", collection_methods)
  collection_methods_post <- post_profiles(env, "collection_methods", collection_methods)
  collection_methods_get <- get_profiles(env, "collection_methods")

  extended_attributes <- import_files(env, "extended_attributes")
  extended_attributes_post <- post_profiles(env, "extended_attributes", extended_attributes)
  extended_attributes_test <- get_profiles("test", "extended_attributes")
  extended_attributes_test_agency_ddl <- extended_attributes_test %>%
    dplyr::filter(customId == "Sampling Agency") %>%
    dplyr::select(id) %>%
    unlist()
  url_parameters <- update_base_url_token("test")
  base_url <- url_parameters[[1]]
  extended_attributes_ddl_test <- get_profiles_for_url("test", str_c(
    base_url, "v1/extendedattributes/",
    extended_attributes_test_agency_ddl, "/dropdownlistitems"
  ))
  extended_attributes_prod <- get_profiles("prod", "extended_attributes")
  extended_attributes_prod_agency_ddl <- extended_attributes_prod %>%
    dplyr::filter(customId == "Sampling Agency") %>%
    dplyr::select(id) %>%
    unlist()
  url_parameters <- update_base_url_token("prod")
  base_url <- url_parameters[[1]]
  extended_attributes_ddl_prod <- get_profiles_for_url("prod", str_c(
    base_url, "v1/extendedattributes/",
    extended_attributes_prod_agency_ddl, "/dropdownlistitems"
  ))
  extended_attributes_ddl_check <- extended_attributes_ddl_test %>%
    rename(customId_test = customId) %>%
    left_join(extended_attributes_ddl_prod %>% rename(customId_prod = customId),
      by = join_by("customId_test" == "customId_prod")
    )

  sampling_agencies <- import_files(env, "sampling_agencies")
  sampling_agencies_put <- put_profiles(env, "sampling_agencies", sampling_agencies)
  sampling_agencies_prod <- get_profiles(env, "sampling_agencies")
  sampling_agencies_test <- get_profiles("test", "sampling_agencies")
  sampling_agencies_check <- sampling_agencies_test %>%
    rename(customId_test = customId) %>%
    full_join(sampling_agencies_prod %>% rename(customId_prod = customId),
      by = join_by("customId_test" == "customId_prod")
    )

  labs <- import_files(env, "labs")
  labs_post <- post_profiles(env, "labs", labs)
  labs_prod <- get_profiles(env, "labs")
  labs_put <- put_profiles(env, "labs", labs)
  labs_test <- get_profiles("test", "labs")
  labs_check <- labs_test %>%
    # dplyr::select(id, customId) %>%
    rename(customId_test = customId) %>%
    full_join(
      labs_prod %>%
        # dplyr::select(id, customId) %>%
        rename(customId_prod = customId),
      by = join_by("customId_test" == "customId_prod")
    )
  # test has no labs that do not exist in prod but is missing labs
  labs_put <- put_profiles("test", "labs", labs)

  detection_conditions <- import_files(env, "detection_conditions")
  detection_conditions_post <- post_profiles(env, "detection_conditions", detection_conditions)
  detection_conditions_put <- put_profiles(env, "detection_conditions", detection_conditions)
  detection_conditions_get <- get_profiles(env, "detection_conditions")

  # #preprocessing projects since only 9 in the XLSX file but 79 in test
  # projects_preproc <- get_profiles("test", "projects") %>%
  #   rename(ID = customId,
  #          Name = name,
  #          Type = type,
  #          StartDate = startTime,
  #          EndDate = endTime,
  #          Comments = description,
  #          Scope = scopeStatement) %>%
  #   dplyr::filter(!str_detect(tolower(ID), "testing|training")) %>%
  #   dplyr::select(-c(id, auditAttributes, approved))
  # projects <- import_files(env, "projects") #%>%
  # rbind(projects_preproc)
  projects_post <- post_profiles(env, "projects", projects)
  projects_get <- get_profiles(env, "projects")

  location_group_types <- import_files(env, "location_group_types")
  location_group_types_put <- put_profiles(env, "location_group_types", location_group_types)
  location_group_types_get <- get_profiles(env, "location_group_types")

  location_types_preproc <- get_profiles("test", "location_types")
  location_types <- import_files(env, "location_types")
  location_types_put <- put_profiles(env, "location_types", location_types)
  location_types_get <- get_profiles(env, "location_types")
  # source("./R/preprocessing_sampling_locations.R")

  preprocessing_sampling_locations(env)
  location_groups <- import_files(env, "location_groups")
  location_groups_post <- post_profiles(env, "location_groups", location_groups)
  location_groups_get <- get_profiles(env, "location_groups")
  location_groups_get <- get_profiles("test", "location_groups") %>%
    mutate(name = as.numeric(name))
  location_groups_check <- location_groups %>%
    full_join(location_groups_get,
      by = join_by("permit_id" == "name")
    ) %>%
    dplyr::filter(is.na(type)) %>%
    inner_join(ams_permits,
      by = join_by("permit_id" == "authorization_number")
    )
  # Need to put locations in before putting in Saved Filters
  locations_get <- get_profiles(env, "locations")
  # source("./R/preprocessing_saved_filters.R")

  preprocessing_saved_filters(env)
  saved_filters <- import_files(env, "saved_filters")
  saved_filters_post <- post_profiles(env, "filters", saved_filters)
  saved_filters_get <- get_profiles(env, "filters")
  saved_filters_test <- get_profiles("test", "filters")
  saved_filters_check <- saved_filters_test %>%
    dplyr::select(customId) %>%
    rename(customid_test = customId) %>%
    anti_join(saved_filters_get %>% rename(customid_prod = customId),
      by = join_by("customid_test" == "customid_prod")
    )
  # source("./R/postprocessing_qa_qc.R")

  analytical_groups_check <- qa_qc_profile(env, "analytical_groups")
  observed_properties_check <- qa_qc_profile(env, "observed_properties")
}

# Set the working environment
# env = "prod"

# main("prod")

# Things to Do:
# Discuss saved filters duplication with different dates with JK
# Discuss analysis methods duplication (different # of methods and method codes and neither matches the POST)
# Do QA/QC for specific functions: Generate message whenever POST/PUT/GET/DELETE
