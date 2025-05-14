# FILE TO PREPROCESS OBSERVED PROPERTIES
source("./utils/config/api_functions.R")

# PREPROCESSING TO CONSOLIDATE OLDER OP FILES -----------------------------

run_init <- FALSE
if (run_init) {

  #EMS exported observed_properties
  observed_properties <- read_excel("./utils/config/ReferenceLists/Observed_Properties.xlsx") %>%
    rename_with(tolower) %>%
    rename_with(~ gsub("\\.", "_", .))
  
  #The units don't matter for non-convertable OP units. For example microbial units. So remove them from consideration.
  observed_properties$sample_unit[observed_properties$convertable_in_samples == "N"] <- ""
  
  #clean up modifers
  observed_properties$modifier[is.na(observed_properties$modifier)] <- ""
  
  #Add an empty result_type column
  observed_properties$result_type <- ""
  
  #Keep the core columns at the front
  cols_to_move <- c("newnameid", "parm_code", "description",
                    "analysis_type", "result_type", "sample_unit_group",
                    "sample_unit","CAS")
  
  #get the unique list of OP IDs
  observed_properties <- observed_properties %>% unique()
  
  
  ## observed_properties new to EnMoDS not in EMS
  observed_properties_new_to_EnMoDS <-
    read_excel("./utils/config/ReferenceLists/New_OPS_not_in_EMS.xlsx",
               sheet = "observed_properties_new") %>% 
    rename_with(tolower) %>%
    rename_with(~ gsub("\\.", "_", .))
  
  #get the unique list of OP IDs
  observed_properties_new_to_EnMoDS <- observed_properties_new_to_EnMoDS %>% unique()
  
  #fixing the missing sample group id issue in the list
  observed_properties_new_to_EnMoDS <- observed_properties_new_to_EnMoDS %>% mutate(sample_unit_group =
                                                      if_else(newnameid == "Biological Sample Volume (vol.)",
                                                              "Volume", sample_unit_group))
  
  #Identify the new columns compared to observed_properties
  new_cols <- setdiff(names(observed_properties), names(observed_properties_new_to_EnMoDS))
  
  # Create a tibble of just those new columns
  new_data <- observed_properties %>%
    dplyr::select(all_of(new_cols)) %>%
    mutate(across(everything(), ~ NA)) %>%
    unique() %>% slice(rep(1, nrow(observed_properties_new_to_EnMoDS)))
  
  observed_properties_new_to_EnMoDS <- observed_properties_new_to_EnMoDS %>% bind_cols(new_data)
  
  observed_properties <- observed_properties %>% bind_rows(observed_properties_new_to_EnMoDS %>% mutate(Results = NA)) %>% unique()
  
  ## Taxonomic observed_properties
  observed_properties_taxonomic <- read.csv("./utils/config/ReferenceLists/Taxonomic_OP.csv", 
                                            stringsAsFactors = F) %>% 
    rename_with(tolower) %>%
    rename_with(~ gsub("\\.", "_", .)) #1607 
  
  #get the unique list of OP IDs
  observed_properties_taxonomic <- observed_properties_taxonomic %>% unique()
  
  #Identify the new columns compared to observed_properties
  new_cols <- setdiff(names(observed_properties), names(observed_properties_taxonomic))
  
  # Create a tibble of just those new columns
  new_data <- observed_properties %>%
    dplyr::select(all_of(new_cols)) %>%
    mutate(across(everything(), ~ NA)) %>%
    unique() %>% slice(rep(1, nrow(observed_properties_taxonomic)))
  
  observed_properties_taxonomic <- observed_properties_taxonomic %>% bind_cols(new_data)
  
  observed_properties <- observed_properties %>% bind_rows(observed_properties_taxonomic %>% mutate(Results = NA)) %>% unique()
  
  ## Merge all observed_properties and process
  
  observed_properties <- observed_properties %>%
    #bind_rows(observed_properties, observed_properties_new_to_EnMoDS, observed_properties_taxonomic) %>%
    #        unique() %>%
    group_by(newnameid) %>%
    mutate(count = n()) %>%
    ungroup()
  
  #the total number of unique newnameid should be the same as the total number of unique rows
  new_name_id_unique <- observed_properties$newnameid %>% unique()
  
  observed_properties <- observed_properties %>%
    mutate(sample_unit = case_when(
      sample_unit == "pH Units" ~ "pH units",
      #sample_unit == NA ~ Unit,
      #sample_unit == "" ~ NA,
      .default = sample_unit),
    )
  
  #rename the required observed_properties so they show up otherwise they are in the background?
  observed_properties <- observed_properties %>%
    mutate(sample_unit_group = case_when(
      newnameid == "Biological Sex (cat.)" ~ "None",
      newnameid == "Biological Life Stage (cat.)" ~ "None",
      sample_unit_group == "Length" ~ "SYS-REQUIRED - Length",
      sample_unit_group == "Apperance"~ "Appearance",
      .default = sample_unit_group
    )) %>%
    mutate(analysis_type = case_when(
      newnameid == "Biological Sex (cat.)" ~ "BIOLOGICAL",
      newnameid == "Biological Life Stage (cat.)" ~ "BIOLOGICAL",
      .default = analysis_type
    ))
  
  #need to get unit group and unit IDs prior to importing observed_properties
  unit_groups <- get_profiles(env, "unit_groups") %>%
    dplyr::select(id, customId, supportsConversion) %>%
    rename("unit_group_id" = "id")
  
  #they are not!
  #current pipeline will upload the first OP into the system
  #But its fine because...
  #they belong to unit groups that are convertible
  #Below code helps you make that check
  observed_properties_convertible <- observed_properties %>%
    dplyr::filter(count>1) %>%
    left_join(unit_groups %>%
                dplyr::select(customId, unit_group_id, supportsConversion),
              by = join_by("sample_unit_group" == "customId")) %>%
    dplyr::filter(supportsConversion == FALSE)
  
  # observed_properties_missing_unitgroup <- observed_properties %>%
  #   dplyr::filter(is.na(unit_group_id))
  
  #add GUID to the list of observed_properties for unit groups
  observed_properties <- left_join(observed_properties, unit_groups,
                                  by = join_by('sample_unit_group' == 'customId'), keep = FALSE)
  
  #units without groups
  units <- get_profiles(env, "units") %>%
    dplyr::select(id, customId) %>%
    rename("unit_id" = "id")
  
  #add GUID to the list of observed_properties for units
  observed_properties <- left_join(observed_properties, units,
                                  by = join_by('sample_unit' == 'customId'), keep = FALSE)
  
  #analysis.Type must be ALL CAPS
  observed_properties$analysis_type <- toupper(observed_properties$analysis_type)
  observed_properties$result_type <- toupper(observed_properties$result_type)
  
  write_xlsx(observed_properties, "./utils/config/ReferenceLists/Consolidated_Observed_Properties.xlsx")
  
  # Load an existing workbook
  wb <- loadWorkbook("./utils/config/ReferenceLists/Consolidated_Observed_Properties.xlsx")
  
  # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
  renameWorksheet(wb, sheet = "Sheet1", newName = "Observed_Properties")
  
  # Save the workbook with the updated sheet name
  saveWorkbook(wb, "./utils/config/ReferenceLists/Consolidated_Observed_Properties.xlsx", overwrite = TRUE)
  
  observed_properties <- observed_properties %>% dplyr::select(c("parm_code", "newnameid", "description", "analysis_type", "result_type", "sample_unit_group", "sample_unit","CAS")) %>% unique()
  
}

# PREPROCESSING OP FOR NEW DATA --------------------------------------

#Get base file; Unit and unit groups may have been updated; remove their IDs
observed_properties_base <- 
  read_excel("./utils/config/ReferenceLists/Consolidated_Observed_Properties.xlsx") %>%
  rename_with(tolower) %>%
  rename_with(~ gsub("\\.", "_", .)) %>%
  rename(supports_conversion = supportsconversion) %>%
  dplyr::select(-c(supports_conversion, unit_group_id, unit_id))

#The units don't matter for non-convertable OP units. For example microbial units. So remove them from consideration.
observed_properties_base$sample_unit[observed_properties_base$convertable_in_samples == "N"] <- ""

#clean up modifers
observed_properties_base$modifier[is.na(observed_properties_base$modifier)] <- ""

#Add an empty result_type column
observed_properties_base$result_type <- ""

#checking if a new file is in the folder
file_exists <- length(list.files(pattern = "^Observed_properties_ems_jk_")) > 0

#if file matching pattern exists in this folder, it would be in this list
file_exists <- list.files(path = "./utils/config/ReferenceLists/", 
                          pattern = "^Observed_properties_ems_jk_", full.names = TRUE)

#ADD NEW FILE HERE; IF NO NEW FILE, CONSOLIDATED BASE OPS FILE WILL BE USED
#picks the latest file of all the files identified
if (length(file_exists) > 0) {
  latest_file <- file_exists[which.max(file.info(file_exists)$mtime)]
  message("Latest file found: ", latest_file)

observed_properties_new <- read_excel(latest_file)

#The units don't matter for non-convertable OP units. For example microbial units. So remove them from consideration.
observed_properties_new$sample_unit[observed_properties_new$convertable_in_samples == "N"] <- ""

#clean up modifers
observed_properties_new$modifier[is.na(observed_properties_new$modifier)] <- ""

#Add an empty result_type column
observed_properties_new$result_type <- ""

#Identify the new columns compared to observed_properties
new_cols <- setdiff(names(observed_properties_base), names(observed_properties_new))

# Create a tibble of just those new columns
new_data <- observed_properties_base %>%
  dplyr::select(all_of(new_cols)) %>%
  mutate(across(everything(), ~ NA)) %>%
  unique() %>% slice(rep(1, nrow(observed_properties_new)))

observed_properties_new <- observed_properties_new %>% bind_cols(new_data)

observed_properties <- observed_properties_base %>% 
  bind_rows(observed_properties_new %>% mutate(Results = NA)) %>% unique()

## Merge all observed_properties
observed_properties <- observed_properties %>%
  #bind_rows(observed_properties, observed_properties_new_to_EnMoDS, observed_properties_taxonomic) %>%
  #        unique() %>% 
  group_by(newnameid) %>% 
  mutate(count = n()) %>%
  ungroup()

#the total number of unique newnameid should be the same as the total number of unique rows
new_name_id_unique <- observed_properties$newnameid %>% unique()

observed_properties <- observed_properties %>% 
  mutate(sample_unit = case_when(
    sample_unit == "pH Units" ~ "pH units",
    #sample_unit == NA ~ Unit,
    #sample_unit == "" ~ NA,
    .default = sample_unit),
  )

#rename the required observed_properties so they show up otherwise they are in the background?
observed_properties <- observed_properties %>% 
  mutate(sample_unit_group = case_when(
    newnameid == "Biological Sex (cat.)" ~ "None",
    newnameid == "Biological Life Stage (cat.)" ~ "None",
    sample_unit_group == "Length" ~ "SYS-REQUIRED - Length",
    sample_unit_group == "Apperance"~ "Appearance",
    .default = sample_unit_group
  )) %>% 
  mutate(analysis_type = case_when(
    newnameid == "Biological Sex (cat.)" ~ "BIOLOGICAL",
    newnameid == "Biological Life Stage (cat.)" ~ "BIOLOGICAL",
    .default = analysis_type
  ))

} else {
  
  #Unit and unit groups may have been updated; remove their IDs
  observed_properties <- observed_properties_base
  
}

#need to get unit group and unit IDs prior to importing observed_properties
unit_groups <- get_profiles(env, "unit_groups") %>% 
  dplyr::select(id, customId, supportsConversion) %>%
  rename("unit_group_id" = "id")

#they are not! 
#current pipeline will upload the first OP into the system
#But its fine because...
#they belong to unit groups that are convertible
#Below code helps you make that check
observed_properties_convertible <- observed_properties %>% 
  dplyr::filter(count>1) %>%
  left_join(unit_groups %>% 
              dplyr::select(customId, unit_group_id, supportsConversion), 
            by = join_by("sample_unit_group" == "customId")) %>%
  dplyr::filter(supportsConversion == FALSE)

#add GUID to the list of observed_properties for unit groups
observed_properties <- left_join(observed_properties, unit_groups, 
                                by = join_by('sample_unit_group' == 'customId'), 
                                keep = FALSE)

observed_properties_missing_unitgroup <- observed_properties %>% dplyr::filter(is.na(unit_group_id))

#units without groups
units <- get_profiles(env, "units") %>%
  dplyr::select(id, customId) %>%
  rename("unit_id" = "id")

#add GUID to the list of observed_properties for units
observed_properties <- observed_properties %>% 
  left_join(units, 
            by = join_by('sample_unit' == 'customId'), 
            keep = FALSE)

#analysis.Type must be ALL CAPS
observed_properties$analysis_type <- toupper(observed_properties$analysis_type)
observed_properties$result_type <- toupper(observed_properties$result_type)

write_xlsx(observed_properties, "./utils/config/ReferenceLists/Consolidated_Observed_Properties.xlsx")

# Load an existing workbook
wb <- loadWorkbook("./utils/config/ReferenceLists/Consolidated_Observed_Properties.xlsx")

# Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
renameWorksheet(wb, sheet = "Sheet1", newName = "Observed_Properties")

# Save the workbook with the updated sheet name
saveWorkbook(wb, "./utils/config/ReferenceLists/Consolidated_Observed_Properties.xlsx", overwrite = TRUE)

#read observed properties
observed_properties <- read_excel("./utils/config/ReferenceLists/Consolidated_Observed_Properties.xlsx",
                                 sheet = "Observed_Properties")

observed_properties <- observed_properties %>% 
  dplyr::select(c("parm_code", "newnameid", "description", 
                  "analysis_type", "sample_unit_group", "sample_unit",
                  "cas")) %>% unique()

# # QA/QC for observed_properties -----------------------------------------------------------
# 
# # Checking which entries are not getting posted
# get_check <- get_profiles("prod", "observedproperties")
# 
# #not all observed_properties getting posted
# #compare get_check for observed_properties with raw observed_properties - 0 but 4172 in AQS 4285 in data frame
# observed_properties_not_posted <- observed_properties %>% anti_join(get_check,
#                                                    by = join_by("newnameid" == "customId"))
