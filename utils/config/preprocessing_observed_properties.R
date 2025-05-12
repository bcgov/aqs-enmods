# FILE TO PREPROCESS OBSERVED PROPERTIES
source("./utils/config/api_functions.R")

# PREPROCESSING TO CONSOLIDATE OLDER OP FILES -----------------------------

run_init <- FALSE
if (run_init) {

  #EMS exported observedProperties
  observedProperties <- read_excel("./utils/config/ReferenceLists/Observed_Properties.xlsx")
  
  #The units don't matter for non-convertable OP units. For example microbial units. So remove them from consideration.
  observedProperties$Sample.Unit[observedProperties$Convertable.In.Samples == "N"] <- ""
  
  #clean up modifers
  observedProperties$Modifier[is.na(observedProperties$Modifier)] <- ""
  
  #Add an empty Result.Type column
  observedProperties$Result.Type <- ""
  
  #Keep the core columns at the front
  cols_to_move <- c("NewNameID", "Parm.Code", "Description",
                    "Analysis.Type", "Result.Type", "Sample.Unit.Group",
                    "Sample.Unit","CAS")
  
  #get the unique list of OP IDs
  observedProperties <- observedProperties %>% unique()
  
  
  ## observedProperties new to EnMoDS not in EMS
  OPs_new_to_EnMoDS <-
    read_excel("./utils/config/ReferenceLists/New_OPS_not_in_EMS.xlsx",
               sheet = "OPs_new")
  
  #get the unique list of OP IDs
  OPs_new_to_EnMoDS <- OPs_new_to_EnMoDS %>% unique()
  
  #fixing the missing sample group id issue in the list
  OPs_new_to_EnMoDS <- OPs_new_to_EnMoDS %>% mutate(Sample.Unit.Group =
                                                      if_else(NewNameID == "Biological Sample Volume (vol.)",
                                                              "Volume", Sample.Unit.Group))
  
  #Identify the new columns compared to observedProperties
  new_cols <- setdiff(names(observedProperties), names(OPs_new_to_EnMoDS))
  
  # Create a tibble of just those new columns
  new_data <- observedProperties %>%
    dplyr::select(all_of(new_cols)) %>%
    mutate(across(everything(), ~ NA)) %>%
    unique() %>% slice(rep(1, nrow(OPs_new_to_EnMoDS)))
  
  OPs_new_to_EnMoDS <- OPs_new_to_EnMoDS %>% bind_cols(new_data)
  
  observedProperties <- observedProperties %>% bind_rows(OPs_new_to_EnMoDS %>% mutate(Results = NA)) %>% unique()
  
  ## Taxonomic observedProperties
  OPs_taxonomic <- read.csv("./utils/config/ReferenceLists/Taxonomic_OP.csv", stringsAsFactors = F) #1607
  
  #get the unique list of OP IDs
  OPs_taxonomic <- OPs_taxonomic %>% unique()
  
  #Identify the new columns compared to observedProperties
  new_cols <- setdiff(names(observedProperties), names(OPs_taxonomic))
  
  # Create a tibble of just those new columns
  new_data <- observedProperties %>%
    dplyr::select(all_of(new_cols)) %>%
    mutate(across(everything(), ~ NA)) %>%
    unique() %>% slice(rep(1, nrow(OPs_taxonomic)))
  
  OPs_taxonomic <- OPs_taxonomic %>% bind_cols(new_data)
  
  observedProperties <- observedProperties %>% bind_rows(OPs_taxonomic %>% mutate(Results = NA)) %>% unique()
  
  ## Merge all observedProperties and process
  
  observedProperties <- observedProperties %>%
    #bind_rows(observedProperties, OPs_new_to_EnMoDS, OPs_taxonomic) %>%
    #        unique() %>%
    group_by(NewNameID) %>%
    mutate(Count = n()) %>%
    ungroup()
  
  #the total number of unique NewNameID should be the same as the total number of unique rows
  newNameIDUnique <- observedProperties$NewNameID %>% unique()
  
  observedProperties <- observedProperties %>%
    mutate(Sample.Unit = case_when(
      Sample.Unit == "pH Units" ~ "pH units",
      #Sample.Unit == NA ~ Unit,
      #Sample.Unit == "" ~ NA,
      .default = Sample.Unit),
    )
  
  #rename the required observedProperties so they show up otherwise they are in the background?
  observedProperties <- observedProperties %>%
    mutate(Sample.Unit.Group = case_when(
      NewNameID == "Biological Sex (cat.)" ~ "None",
      NewNameID == "Biological Life Stage (cat.)" ~ "None",
      Sample.Unit.Group == "Length" ~ "SYS-REQUIRED - Length",
      Sample.Unit.Group == "Apperance"~ "Appearance",
      .default = Sample.Unit.Group
    )) %>%
    mutate(Analysis.Type = case_when(
      NewNameID == "Biological Sex (cat.)" ~ "BIOLOGICAL",
      NewNameID == "Biological Life Stage (cat.)" ~ "BIOLOGICAL",
      .default = Analysis.Type
    ))
  
  #need to get unit group and unit IDs prior to importing observedProperties
  unitGroups <- get_profiles("prod", "unitgroups") %>%
    dplyr::select(id, customId, supportsConversion) %>%
    rename("Unit.Group.Id" = "id")
  
  #they are not!
  #current pipeline will upload the first OP into the system
  #But its fine because...
  #they belong to unit groups that are convertible
  #Below code helps you make that check
  OPs_convertible <- observedProperties %>%
    dplyr::filter(Count>1) %>%
    left_join(unitGroups %>%
                dplyr::select(customId, Unit.Group.Id, supportsConversion),
              by = join_by("Sample.Unit.Group" == "customId")) %>%
    dplyr::filter(supportsConversion == FALSE)
  
  # OPs_missing_unitgroup <- observedProperties %>%
  #   dplyr::filter(is.na(Unit.Group.Id))
  
  #add GUID to the list of observedProperties for unit groups
  observedProperties <- left_join(observedProperties, unitGroups,
                                  by = join_by('Sample.Unit.Group' == 'customId'), keep = FALSE)
  
  #units without groups
  units <- get_profiles("prod", "units") %>%
    dplyr::select(id, customId) %>%
    rename("Unit.Id" = "id")
  
  #add GUID to the list of observedProperties for units
  observedProperties <- left_join(observedProperties, units,
                                  by = join_by('Sample.Unit' == 'customId'), keep = FALSE)
  
  #analysis.Type must be ALL CAPS
  observedProperties$Analysis.Type <- toupper(observedProperties$Analysis.Type)
  observedProperties$Result.Type <- toupper(observedProperties$Result.Type)
  
  write_xlsx(observedProperties, "./utils/config/ReferenceLists/consolidatedObservedProperties.xlsx")
  
  # Load an existing workbook
  wb <- loadWorkbook("./utils/config/ReferenceLists/consolidatedObservedProperties.xlsx")
  
  # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
  renameWorksheet(wb, sheet = "Sheet1", newName = "ObservedProperties")
  
  # Save the workbook with the updated sheet name
  saveWorkbook(wb, "./utils/config/ReferenceLists/consolidatedObservedProperties.xlsx", overwrite = TRUE)
  
  observedProperties <- observedProperties %>% dplyr::select(c("Parm.Code", "NewNameID", "Description", "Analysis.Type", "Result.Type", "Sample.Unit.Group", "Sample.Unit","CAS")) %>% unique()
  
}

# PREPROCESSING OP FOR NEW DATA --------------------------------------

observedProperties_base <- read_excel("./utils/config/ReferenceLists/consolidatedObservedProperties.xlsx")

#The units don't matter for non-convertable OP units. For example microbial units. So remove them from consideration.
observedProperties_base$Sample.Unit[observedProperties_base$Convertable.In.Samples == "N"] <- ""

#clean up modifers
observedProperties_base$Modifier[is.na(observedProperties_base$Modifier)] <- ""

#Add an empty Result.Type column
observedProperties_base$Result.Type <- ""

#Unit and unit groups may have been updated; remove their IDs
observedProperties_base <- observedProperties_base %>%
  dplyr::select(-c(Unit.Id, Unit.Group.Id, supportsConversion))

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

observedProperties_new <- read_excel(latest_file)

#The units don't matter for non-convertable OP units. For example microbial units. So remove them from consideration.
observedProperties_new$Sample.Unit[observedProperties_new$Convertable.In.Samples == "N"] <- ""

#clean up modifers
observedProperties_new$Modifier[is.na(observedProperties_new$Modifier)] <- ""

#Add an empty Result.Type column
observedProperties_new$Result.Type <- ""

#Identify the new columns compared to observedProperties
new_cols <- setdiff(names(observedProperties_base), names(observedProperties_new))

# Create a tibble of just those new columns
new_data <- observedProperties_base %>%
  dplyr::select(all_of(new_cols)) %>%
  mutate(across(everything(), ~ NA)) %>%
  unique() %>% slice(rep(1, nrow(observedProperties_new)))

observedProperties_new <- observedProperties_new %>% bind_cols(new_data)

observedProperties <- observedProperties_base %>% 
  bind_rows(observedProperties_new %>% mutate(Results = NA)) %>% unique()

## Merge all observedProperties
observedProperties <- observedProperties %>%
  #bind_rows(observedProperties, OPs_new_to_EnMoDS, OPs_taxonomic) %>%
  #        unique() %>% 
  group_by(NewNameID) %>% 
  mutate(Count = n()) %>%
  ungroup()

#the total number of unique NewNameID should be the same as the total number of unique rows
newNameIDUnique <- observedProperties$NewNameID %>% unique()

observedProperties <- observedProperties %>% 
  mutate(Sample.Unit = case_when(
    Sample.Unit == "pH Units" ~ "pH units",
    #Sample.Unit == NA ~ Unit,
    #Sample.Unit == "" ~ NA,
    .default = Sample.Unit),
  )

#rename the required observedProperties so they show up otherwise they are in the background?
observedProperties <- observedProperties %>% 
  mutate(Sample.Unit.Group = case_when(
    NewNameID == "Biological Sex (cat.)" ~ "None",
    NewNameID == "Biological Life Stage (cat.)" ~ "None",
    Sample.Unit.Group == "Length" ~ "SYS-REQUIRED - Length",
    Sample.Unit.Group == "Apperance"~ "Appearance",
    .default = Sample.Unit.Group
  )) %>% 
  mutate(Analysis.Type = case_when(
    NewNameID == "Biological Sex (cat.)" ~ "BIOLOGICAL",
    NewNameID == "Biological Life Stage (cat.)" ~ "BIOLOGICAL",
    .default = Analysis.Type
  ))

} else {
  
  #Unit and unit groups may have been updated; remove their IDs
  observedProperties <- observedProperties_base
  
}

#need to get unit group and unit IDs prior to importing observedProperties
unitGroups <- get_profiles("prod", "unitgroups") %>% 
  dplyr::select(id, customId, supportsConversion) %>%
  rename("Unit.Group.Id" = "id")

#they are not! 
#current pipeline will upload the first OP into the system
#But its fine because...
#they belong to unit groups that are convertible
#Below code helps you make that check
OPs_convertible <- observedProperties %>% 
  dplyr::filter(Count>1) %>%
  left_join(unitGroups %>% 
              dplyr::select(customId, Unit.Group.Id, supportsConversion), 
            by = join_by("Sample.Unit.Group" == "customId")) %>%
  dplyr::filter(supportsConversion == FALSE)

#add GUID to the list of observedProperties for unit groups
observedProperties <- left_join(observedProperties, unitGroups, 
                                by = join_by('Sample.Unit.Group' == 'customId'), keep = FALSE)

OPs_missing_unitgroup <- observedProperties %>% dplyr::filter(is.na(Unit.Group.Id))

#units without groups
units <- get_profiles("prod", "units") %>%
  dplyr::select(id, customId) %>%
  rename("Unit.Id" = "id")

#add GUID to the list of observedProperties for units
observedProperties <- observedProperties %>% 
  left_join(units, 
            by = join_by('Sample.Unit' == 'customId'), 
            keep = FALSE)

#analysis.Type must be ALL CAPS
observedProperties$Analysis.Type <- toupper(observedProperties$Analysis.Type)
observedProperties$Result.Type <- toupper(observedProperties$Result.Type)

write_xlsx(observedProperties, "./utils/config/ReferenceLists/consolidatedObservedProperties.xlsx")

# Load an existing workbook
wb <- loadWorkbook("./utils/config/ReferenceLists/consolidatedObservedProperties.xlsx")

# Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
renameWorksheet(wb, sheet = "Sheet1", newName = "ObservedProperties")

# Save the workbook with the updated sheet name
saveWorkbook(wb, "./utils/config/ReferenceLists/consolidatedObservedProperties.xlsx", overwrite = TRUE)

#read observed properties
observedProperties <- read_excel("./utils/config/ReferenceLists/consolidatedObservedProperties.xlsx",
                                 sheet = "ObservedProperties")

observedProperties <- observedProperties %>% dplyr::select(c("Parm.Code", "NewNameID", "Description", "Analysis.Type", "Sample.Unit.Group", "Sample.Unit","CAS")) %>% unique()

# # QA/QC for OPs -----------------------------------------------------------
# 
# # Checking which entries are not getting posted
# get_check <- get_profiles("prod", "observedproperties")
# 
# #not all observedProperties getting posted
# #compare get_check for observedProperties with raw observedProperties - 0 but 4172 in AQS 4285 in data frame
# OPs_not_posted <- observedProperties %>% anti_join(get_check,
#                                                    by = join_by("NewNameID" == "customId"))
