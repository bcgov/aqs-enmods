# FILE TO PREPROCESS SAMPLING LOCATION TYPES and LOCATIONS in that order

# PREPROCESSING TO GENERATE OLDER LOCATION GROUP TYPES FILES ------------

run_init <- FALSE
if(run_init){
#Had to run this only once in a lifetime
location_group_types <- get_profiles("test", "location_group_types") %>%
  dplyr::select(customId)

# Save workbook
write_xlsx(list(location_group_types), "./utils/config/ReferenceLists/Location_Group_Types.xlsx")

# Load an existing workbook
wb <- loadWorkbook("./utils/config/ReferenceLists/Location_Group_Types.xlsx")

# Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
renameWorksheet(wb, sheet = "Sheet1", newName = "Location_Group_Types")

# Save the workbook with the updated sheet name
saveWorkbook(wb, "./utils/config/ReferenceLists/Location_Group_Types.xlsx", 
             overwrite = TRUE)
}

# PREPROCESSING LOCATION GROUP TYPES FOR NEW DATA -------------------------

location_group_types <- read_excel("./utils/config/ReferenceLists/location_group_types.xlsx", 
                                 sheet = "Location_Group_Types") 
# %>% 
#   rename_with(tolower) %>%
#   rename_with(~ gsub("\\.", "_", .))

# LOCATION TYPES ---------------------------------------------------------
location_types <- read_excel("./utils/config/ReferenceLists/location_types.xlsx", 
                            sheet = "Location_Types")

location_types <- location_types %>% 
  mutate(customid = case_when(
    customid == "Land - Fram" ~ "Land - Farm",
    .default = customid
  ))

# SAMPLING LOCATION GROUPS AND LOCATIONS-------------------------------------------
# PREPROCESSING SAMPLING LOCATIONS AND GROUPS FOR NEW DATA --------------------
non_zero_post_2006 <- read_excel("./utils/config/ReferenceLists/samplingLocations/March5_2025NonzeroSamplesAfter2006Export.xlsx")
zero_pre_2006_auth <- read_excel("./utils/config/ReferenceLists/samplingLocations/March5_2025_ZeroSamplesBefore2006ActiveSuspended.xlsx")
zero_2006_2024_can <- read_excel("./utils/config/ReferenceLists/samplingLocations/March5_2025_Between2006And2024ZeroSamples.xlsx")

#Check for identical column names
if(identical(names(zero_pre_2006_auth), names(non_zero_post_2006))){
  
  if(identical(names(zero_2006_2024_can), names(non_zero_post_2006))){
    
    print("The column names are SAME across the location files")
    
  } else {
    
    print("The column names are DIFFERENT across the location files")
    
  }
  
} else {
  
  print("The column names are DIFFERENT across the location files")
  
}

#join together the ones with actual data that needs to be included
locations <- rbind(non_zero_post_2006, zero_pre_2006_auth)

#remove those locations that were cancelled and never sampled
locations <- locations %>% 
  anti_join(zero_2006_2024_can, by = join_by("Location ID"))

#remove do not migrate
locations <- locations %>% dplyr::filter(Type != "DO NOT MIGRATE")

#remove all NA from data frame!
locations <- locations %>% 
  mutate(across(where(is.character), ~ replace(., is.na(.), "")),
         across(where(is.numeric), ~ replace(., is.na(.), "")),
         across(where(is.logical), ~ replace(., is.na(.), "")),)

run_init <- FALSE

if(run_init){
ams_url <- "https://www2.gov.bc.ca/assets/gov/environment/waste-management/waste-discharge-authorization/datamart/all_ams_authorizations.xlsx"
download.file(ams_url, "all_ams_authorizations.xlsx", mode = "wb")
}

#read the AMS data to get location group info
amsPermits <- read_excel("all_ams_authorizations.xlsx")

#storing relevant records of AMS Permits
amsPermits <- amsPermits %>% 
  dplyr::select(`Authorization Number`, Company, `Facility Type - Description`, `Facility Address`)

#remove duplicates in AMS
amsPermits <- unique(amsPermits)

#Get the list of permits from locations
emsPermits <- unique(locations$`Location Groups`)
emsPermits <- emsPermits[emsPermits != ""]

emsPermits <- data.frame("Permit ID" = unlist(strsplit(emsPermits, ";"))) %>% unique()
colnames(emsPermits) <- "Permit ID"
emsPermits$`Permit ID` <- as.numeric(emsPermits$`Permit ID`)
emsPermits <- emsPermits %>% unique()

#join EMS with AMS dropping records that are only in the AMS side
locationGroups <- left_join(emsPermits, amsPermits, 
                            join_by(`Permit ID` == `Authorization Number`))

locationGroups$Description = paste0("", locationGroups$Company)

locationGroups$Type = "Authorization"

#make the groups
locationGroups <- locationGroups %>% dplyr::select(`Permit ID`, Type, Description)

location_group_types <- get_profiles(env, "location_group_types")

#joing location group guid to location groups table
locationGroups <- inner_join(locationGroups, location_group_types, 
                             by = join_by(Type == customId)) %>%
  rename(locationgrouptypeID = id)

# could not figure out locations completely
# generating split files that have to be loaded manually
# make the files smaller
locations <- locations %>% 
  mutate(`Elevation Unit` = 
           case_when(#is.na(`Elevation Unit`) ~ "metre", 
             `Elevation Unit` == "metre" ~ "m",
             .default = `Elevation Unit`))

#locations have to be pushed manually, 10000 at a time using AQS Import
locations_1 <- locations[seq(1,10000),]
write.csv(locations_1, file = "./utils/config/ReferenceLists/samplingLocations/1_Locations_Extract_May6_2025.csv", row.names = F)

locations_2 <- locations[seq(10001,20000),]
write.csv(locations_2, file = "./utils/config/ReferenceLists/samplingLocations/2_Locations_Extract_May6_2025.csv", row.names = F)

locations_3 <- locations[seq(20001,30000),]
write.csv(locations_3, file = "./utils/config/ReferenceLists/samplingLocations/3_Locations_Extract_May6_2025.csv", row.names = F)

locations_4 <- locations[seq(30001, nrow(locations)),]
write.csv(locations_4, file = "./utils/config/ReferenceLists/samplingLocations/4_Locations_Extract_May6_2025.csv", row.names = F)



# SAVED FILTERS ----
# PREPROCESSING TO GENERATE OLDER SAVED FILTERS FILES ---------------

run_init = TRUE
if(run_init){
  #Had to run this only once in a lifetime
  saved_filters <- get_profiles("test", "filters") #%>%
  #dplyr::select(customId)
  
  # Save workbook
  write_xlsx(saved_filters, "./utils/config/ReferenceLists/Saved_Filters.xlsx")
  
  # Load an existing workbook
  wb <- loadWorkbook("./utils/config/ReferenceLists/Saved_Filters.xlsx")
  
  # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
  renameWorksheet(wb, sheet = "Sheet1", newName = "Saved_Filters")
  
  # Save the workbook with the updated sheet name
  saveWorkbook(wb, "./utils/config/ReferenceLists/Saved_Filters.xlsx", overwrite = TRUE)
}

# PREPROCESSING SAVED FILTERS FOR NEW DATA ---------------------------

saved_filters <- read_excel("./utils/config/ReferenceLists/Saved_Filters.xlsx", 
                            sheet = "Saved_Filters") %>% rename_with(tolower) %>% 
  rename_with(~ gsub("\\.", "_", .)) %>% 
  rename_with(~ gsub(" ", "_", .)) %>%
  rename(observed_properties = observedproperties,
         sampling_locations = samplinglocations)

