# FILE TO PREPROCESS SAMPLING LOCATIONs and LOCATION GROUPS in that order
library(readxl)
library(dplyr)

#https://github.com/bcgov/nr-enmods-dar/blob/main/data%20conversion/locationExtractQueries.sql

# SAMPLING LOCATION GROUPS AND LOCATIONS ----
# PREPROCESSING SAMPLING LOCATIONS AND GROUPS FOR NEW DATA --------------------
non_zero_post_2006 <- read.csv("./utils/config/ReferenceLists/Sampling_Locations/location_after_2006_with_samples_20260116.csv")

zero_pre_2006_auth <- read.csv("./utils/config/ReferenceLists/Sampling_Locations/location_no_samples_pre_2006_active_and_suspended_20260116.csv")

zero_2006_2024_can <- read.csv("./utils/config/ReferenceLists/Sampling_Locations/location_created_2006-2024_no_samples_20260116.csv")

non_zero_post_2006 <- non_zero_post_2006 %>% 
  rename_with(tolower) %>% 
  rename_with(~ gsub("\\.", "_", .)) %>% 
  rename_with(~ gsub(" ", "_", .))

zero_pre_2006_auth <- zero_pre_2006_auth %>%
  rename_with(tolower) %>% 
  rename_with(~ gsub("\\.", "_", .)) %>% 
  rename_with(~ gsub(" ", "_", .))

zero_2006_2024_can <- zero_2006_2024_can %>% 
  rename_with(tolower) %>% 
  rename_with(~ gsub("\\.", "_", .)) %>% 
  rename_with(~ gsub(" ", "_", .))

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
  anti_join(zero_2006_2024_can, by = join_by(location_id))

#remove do not migrate
locations <- locations %>% dplyr::filter(type != "DO NOT MIGRATE")

#remove all NA from data frame!
locations <- locations %>% 
  mutate(across(where(is.character), ~ replace(., is.na(.), "")),
         across(where(is.numeric), ~ replace(., is.na(.), "")),
         across(where(is.logical), ~ replace(., is.na(.), "")),)

run_init <- TRUE

if(run_init){
ams_url <- "https://www2.gov.bc.ca/assets/gov/environment/waste-management/waste-discharge-authorization/datamart/all_ams_authorizations.xlsx"
download.file(ams_url, "all_ams_authorizations.xlsx", mode = "wb")
}

#read the AMS data to get location group info
#NEED TO OPEN THE FILE AND TURN OFF THE MACROS 
#ALSO NEED TO MAKE IT COMPATIBLE WITH LATEST EXL VRSN
ams_permits <- read_excel("all_ams_authorizations.xlsx") %>% 
  rename_with(tolower) %>% 
  rename_with(~ gsub("\\.", "_", .)) %>% 
  rename_with(~ gsub("\\-", "_", .)) %>%
  rename_with(~ gsub(" ", "_", .))

#storing relevant records of AMS Permits
ams_permits <- ams_permits %>% 
  dplyr::select(authorization_number, company, facility_type___description, 
                facility_address)

#remove duplicates in AMS
ams_permits <- unique(ams_permits)

#Get the list of permits from locations
ems_permits <- unique(locations$location_groups)
ems_permits <- ems_permits[ems_permits != ""]

ems_permits <- data.frame(permit_id = unlist(strsplit(ems_permits, ";"))) %>% 
                mutate(permit_id = as.numeric(permit_id)) %>% 
                unique()

#join EMS with AMS dropping records that are only in the AMS side
location_groups <- left_join(ems_permits, ams_permits, 
                            join_by(permit_id == authorization_number))

location_groups$description = paste0("", location_groups$company)

location_groups$type = "Authorization"

#make the groups
location_groups <- location_groups %>% dplyr::select(permit_id, type, description)

location_group_types <- get_profiles(env, "location_group_types")

#joing location group guid to location groups table
location_groups <- inner_join(location_groups, location_group_types, 
                             by = join_by(type == customId)) %>%
  rename(location_group_type_id = id)

# could not figure out locations completely
# generating split files that have to be loaded manually
# make the files smaller
locations <- locations %>% 
  mutate(elevation_unit = 
           case_when(#is.na(elevation_unit) ~ "metre", 
             elevation_unit == "metre" ~ "m",
             .default = elevation_unit)) %>%
  rename_with(
    ~ case_when(
      .x == "location_id" ~ "Location ID",
      .x == "name" ~ "Name",
      .x == "type" ~ "Type",
      .x == "comment" ~ "Comment",
      .x == "country" ~ "Country",
      .x == "state" ~ "State",
      .x == "county" ~ "County",
      .x == "latitude" ~ "Latitude",
      .x == "longitude" ~ "Longitude",
      .x == "horizontal_datum" ~ "Horizontal Datum",
      .x == "horizontal_collection_method" ~ "Horizontal Collection Method",
      .x == "vertical_datum" ~ "Vertical Datum",
      .x == "vertical_collection_method" ~ "Vertical Collection Method",
      .x == "location_groups" ~ "Location Groups",
      .x == "elevation" ~ "Elevation",
      .x == "elevation_unit" ~ "Elevation Unit",
      .x == "standards" ~ "Standards",
      .x == "ea_closed_date" ~ "EA_Closed Date",
      .x == "ea_ems_when_created" ~ "EA_EMS When Created",
      .x == "ea_ems_when_updated" ~ "EA_EMS When Updated",
      .x == "ea_ems_who_created" ~ "EA_EMS Who Created",
      .x == "ea_ems_who_updated" ~ "EA_EMS Who Updated",
      .x == "ea_established_date" ~ "EA_Established Date",
      .x == "ea_well_tag_id" ~ "EA_Well Tag ID",
      .default = .x
    )
  )

#Make the data-time format nicer'
locations_when_created <- as.POSIXct(locations$`EA_EMS When Created`, tz= 'America/Vancouver', '%m/%d/%Y %I:%M:%S %p')

#For this migration the cut off is 5:15 PST on January 15th 2026
#checked and last locations was made at 11:38 PST on 15th 2026 so no need to filter anything

#total count is 32,240
write.csv(locations, file = "./utils/config/ReferenceLists/Sampling_Locations/LocationsExtraction_202601151715.csv", row.names = F)

#total locations group is count is 3,065 public ams source data from Oct 28 2025
write.csv(location_groups, "./utils/config/ReferenceLists/Sampling_Locations/LocationsGroups_202601151715.csv", row.names = F)

#locations have to be pushed manually, 10000 at a time using AQS Import
locations_1 <- locations[seq(1,10000),]
write.csv(locations_1, file = "./utils/config/ReferenceLists/Sampling_Locations/1_Locations_Extract_Jan8_2026.csv", row.names = F)

locations_2 <- locations[seq(10001,20000),]
write.csv(locations_2, file = "./utils/config/ReferenceLists/Sampling_Locations/2_Locations_Extract_Jan8_2026.csv", row.names = F)

locations_3 <- locations[seq(20001,30000),]
write.csv(locations_3, file = "./utils/config/ReferenceLists/Sampling_Locations/3_Locations_Extract_Jan8_2026.csv", row.names = F)

locations_4 <- locations[seq(30001, nrow(locations)),]
write.csv(locations_4, file = "./utils/config/ReferenceLists/Sampling_Locations/4_Locations_Extract_Jan8_2026.csv", row.names = F)



