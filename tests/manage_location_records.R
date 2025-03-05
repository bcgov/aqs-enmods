#Use this script to delete everything from a given location but keep the location
#record. This could be made into a function and used for loop through multiple locations
#there might be errors to do with attachments it hasn't been fully tested.

#this script is delete everyting from a given location

library(httr)
library(jsonlite)
library(tidyr)

#Get the api token and set the url
readRenviron(paste0(getwd(), "./.Renviron"))
token <- Sys.getenv("api_test_token")
base_url = 'https://bcenv-enmods-test.aqsamples.ca/api/'

#the custom ID of the location to remove everything from
loc_id <- "E328304"

source("./utils/config/delete_all_location_obs.R")

#delete records for a single location but keep the location record
delete_location_obs(base_url, loc_id, token)

#delete a location group
delete_location_group(base_url, token, "96")

#delete all location groups
delete_all_location_groups(base_url, token) 

#delete all saved filters
delete_all_saved_filters(base_url, token)

#delete all records including all locations
delete_all_records(base_url, token)

#add location groups
#Read permits from EMS and AMS, join to get company name for location group
#read the EMS permits
EMS_Permit_IDs <- read_excel("Permits_2024_10_31.xlsx")

#filter to only those locations that are assoicated with a permit
EMS_Permit_IDs <- EMS_Permit_IDs %>% filter(!is.na(`Permit ID`))

#unique list of permits in EMS, each of these will need it's own location group in EnMoDS
EMS_Permits <- EMS_Permit_IDs %>% select(`Permit ID`)
EMS_Permits <- unique(EMS_Permits) #3,540 permits in EMS

#read the AMS data
AMS_Permits <- read_excel("all_ams_authorizations.xlsx")

#select the needed columns
AMS_Permits <- AMS_Permits %>% select(`Authorization Number`, Company, `Facility Type - Description`, `Facility Address`)

#remove duplicates in AMS
AMS_Permits <- unique(AMS_Permits)

#join EMS with AMS dropping records that are only in the AMS side
joined_permits <- left_join(EMS_Permits, AMS_Permits, join_by(`Permit ID` == `Authorization Number`))

#Format this for Location Groups in EnMoDS
Location_Groups <- joined_permits

Location_Groups$Description = paste0("", Location_Groups$Company)

Location_Groups$Type = "Authorization"

#select just needed columns
Location_Groups <- Location_Groups %>% select(`Permit ID`, Type, Description)

## call function here!!

#add locations

#add saved filters
