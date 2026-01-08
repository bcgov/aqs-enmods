#Use this script to get locations made since x date and time


library(httr)
library(jsonlite)
library(dplyr)
library(stringr)
library(tidyr)
library(aws.s3)

#readRenviron(paste0(getwd(), "./.Renviron"))
testToken <- Sys.getenv("TEST_TOKEN")
#prodToken <- Sys.getenv("PROD_READ_ONLY_TOKEN")
testURL <- Sys.getenv("TEST_URL")
#prodURL <- Sys.getenv("PROD_URL")

locationsMadeAfter <- "2025-12-01T00:01:00.000-08:00"

#Set up connection to BC box
Sys.setenv("AWS_ACCESS_KEY_ID" =  Sys.getenv("AWS_ACCESS_KEY"),
           "AWS_SECRET_ACCESS_KEY" =  Sys.getenv("AWS_SECRET_ACCESS_KEY"),
           "AWS_S3_ENDPOINT" = "nrs.objectstore.gov.bc.ca",
           "AWS_DEFAULT_REGION" = "")

#---Get all Locations from Export---
#Because locations is a big load we read locations from object store
#TEST version here
all_locations <- read.csv('https://coms.api.gov.bc.ca/api/v1/object/6fc7cb4c-dabf-4c41-bb69-f97045a1ed35?download=proxy')

#debug
class(all_locations)

#clean up the data types for dates
all_locations$ESTABLISHED_DATE <- as.Date(all_locations$ESTABLISHED_DATE)
all_locations$LATEST_FIELD_VISIT <- as.Date(all_locations$LATEST_FIELD_VISIT)

#filter to just those locations sampled in the last 5 years or made in the last 3 years
#remove air stations since those have their own list
locations <- all_locations %>% filter(ESTABLISHED_DATE > (as.Date(Sys.time()) - 365*3) | 
                                        LATEST_FIELD_VISIT > (as.Date(Sys.time()) - 365*5))
locations <- locations %>% filter(TYPE != 'Air Quality')


#Make the json file for the CoC form using the minimum info just ID and name
jsonLocationsRaw <- locations %>%
  dplyr::select(ID, NAME) %>%
  rename(disp_name = NAME) %>%
  mutate(disp_name = str_c(ID, " - ", disp_name)) %>%
  dplyr::select(disp_name)

jsonLocationsRaw  <- rbind(jsonLocationsRaw, "Other - Enter below")

#--- add new locations made since x date
locs <- GET(paste0(testURL, "v1/samplinglocations?startModificationTime=", locationsMadeAfter), config = c(add_headers(.headers = c('Authorization' = testToken ))), body = list(), encode = 'json')
locs <-fromJSON(rawToChar(locs$content))$domainObjects

locs <- locs %>% select(customId, name)
locs$disp_name <- paste0(locs$customId, " - ", locs$name)
locs <- locs %>% select(disp_name)

#bind the new stuff with the older locations
jsonLocationsRaw  <- rbind(jsonLocationsRaw, locs)

#convert to json
jsonLocationsProc <- toJSON(list(items = jsonLocationsRaw), pretty = TRUE)

#put the json file in the bc bucket
write(jsonLocationsProc, file = "not_air_enmods_locations_data.json")

put_object(file = "not_air_enmods_locations_data.json", 
           object = "CoC_Tables/not_air_enmods_locations_data.json",
           bucket = "enmods",
           region = "",
           acl = "public-read")

#for air the list is so short display all air locations no date filtering needed
locations <- all_locations %>% filter(TYPE == 'Air Quality')


#Make the json file for the CoC form using the minimum info just ID and name
jsonLocationsRaw <- locations %>%
  dplyr::select(ID, NAME) %>%
  rename(disp_name = NAME) %>%
  mutate(disp_name = str_c(ID, " - ", disp_name)) %>%
  dplyr::select(disp_name)

jsonLocationsRaw  <- rbind(jsonLocationsRaw, "Other - Enter below")

#bind the new stuff with the older locations
jsonLocationsRaw  <- rbind(jsonLocationsRaw, locs)

#make into a json
jsonLocationsProc <- toJSON(list(items = jsonLocationsRaw), pretty = TRUE)

#put the json file in the bc bucket
write(jsonLocationsProc, file = "air_enmods_locations_data.json")

put_object(file = "air_enmods_locations_data.json", 
           object = "CoC_Tables/air_enmods_locations_data.json",
           bucket = "enmods",
           region = "",
           acl = "public-read")


#---New Locations---
if (FALSE) {
  locs <- GET(paste0(testURL, "v1/samplinglocations?startModificationTime=", locationsMadeAfter), config = c(add_headers(.headers = c('Authorization' = testToken ))), body = list(), encode = 'json')
  locs <-fromJSON(rawToChar(locs$content))$domainObjects
  
  locs <- locs %>% select(customId, name)
  locs$disp_name <- paste0(locs$customId, " - ", locs$name)
  locs <- locs %>% select(disp_name)
  
  #make nice json to copy and paste into the CoC json
  jsonLoc <- toJSON(list(items = locs), pretty = TRUE)
  write(jsonLoc, file = "enmods_new_locations.json")
}
