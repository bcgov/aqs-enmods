library(httr)
library(jsonlite)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

#Example of working API calls
#get the API tokens from your environment file
readRenviron(paste0(getwd(), "./.Renviron"))
#testToken <- Sys.getenv("TEST_TOKEN")
prodToken <- Sys.getenv("PROD_TOKEN")
#testURL <- Sys.getenv("TEST_URL")
prodURL <- Sys.getenv("PROD_URL")

#Read the standards template from excel
#standards <- read_excel("./utils/config/standards/2025_07_08_EnMoDS_Standards_Template_pilot.xlsx")
standards <- read_excel("./data/2026-03-13_standards.xlsx")

#need units and OPs guids from AQS
env = "prod"

#define token and urls
update_base_url_token <- function(env) {
  if (env == "prod") list(prodURL, prodToken) else list(testURL, testToken)
}

source("./utils/config/api_functions.R")

#get reference lists from the system
units <- get_profiles(env, "units")
units$groupId <- units$unitGroup$id
units <- units %>% select("id", "customId", "groupId","name")
units <- units %>% separate('name', into = c("Unit_Code", "Unit_Name"), sep = " - ")
OPs <- get_profiles(env, "observed_properties") %>% select ("id", "customId")
locs <- get_profiles(env, "locations") %>% select ("id", "customId")

#join units in
standards <- merge(standards, units, by.x = "Units", by.y = "Unit_Code", all.x = T, all.y = F)
standards <- standards %>% rename("unit.id" = "id")

#join OP in
standards <- merge(standards, OPs, by.x = "Observed Property (Parameter)", by.y = "customId", all.x = T, all.y = F)
standards <- standards %>% rename("OP.id" = "id")

#join locations in
standards <- merge(standards, locs, by.x = "Location ID", by.y = "customId", all.x = T, all.y = F)
standards <- standards %>% rename("loc.id" = "id")

#add a check to remove standards that do not have unit id or OP id
standards <- standards %>% filter(!is.na(OP.id), !is.na(unit.id), !is.na(loc.id))


#Add applicable from date
standards$`Applicable From (yyyy-mm-dd)`[!is.na(standards$`Applicable From (yyyy-mm-dd)`)] <-
  paste0(standards$`Applicable From (yyyy-mm-dd)`[!is.na(standards$`Applicable From (yyyy-mm-dd)`)],
         "T00:00:00-08:00")
standards$`Applicable From (yyyy-mm-dd)` <- as.character(standards$`Applicable From (yyyy-mm-dd)`)
standards$`Applicable From (yyyy-mm-dd)`[is.na(standards$`Applicable From (yyyy-mm-dd)`)] <- ''


#Add applicable to
standards$`Applicable To (yyyy-mm-dd)` <- as.character(standards$`Applicable To (yyyy-mm-dd)`)
standards$`Applicable To (yyyy-mm-dd)`[!is.na(standards$`Applicable To (yyyy-mm-dd)`)] <-
  paste0(standards$`Applicable To (yyyy-mm-dd)`[!is.na(standards$`Applicable To (yyyy-mm-dd)`)],
         "T00:00:00-08:00")
standards$`Applicable To (yyyy-mm-dd)`[is.na(standards$`Applicable To (yyyy-mm-dd)`)] <- ''

#standards apply the same list of OPs to every location but because the data from
#CEEB has different lists of OPs per location we need to make a standard per location per date window
make_date_string <- function(date_from, date_to) {
  ifelse(
    date_to == "",
    paste0(as.Date(date_from)),
    paste0(as.Date(date_from), "_to_", as.Date(date_to))
  )
}

standards$newName <- make_date_string(standards$`Applicable From (yyyy-mm-dd)`, standards$`Applicable To (yyyy-mm-dd)`)

standards$newName <- paste0(standards$`Auth #`, "_", standards$`Location ID`, 
                            "_", standards$newName)

#get list of standards
standards_ID <- unique(standards$newName)

#add a check for locations with lots of obs
location_summary <- read.csv("https://coms.api.gov.bc.ca/api/v1/object/e4e1829d-c1a1-4932-b275-de6e423a6d71")


standars_obs_check <- merge(standards, location_summary, by.x = 'Location ID', by.y = 'ID')
#Post standards
for (i in seq(1, length(standards_ID))) {
  #i=10 #1, 2, 7, 8, 9 and 10 are done
  standards_data <- standards %>% filter(newName == standards_ID[i])
  
  locations <- unique(standards_data$loc.id)
  
  #define the nested list for observed properties, values and units
  ops_list <- list()
  for (j in seq(1, nrow(standards_data))) {
    
    #check for lower limits
    if (is.na(standards_data$Minimum[j])) { #no lower limit
      ops_list[j] <- list(list(
        'observedProperty' = list('id' = standards_data$OP.id[j]), 
        'resultUpperLimit' = list('value' = standards_data$Maximum[j], 'unit' =
                                    list('id' = standards_data$unit.id[j], 'unitGroup' =
                                           list('id' = standards_data$groupId[j]))),
        'ruleText' = standards_data$`Warning Message`[j]
      ))
    } else { #has lower limit
      ops_list[j] <- list(list(
        'observedProperty' = list('id' = standards_data$OP.id[j]), 
        'resultLowerLimit' = list('value' = standards_data$Minimum[j], 'unit' = 
                                    list('id' = standards_data$unit.id[j], 'unitGroup' =
                                         list('id' = standards_data$groupId[j]))), #lower limit
        
        'resultUpperLimit' = list('value' = standards_data$Maximum[j], 'unit' = 
                                    list('id' = standards_data$unit.id[j], 'unitGroup' =
                                           list('id' = standards_data$groupId[j]))), #upper limit
        'ruleText' = standards_data$`Warning Message`[j]
        ))
      }
    
  }
  
  data_body <- list('customId' = standards_ID[i],
                    'name' = unique(standards_data$Name),
                    'description' = unique(standards_data$Description),
                    'issuingOrganization' = 'BC Ministry of Environment and Parks',
                    'samplingLocations' = list(list('id' = unique(standards_data$loc.id))), #only a single location ever
                    'active' = 'TRUE',
                    'observationStandards' = ops_list,
                    'applicabilityRange' = list('start' = unique(standards_data$`Applicable From (yyyy-mm-dd)`),
                                                'end' = unique(standards_data$`Applicable To (yyyy-mm-dd)`))
  )
  
  y<-POST(paste0(prodURL, "v1/standards/"), config = c(add_headers(.headers = c('Authorization' = prodToken ))), body = data_body, encode = 'json')
  
  print(fromJSON(rawToChar(y$content)))
  
  #if it already exists use PUT but test the customId first
  Sys.sleep(60*10)
  
} #end for loop of unique standards

#Get standards already in the prod
x <- GET(paste0(prodURL, "v1/standards/"), config = c(add_headers(.headers = c('Authorization' = prodToken ))), body = list(), encode = 'json')
standards_guid <- fromJSON(rawToChar(x$content))$domainObjects

#check for those that are already in prod
standards_not_in_prod <- standards_ID[!(standards_ID %in% standards_guid$customId)]

### Code to delete all standards to post a fresh copy
x <- GET(paste0(prodURL, "v1/standards/"), config = c(add_headers(.headers = c('Authorization' = prodToken ))), body = list(), encode = 'json')
standards_guid <- fromJSON(rawToChar(x$content))$domainObjects$id

for (i in seq(1, length(standards_guid))) {
  DELETE(paste0(prodURL, "v1/standards/", standards_guid[i]), config = c(add_headers(.headers = c('Authorization' = prodToken ))), body = list(), encode = 'json')
  Sys.sleep(60*5)
  print(i)
}