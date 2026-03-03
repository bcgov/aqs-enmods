library(httr)
library(jsonlite)
library(readxl)
library(dplyr)
library(stringr)

#Example of working API calls
#get the API tokens from your environment file
readRenviron(paste0(getwd(), "./.Renviron"))
testToken <- Sys.getenv("TEST_TOKEN")
prodToken <- Sys.getenv("PROD_TOKEN")
testURL <- Sys.getenv("TEST_URL")
prodURL <- Sys.getenv("PROD_URL")

#Read the authorizations template from excel
auth <- read_excel("./data/All AMS Authorizations Feb_23_2026.xlsx")

#clean it up
auth <- auth %>% select('Authorization Number [customId]', 'Company [Name]', 
                        'Issue Date [Start Date]',
                        'Waste Type [comments]','State [Active or Suspended]',
                        'Facility Type - Description [comments]')

auth <- auth %>% filter('State [Active or Suspended]' = "Active")

#Post standards
for (i in seq(1, length(standards_ID))) {
  
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
                    'issuingOrganization' = 'Test Standards Import',
                    'samplingLocations' = list(list('id' = unique(standards_data$loc.id))), #only a single location ever
                    'active' = 'TRUE',
                    'observationStandards' = ops_list,
                    'applicabilityRange' = list('start' = unique(standards_data$`Applicable From (yyyy-mm-dd)`),
                                                'end' = unique(standards_data$`Applicable To (yyyy-mm-dd)`))
  )
  
  y<-POST(paste0(testURL, "v1/standards/"), config = c(add_headers(.headers = c('Authorization' = testToken ))), body = data_body, encode = 'json')
  
  print(fromJSON(rawToChar(y$content)))
  
  #if it already exists use PUT but test the customId first
  
} #end for loop of unique standards


### Code to delete all standards to post a fresh copy
x <- GET(paste0(testURL, "v1/standards/"), config = c(add_headers(.headers = c('Authorization' = testToken ))), body = list(), encode = 'json')
standards_guid <- fromJSON(rawToChar(x$content))$domainObjects$id

for (i in seq(1, length(standards_guid))) {
  DELETE(paste0(testURL, "v1/standards/", standards_guid[i]), config = c(add_headers(.headers = c('Authorization' = testToken ))), body = list(), encode = 'json')
}