upload_standards <- function(env){

# library(httr)
# library(jsonlite)
# library(readxl)
# library(dplyr)
# library(stringr)
#
# #Example of working API calls
# #get the API tokens from your environment file
# readRenviron(paste0(getwd(), "./.Renviron"))
# testToken <- Sys.getenv("TEST_TOKEN")
# prodToken <- Sys.getenv("PROD_TOKEN")
# testURL <- Sys.getenv("TEST_URL")
# prodURL <- Sys.getenv("PROD_URL")
#
# #Read the standards template from excel
# standards <- read_excel("./utils/config/standards/EnMoDS_Standards_Template_pilot.xlsx")
#
# #need units and OPs guids from AQS
# env = "test"
#
# #define token and urls
# update_baseURL_token <- function(env) {
#   if (env == "prod") list(prodURL, prodToken) else list(testURL, testToken)
# }
#
# source("./utils/config/api_functions.R")
#
# #get reference lists from the system
# units <- get_profiles(env, "units")
# units$groupId <- units$unitGroup$id
# untits <- units %>% select("id", "customId", "groupId")
# OPs <- get_profiles(env, "observedproperties") %>% select ("id", "customId")
# locs <- get_profiles(env, "locations") %>% select ("id", "customId")
#
# #join units in
# standards <- merge(standards, units, by.x = "Units", by.y = "customId", all.x = T, all.y = F)
# standards <- standards %>% rename("unit.id" = "id")
#
# #join OP in
# standards <- merge(standards, OPs, by.x = "Observed Property (Parameter)", by.y = "customId", all.x = T, all.y = F)
# standards <- standards %>% rename("OP.id" = "id")
#
# #join locations in
# standards <- merge(standards, locs, by.x = "Location ID", by.y = "customId", all.x = T, all.y = F)
# standards <- standards %>% rename("loc.id" = "id")
#
# #add a check to remove standards that do not have unit id or OP id
# standards <- standards %>% filter(!is.na(OP.id), !is.na(unit.id), !is.na(loc.id))
#
# #standards apply the same list of OPs to every location but becaues the data from
# #CEEB has different lists of OPs per location we need to make a standard per location
#
# standards$newName <- paste0(standards$`Auth #`, "-", standards$`Location ID`)
#
# #get list of standards
# standards_ID <- unique(standards$newName)
#
# for (i in seq(1, length(standards_ID))) {
#
#   standards_data <- standards %>% filter(newName == standards_ID[i])
#
#   locations <- unique(standards_data$loc.id)
#
#   #define the nested list for observed properties, values and units
#   ops_list <- list()
#   for (j in seq(1, nrow(standards_data))) {
#
#     #check for lower limits
#     if (is.na(standards_data$Minimum[j])) { #no lower limit
#       ops_list[j] <- list(list(
#         'observedProperty' = list('id' = standards_data$OP.id[j]),
#         'resultUpperLimit' = list('value' = standards_data$Maximum[j], 'unit' =
#                                     list('id' = standards_data$unit.id[j], 'unitGroup' =
#                                            list('id' = standards_data$groupId[j]))),
#         'ruleText' = standards_data$`Warning Message`[j]
#       ))
#     } else { #has lower limit
#       ops_list[j] <- list(list(
#         'observedProperty' = list('id' = standards_data$OP.id[j]),
#         'resultLowerLimit' = list('value' = standards_data$Minimum[j], 'unit' =
#                                     list('id' = standards_data$unit.id[j], 'unitGroup' =
#                                          list('id' = standards_data$groupId[j]))), #lower limit
#
#         'resultUpperLimit' = list('value' = standards_data$Maximum[j], 'unit' =
#                                     list('id' = standards_data$unit.id[j], 'unitGroup' =
#                                            list('id' = standards_data$groupId[j]))), #upper limit
#         'ruleText' = standards_data$`Warning Message`[j]
#         ))
#       }
#
#   }
#
#   data_body <- list('customId' = standards_ID[i],
#                     'name' = unique(standards_data$Name),
#                     'description' = unique(standards_data$Description),
#                     'issuingOrganization' = 'Test Standards Import',
#                     'samplingLocations' = list(list('id' = unique(standards_data$loc.id))), #only a single location ever
#                     'active' = 'TRUE',
#                     'observationStandards' = ops_list
#   )
#
#   y<-POST(paste0(testURL, "v1/standards/"), config = c(add_headers(.headers = c('Authorization' = testToken ))), body = data_body, encode = 'json')
#
#   fromJSON(rawToChar(y$content))
#
#   #if it already exists use PUT but test the customId first
#
# } #end for loop of unique standards
#
#
# ### Code to delete all standards to post a fresh copy
# x <- GET(paste0(testURL, "v1/standards/"), config = c(add_headers(.headers = c('Authorization' = testToken ))), body = list(), encode = 'json')
# standards_guid <- fromJSON(rawToChar(x$content))$domainObjects$id
#
# for (i in seq(1, length(standards_guid))) {
#   DELETE(paste0(testURL, "v1/standards/", standards_guid[i]), config = c(add_headers(.headers = c('Authorization' = testToken ))), body = list(), encode = 'json')
# }

}
