#reference packages 
library(httr)
library(jsonlite)
library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)
library(readr)

#get the API token from your environment file
#readRenviron(paste0(getwd(), "./.Renviron"))
#testToken <- Sys.getenv("TEST_TOKEN")
#prodToken <- Sys.getenv("PROD_TOKEN")
#testURL <- Sys.getenv("TEST_URL")
#prodURL <- Sys.getenv("PROD_URL")

# Reference functions----------------------------------------------------
#function to update URL/token based on chosen environment
update_base_url_token <- function(env){
  
  if(env == "prod"){
    
    baseURL <- prodURL
    token <- prodToken
    
  } else {
    
    baseURL <- testURL
    token <- testToken
    
  }
  
  urlParameters <- list(baseURL, token)
  
  return(urlParameters)
  
}

#by default global variables always reference "test" environment
urlParameters <- update_base_url_token("test")
baseURL <- urlParameters[[1]]
token <- urlParameters[[2]]

get_profiles_for_url <- function(env, url){
  
  urlParameters <- update_base_url_token(env)
  baseURL <- urlParameters[[1]]
  token <- urlParameters[[2]]
  
  data_body <- list()
  
  xTemp <- GET(url, config = c(add_headers(.headers = 
                                              c('Authorization' = token))), body = data_body, encode = 'json')
  
  total = fromJSON(rawToChar(xTemp$content))$totalCount
  
  if (total > 1000) { #if there are more than 1000 records loop
    
    temp <- fromJSON(rawToChar(xTemp$content))$domainObjects
    
    numberLoops = ceiling(total/1000)
    
    for (i in seq(2, numberLoops)) {
      
      cursor = fromJSON(rawToChar(xTemp$content))$cursor
      
      tempURL = paste0(url, "&cursor=", cursor)
      
      xTemp <- GET(tempURL, config = c(add_headers(.headers = 
                                                      c('Authorization' = token))), body = data_body, encode = 'json')
      
      tempElement <- fromJSON(rawToChar(xTemp$content))$domainObjects
      
      rownames(tempElement) <- NULL
      
      rownames(temp) <- NULL
      
      temp <- bind_rows(temp, tempElement)
      
      print(i)
      
    }
    
  } else {
    
    temp <- fromJSON(rawToChar(xTemp$content))$domainObjects
    
  }
  
  return(temp)  
  
}
get_profiles <- function(env, dataType){
  
  #for prod env, use the function parameter env = "prod"
  urlParameters <- update_base_url_token(env)
  baseURL <- urlParameters[[1]]
  token <- urlParameters[[2]]
  
  if(dataType == "units"){
    
    url <- str_c(baseURL, "v1/units")
    
  } else if(dataType == "unitgroups"){
    
    url <- str_c(baseURL, "v1/unitgroups")
    
  } else if(dataType == "extendedattributes"){
    
    url <- str_c(baseURL, "v1/extendedattributes")
    
  } else if(dataType == "observedproperties"){
    
    url <- str_c(baseURL, "v1/observedproperties")
    
  } else if(dataType == "methods"){
    
    url <- str_c(baseURL, "v1/analysismethods")
    
  } else if(dataType == "labs"){
    
    url <- str_c(baseURL, "v1/laboratories")
    
  } else if(dataType == "locationgrouptypes"){
    
    url <- str_c(baseURL, "v1/samplinglocationgrouptypes")
    
  } else if(dataType == "locationtypes"){
    
    url <- str_c(baseURL, "v1/samplinglocationtypes")
    
  } else if(dataType == "locationgroups"){
    
    url <- str_c(baseURL, "v1/samplinglocationgroups")
    
  } else if(dataType == "locations"){
    
    url <- str_c(baseURL, "v1/samplinglocations?limit=1000")
    
  } else if(dataType == "mediums"){
    
    url <- str_c(baseURL, "v1/mediums")
    
  } else if(dataType == "taxonomylevels"){
    
    url <- str_c(baseURL, "v1/taxonomylevels")
    
  } else if(dataType == "detectionconditions"){
    
    url <- str_c(baseURL, "v1/detectionconditions")
    
  } else if(dataType == "resultgrades"){
    
    url <- str_c(baseURL, "v1/resultgrades")
    
  } else if(dataType == "resultstatuses"){
    
    url <- str_c(baseURL, "v1/resultstatuses")
    
  } else if(dataType == "fishtaxonomy"){
    
    url <- str_c(baseURL, "v1/taxons")
    
  } else if(dataType == "collectionmethods"){
    
    url <- str_c(baseURL, "v1/collectionmethods")
    
  } else if(dataType == "filters"){
    
    url <- str_c(baseURL, "v1/filters")
    
  } else if(dataType == "projects"){
    
    url <- str_c(baseURL, "v1/projects")
    
  }
  
  tempProfiles <- get_profiles_for_url(env, url)
  
  return(tempProfiles)
  
}
make_api_request <- function(env, id, dataType) {
  
  urlParameters <- update_base_url_token(env)
  baseURL <- urlParameters[[1]]
  token <- urlParameters[[2]]
  
  if (dataType == "activities") {
    
    url <- str_c(baseURL, "v1/activities/", id)
    
  } else if (dataType == "analyticalgroups") {
    
    url <- str_c(baseURL, "v1/analyticalgroups/", id)
    
  } else if (dataType == "collectionmethods") {
    
    url <- str_c(baseURL, "v1/collectionmethods/", id)
    
  } else if (dataType == "extendedattributes") {
    
    url <- str_c(baseURL, "v1/extendedattributes/", id)
    
  } else if (dataType == "fieldvisits") {
    
    url <- str_c(baseURL, "v1/fieldvisits/", id)
    
  } else if (dataType == "filters") {
    
    url <- str_c(baseURL, "v1/filters/", id)
    
  } else if (dataType == "laboratories") {
    
    url <- str_c(baseURL, "v1/laboratories/", id)
    
  } else if(dataType == "mediums"){
    
    url <- str_c(baseURL, "v1/mediums/")
    
  } else if (dataType == "observations"){
    
    url <- str_c(baseURL, "v2/observations/", id)
    
  } else if (dataType == "projects") {
    
    url <- str_c(baseURL, "v1/projects/", id)
    
  } else if (dataType == "Sampling Agency") {

    url <- str_c(baseURL, "v1/extendedattributes/", id, "/dropdownlistitems")
    
  } else if (dataType == "samplinglocations") {
    
    url <- str_c(baseURL, "v1/samplinglocations/", id)
    
  } else if (dataType == "samplinglocationgroups") {
    
    url <- str_c(baseURL, "v1/samplinglocationgroups/", id)
    
  } else if (dataType == "specimens") {
    
    url <- str_c(baseURL, "v1/specimens/", id)
    
  } else {
    
    return("The code is currently set to only make requests for observations, field visits, and samplinglocations")
    
  }
  
  data_body <- list()
  
  response <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  
  if (status_code(response) != 200) {
    
    return("Failed to fetch data: ", status_code(response))
    
  }
  
  data <- fromJSON(rawToChar(response$content)) #%>% unnest_wider(auditAttributes, names_repair = "universal")
  
  if(dataType %in% c("mediums", "samplingagency")){
    
    data <- data$domainObjects
    
  }
  
  return(data)
}

# getting relevant parameters (other than id) to download for given variable --------
gen_list_rel_var <- function(dataType){
  
  if (dataType == "activities"){
    
    relVar <- c("fieldVisit.id", "fieldVisit.project.id", "activity.samplingLocation.id", "auditAttributes.creationUserProfileId")
    
  } else if (dataType == "analyticalgroups"){
    
    relVar <- c("name", "type", "auditAttributes.creationUserProfileId")
    
  } else if (dataType == "collectionmethods"){
    
    relVar <- c("customId", "identifierOrganization", "name")
    
  } else if (dataType == "extendedattributes"){
    
    relVar <- c("customId", "dataType")
    
  } else if (dataType == "fieldvisits"){
    
    relVar <- c("project.id", "startTime", "auditAttributes.creationUserProfileId", "samplingLocation.id")
    
  } else if (dataType == "filters"){
    
    relVar <- c("customId", "samplingLocations.id", "auditAttributes.creationUserProfileId")
    
  } else if (dataType == "labs"){
    
    relVar <- c("customId", "name")
    
  } else if (dataType == "mediums"){
    
    relVar <- c("customId")
    
  } else if (dataType == "observations"){
    
    relVar <- c("activity.id", "fieldVisit.id", "fieldVisit.project.id", "activity.samplingLocation.id", "auditAttributes.creationUserProfileId")
    
  } else if (dataType == "projects"){
    
    relVar <- c("customId", "name", "description", "type", "auditAttributes.creationUserProfileId")
    
  } else if (dataType == "samplinglocations"){
    
    relVar <- c("customId", "name", "type.customId", "description", "attachments.attachment.comment", "elevation.value", "elevation.unit.customId", "longitude", "latitude", "horizontalCollectionMethod", "auditAttributes.creationUserProfileId",  "auditAttributes.modificationUserProfileId", "auditAttributes.creationTime", "auditAttributes.modificationTime", "samplingLocationGroups.id", "samplingLocationGroups.name", "samplingLocationGroups.description", "samplingLocationGroups.locationGroupType.customId")
    
  } else if (dataType == "samplinglocationgroups"){
    
    relVar <- c("name", "description", "auditAttributes.creationUserProfileId")
    
  } else if (dataType == "specimens"){
    
    relVar <- c("activity.id", "observations.id", "fieldVisit.id", "fieldVisit.project.id", "activity.samplingLocation.id", "auditAttributes.creationUserProfileId")
    
  } 
  
  return(relVar)
  
}
# getting dropdown lists of extended attributes
dropdownlist_extended_attributes <- function(env, dataType){
  
  idDataType <- get_profiles(env, "extendedattributes") %>%
    dplyr::filter(customId == dataType) %>% dplyr::select(id) %>% unlist()
  
  data_dropdown <- make_api_request(env, idDataType, dataType)  
  
  return(data_dropdown)
  
}
