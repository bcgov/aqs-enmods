#reference packages 
library(httr)
library(jsonlite)
library(tidyverse)
library(dplyr)
library(purrr)
library(lubridate)
library(stringr)
library(bcdata)
library(sf)
library(tidygeocoder)
library(readr)
library(readxl)
library(writexl)
library(openxlsx)
library(hunspell)

#get the API token from your environment file
readRenviron(paste0(getwd(), "./.Renviron"))
test_token <- Sys.getenv("TEST_TOKEN")
prod_token <- Sys.getenv("PROD_TOKEN")
test_url <- Sys.getenv("TEST_URL")
prod_url <- Sys.getenv("PROD_URL")

#function to update URL/token based on chosen environment
update_base_url_token <- function(env){
  
  if(env == "prod"){
    
    base_url <- prod_url
    token <- prod_token
    
  } else {
    
    base_url <- test_url
    token <- test_token
    
  }
  
  url_parameters <- list(base_url, token)
  
  return(url_parameters)
  
}

#by default global variables always reference "test" environment
url_parameters <- update_base_url_token("test")
base_url <- url_parameters[[1]]
token <- url_parameters[[2]]

# Reference functions----------------------------------------------------
get_profiles_for_url <- function(env, url){
  
  url_parameters <- update_base_url_token(env)
  base_url <- url_parameters[[1]]
  token <- url_parameters[[2]]
  
  data_body <- list()
  
  x_temp <- GET(url, config = c(add_headers(.headers = 
                                              c('Authorization' = token))), body = data_body, encode = 'json')
  
  total = fromJSON(rawToChar(x_temp$content))$totalCount
  
  if (total > 1000) { #if there are more than 1000 records loop
    
    temp <- fromJSON(rawToChar(x_temp$content))$domainObjects
    
    number_loops = ceiling(total/1000)
    
    for (i in seq(2,number_loops)) {
      
      cursor = fromJSON(rawToChar(x_temp$content))$cursor
      
      tempURL = paste0(url, "&cursor=", cursor)
      
      x_temp <- GET(tempURL, config = c(add_headers(.headers = 
                                                      c('Authorization' = token))), body = data_body, encode = 'json')
      
      temp_element <- fromJSON(rawToChar(x_temp$content))$domainObjects
      
      rownames(temp_element) <- NULL
      
      rownames(temp) <- NULL
      
      temp <- bind_rows(temp, temp_element)
      
      print(i)
      
    }
    
  } else {
    
    temp <- fromJSON(rawToChar(x_temp$content))$domainObjects
    
  }
  
  return(temp)  
  
}
get_profiles <- function(env, data_type){
  
  #for prod env, use the function parameter env = "prod"
  url_parameters <- update_base_url_token(env)
  base_url <- url_parameters[[1]]
  token <- url_parameters[[2]]
  
  if(data_type == "units"){
    
    url <- str_c(base_url, "v1/units")
    
  } else if(data_type == "unitgroups"){
    
    url <- str_c(base_url, "v1/unitgroups")
    
  } else if(data_type == "extendedattributes"){
    
    url <- str_c(base_url, "v1/extendedattributes")
    
  } else if(data_type == "observedproperties"){
    
    url <- str_c(base_url, "v1/observedproperties")
    
  } else if(data_type == "methods"){
    
    url <- str_c(base_url, "v1/analysismethods")
    
  } else if(data_type == "labs"){
    
    url <- str_c(base_url, "v1/laboratories")
    
  } else if(data_type == "locationgrouptypes"){
    
    url <- str_c(base_url, "v1/samplinglocationgrouptypes")
    
  } else if(data_type == "locationtypes"){
    
    url <- str_c(base_url, "v1/samplinglocationtypes")
    
  } else if(data_type == "locationgroups"){
    
    url <- str_c(base_url, "v1/samplinglocationgroups")
    
  } else if(data_type == "locations"){
    
    url <- str_c(base_url, "v1/samplinglocations?limit=1000")
    
  } else if(data_type == "mediums"){
    
    url <- str_c(base_url, "v1/mediums")
    
  } else if(data_type == "taxonomylevels"){
    
    url <- str_c(base_url, "v1/taxonomylevels")
    
  } else if(data_type == "detectionconditions"){
    
    url <- str_c(base_url, "v1/detectionconditions")
    
  } else if(data_type == "resultgrades"){
    
    url <- str_c(base_url, "v1/resultgrades")
    
  } else if(data_type == "resultstatuses"){
    
    url <- str_c(base_url, "v1/resultstatuses")
    
  } else if(data_type == "fishtaxonomy"){
    
    url <- str_c(base_url, "v1/taxons")
    
  } else if(data_type == "collectionmethods"){
    
    url <- str_c(base_url, "v1/collectionmethods")
    
  } else if(data_type == "filters"){
    
    url <- str_c(base_url, "v1/filters")
    
  } else if(data_type == "projects"){
    
    url <- str_c(base_url, "v1/projects")
    
  }
  
  temp_profiles <- get_profiles_for_url(env, url)
  
  return(temp_profiles)
  
}

# #example code using the reference functions -------------------------------
mediums <- get_profiles("prod", "mediums")

#extracting medium names from the imported mediums file
json_mediums_raw <- mediums %>% dplyr::select(customId)

#storing medium names in JSON format
json_mediums_proc <- toJSON(list(items = json_mediums_data), pretty = TRUE)

#writing the created JSON file to upload to BC Box
write(json_mediums_proc, file = "enmods_mediums_data.json")