library(httr)
library(jsonlite)
library(tidyverse)
library(dplyr)
library(lubridate)
library(stringr)
library(bcdata)
library(sf)
library(tidygeocoder)
library(readr)

#get all location from dev

#get the API token from your environment file
readRenviron(paste0(getwd(), "./.Renviron"))
test_token <- Sys.getenv("test_token")
prod_token <- Sys.getenv("prod_token")
test_url <- Sys.getenv("test_url")
prod_url <- Sys.getenv("prod_url")

#data_access = "test"

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

url_parameters <- update_base_url_token("test")
base_url <- url_parameters[[1]]
token <- url_parameters[[2]]

# universal functions -----------------------------------------------------
# getting a complete list of IDs ------------------------------

get_profiles_for_url <- function(env, url){
  
  url_parameters <- update_base_url_token(env)
  base_url <- url_parameters[[1]]
  token <- url_parameters[[2]]
  
  data_body <- list()
  
  x_temp <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), 
                body = data_body, encode = 'json')
  
  total = fromJSON(rawToChar(x_temp$content))$totalCount
  
  if (total > 1000) { #if there are more than 1000 records loop
    
    temp <- fromJSON(rawToChar(x_temp$content))$domainObjects
    
    number_loops = ceiling(total/1000)
    
    #i = 2
    
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
      
      # if(i==3){
      #   break
      # }
      
    }
    
  } else {
    
    temp <- fromJSON(rawToChar(x_temp$content))$domainObjects
    
  }
  
  return(temp)  
  
}

start_time <- proc.time()

url <- str_c(base_url, "v2/observations?limit=1000")

test_get_obs <- get_profiles_for_url(url)

end_time <- proc.time()

elapsed_time <- end_time - start_time

print(elapsed_time)

#test_get_obs_unnested <- unnest_wider(test_get_obs, activity)

get_profiles <- function(env, data_type){
  
  #env <- "test"
  
  #default is "test" and for prod env, use the function parameter "prod"
  url_parameters <- update_base_url_token(env)
  base_url <- url_parameters[[1]]
  token <- url_parameters[[2]]
  
  #data_type <- "unitgroups"
  
  if(data_type == "units"){
    
    url <- str_c(base_url, "v1/units")
    
    temp_profiles <- get_profiles_for_url(env, url)
    
  } else if(data_type == "unitgroups"){
    
    url <- str_c(base_url, "v1/unitgroups")
    
    temp_profiles <- get_profiles_for_url(env, url)
    
  }
  
  return(temp_profiles)
  
}

post_profiles_for_url <- function(env, data_type, profile){
  
  # env = "prod"
  # 
  # data_type = "units"
  # 
  # profile <- units_profiles %>%
  #   dplyr::filter(customId == "ppt")

  #default is "test" and for prod env, use the function parameter "prod"
  url_parameters <- update_base_url_token(env)
  base_url <- url_parameters[[1]]
  token <- url_parameters[[2]]
  
  profile <- profile %>% 
    mutate(across(everything(), ~ replace(., is.na(.), "")))
  
  if(data_type == "unitgroups"){
    
      url <- paste0(base_url, "v1/unitgroups")
      
      rel_var <- c("customId", "supportsConversion")
      
      } else if(data_type == "units"){
    
      url <- paste0(base_url, "v1/units")
      
      rel_var <- c("customId", "name", "baseMultiplier",
                   "baseOffset", "unitGroup.id", 
                   "unitGroup.supportsConversion")
      }
  
  #post_check_temp <- character(dim(profile)[1])
  
  #print(length(post_check_temp))
  
  for(j in 1:dim(profile)[1]){
    
    #j <- 1
      
    temp_profile <- profile %>% 
        keep(names(.) %in% rel_var) %>%
        slice(j) %>%
        as.list()
    
    data_body <- temp_profile
    
    if(data_type == "units"){
      
      #loop to put all the units in the group
        
        #If the unit group supports conversion provide conversion factors
        if (temp_profile$unitGroup.supportsConversion == TRUE) {
          
          data_body <- list(
            customId = temp_profile$customId,
            "name" = temp_profile$name,
            "baseMultiplier" = temp_profile$baseMultiplier,
            "baseOffset" = temp_profile$baseOffset,
            "unitGroup" = list("id" = temp_profile$unitGroup.id))
          
        } else { 
          
          data_body <- list(
            customId = temp_profile$customId,
            "name" = temp_profile$name,
            "unitGroup" = list("id" = temp_profile$unitGroup.id))
          
        }
        
      }
      
      #Post the configuration
      x<-POST(url, config = c(add_headers(.headers = 
        c('Authorization' = token))), body = data_body, 
        encode = 'json')
      
      message <- fromJSON(rawToChar(x$content))
      
      print(j)
      
      # Add a sleep to avoid hitting the rate limit
      # Sys.sleep(5)
      
    }
    
  #}
  
  
  # if(status_code(x)!=200){
  #   
  #   print(i)
  #   
  #   next
  #} else {
  #   
  #   post_check_temp[i] <- fromJSON(rawToChar(x$content))$message
  #   
  #}
  
  #}

#return(post_check_temp)

return(message)

}

#get unit group data to post
unitgroups_profiles <- get_profiles("test", "unitgroups")

#post unit group data
post_check <- post_profiles_for_url("prod", "unitgroups", unitgroups_profiles)

#get posted unit group data
unitgroups_profiles <- get_profiles("prod", "unitgroups")

#get unit data
units_profiles <- get_profiles("test", "units")

#update unit data to account for posted unit groups
units_profiles <- units_profiles %>%
  unnest_wider(unitGroup, names_sep = ".") %>% 
  dplyr::select(-c(unitGroup.id)) %>% 
  left_join(unitgroups_profiles %>% 
              dplyr::select(id, customId) %>%
              rename(unitGroup.id = id), 
            by = join_by(unitGroup.customId == customId), 
            keep = FALSE) %>%
  dplyr::filter(!(is.na(unitGroup.customId))) %>%
  dplyr::select(id, customId, name, baseMultiplier,
                baseOffset, unitGroup.id, everything())

#post unit data
post_check <- post_profiles_for_url("prod", "units", units_profiles)

del_profiles_for_url <- function(env, data_type){

  # env <- "prod"
  # 
  # data_type <- "unitgroups"
  
  temp_profile <- get_profiles(env, data_type)
  
  #default is "test" and for prod env, use the function parameter "prod"
  url_parameters <- update_base_url_token(env)
  base_url <- url_parameters[[1]]
  token <- url_parameters[[2]]
  
  if(data_type == "unitgroups"){
    
    base_url <- str_c(base_url, "v1/unitgroups/")
    
  } else if(data_type == "units"){
    
    base_url <- str_c(base_url, "v1/units/")
    
  }
  
  del_ids <- temp_profile$id
  
  for(id in del_ids){
    
    #id <- del_ids
    
    data_body <- list()
    
    url <- str_c(base_url, id)
    
    #Make the unit group
    x<-DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), 
            body = data_body, encode = 'json')
    
    #response_check <- fromJSON(rawToChar(x$content))$message
    
    # Add a sleep to avoid hitting the rate limit
    #Sys.sleep(5)
    
    # if(status_code(x)!=200){
    #   
    #   print(id)
    #   
    #   next
    #} # else {
    #   
    #   post_check_temp[i] <- fromJSON(rawToChar(x$content))$message
    #   
    # }
    
  }
  
  #return(response_check)
  return()
  
}

del_check <- del_profiles_for_url("prod", "units")

del_check <- del_profiles_for_url("prod", "unitgroups")

get_ids_for_url <- function(url){
  
  
  data_body <- list()
  
  x_temp <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), 
                body = data_body, encode = 'json')
  
  total = fromJSON(rawToChar(x_temp$content))$totalCount
  
  if (total > 1000) { #if there are more than 1000 records loop
    
    temp <- fromJSON(rawToChar(x_temp$content))$domainObjects
    
    temp_ids <- temp$id
    
    number_loops = ceiling(total/1000)
    
    #i = 2
    
    for (i in seq(2,number_loops)) {
      
      cursor = fromJSON(rawToChar(x_temp$content))$cursor
      
      tempURL = paste0(url, "&cursor=", cursor)
      
      x_temp <- GET(tempURL, config = c(add_headers(.headers = 
                                                      c('Authorization' = token))), body = data_body, encode = 'json')
      
      temp_element <- fromJSON(rawToChar(x_temp$content))$domainObjects
      
      temp_element_ids <- temp_element$id 
      
      rownames(temp_element_ids) <- NULL
      
      rownames(temp_ids) <- NULL
      
      temp_ids <- append(temp_ids, temp_element_ids)
      
      #locs <- rbind(locs, temp_locs)
      
      print(i)
      
    }
    
  } else {
    
    temp <- fromJSON(rawToChar(x_temp$content))$domainObjects
    
    temp_ids <- temp$id
    
  }
  
  return(temp_ids)  
  
}

del_ids_for_url <- function(url){
  
  
  data_body <- list()
  
  x_temp <- DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), 
                body = data_body, encode = 'json')
  
  # total = fromJSON(rawToChar(x_temp$content))$totalCount
  # 
  # if (total > 1000) { #if there are more than 1000 records loop
  #   
  #   temp <- fromJSON(rawToChar(x_temp$content))$domainObjects
  #   
  #   temp_ids <- temp$id
  #   
  #   number_loops = ceiling(total/1000)
  #   
  #   #i = 2
  #   
  #   for (i in seq(2,number_loops)) {
  #     
  #     cursor = fromJSON(rawToChar(x_temp$content))$cursor
  #     
  #     tempURL = paste0(url, "&cursor=", cursor)
  #     
  #     x_temp <- DELETE(tempURL, config = c(add_headers(.headers = 
  #                     c('Authorization' = token))), body = data_body, encode = 'json')
  #     
  #     temp_element <- fromJSON(rawToChar(x_temp$content))$domainObjects
  #     
  #     # temp_element_ids <- temp_element$id 
  #     # 
  #     # rownames(temp_element_ids) <- NULL
  #     # 
  #     # rownames(temp_ids) <- NULL
  #     # 
  #     # temp_ids <- append(temp_ids, temp_element_ids)
  #     
  #     #locs <- rbind(locs, temp_locs)
  #     
  #     print(i)
  #     
  #   }
  #   
  # } else {
  #   
  #   temp <- fromJSON(rawToChar(x_temp$content))$domainObjects
  #   
  #   #temp_ids <- temp$id
  #   
  # }
  # 
  # #return(temp_ids)  
  
  return(x_temp)
  
}

get_ids <- function(data_type){
  
  if (data_type == "activities") {
    
    url <- str_c(base_url, "v1/activities?limit=1000")
    
  } else if (data_type == "analyticalgroups") {
    
    url <- str_c(base_url, "v1/analyticalgroups?limit=1000")
    
  } else if (data_type == "collectionmethods") {
    
    url <- str_c(base_url, "v1/collectionmethods")
    
  } else if (data_type == "extendedattributes") {
    
    url <- str_c(base_url, "v1/extendedattributes?limit=1000")
    
  } else if (data_type == "fieldvisits") {
    
    url <- str_c(base_url, "v1/fieldvisits?limit=1000")
    
  } else if (data_type == "filters") {
    
    url <- str_c(base_url, "v1/filters")
    
  } else if (data_type == "laboratories") {
    
    url <- str_c(base_url, "v1/laboratories")
    
  } else if(data_type == "mediums"){
    
    #get all observation IDs out (for editing)
    url <- str_c(base_url, "v1/mediums")
    
  } else if(data_type == "observations"){
    
    #get all observation IDs out (for editing)
    url <- str_c(base_url, "v2/observations?limit=1000")
    
  } else if (data_type == "projects") {
    
    url <- str_c(base_url, "v1/projects?limit=1000")
    
  } else if (data_type == "samplinglocations") {
    
    url <- str_c(base_url, "v1/samplinglocations?limit=1000")
    
  } else if (data_type == "samplinglocationgroups") {
    
    url <- str_c(base_url, "v1/samplinglocationgroups")
    
  } else if (data_type == "specimens") {
    
    url <- str_c(base_url, "v1/specimens?limit=1000")
    
  } else if (data_type == "units") {
    
    url <- str_c(base_url, "v1/units")
    
  } else if (data_type == "unitgroups") {
    
    url <- str_c(base_url, "v1/unitgroups?limit=1000")
    
  } else {
    
    return("The coding pipeline has been developed only for observations, field visits, and samplinglocations.")
    
  }
  
  # data_body <- list()
  # 
  # x_temp <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), 
  #               body = data_body, encode = 'json')
  # 
  # total = fromJSON(rawToChar(x_temp$content))$totalCount
  # 
  # if (total > 1000) { #if there are more than 1000 records loop
  #   
  #   temp <- fromJSON(rawToChar(x_temp$content))$domainObjects
  #   
  #   temp_ids <- temp$id
  #   
  #   number_loops = ceiling(total/1000)
  #   
  #   #i = 2
  #   
  #   for (i in seq(2,number_loops)) {
  #     
  #     cursor = fromJSON(rawToChar(x_temp$content))$cursor
  #     
  #     tempURL = paste0(url, "&cursor=", cursor)
  #     
  #     x_temp <- GET(tempURL, config = c(add_headers(.headers = 
  #       c('Authorization' = token))), body = data_body, encode = 'json')
  #     
  #     temp_element <- fromJSON(rawToChar(x_temp$content))$domainObjects
  #     
  #     temp_element_ids <- temp_element$id 
  #     
  #     rownames(temp_element_ids) <- NULL
  #     
  #     rownames(temp_ids) <- NULL
  #     
  #     temp_ids <- append(temp_ids, temp_element_ids)
  #     
  #     #locs <- rbind(locs, temp_locs)
  #     
  #     print(i)
  #   
  #   }
  #   
  # } else {
  #   
  #   temp <- fromJSON(rawToChar(x_temp$content))$domainObjects
  #   
  #   temp_ids <- temp$id
  #   
  # }
  # 
  # return(temp_ids)  
  
  temp_ids <- get_ids_for_url(url)
  
  return(temp_ids)
  
}

spcmns_ids <- get_ids("specimens")

spcmns_ids_old <- spcmns_ids

obs_ids <- get_ids("observations")

actvty_ids <- get_ids("activities")

vsts_ids <- get_ids("fieldvisits")

locs_ids <- get_ids("samplinglocations")

locgrps_ids <- get_ids("samplinglocationgroups")

prjcts_ids <- get_ids("projects")

analyticalgrps_ids <- get_ids("analyticalgroups")

fltrs_ids <- get_ids("filters")

labs_ids <- get_ids("laboratories")

mediums_ids <- get_ids("mediums")

collectionmthds_ids <- get_ids("collectionmethods")

extendedattrbts_ids <- get_ids("extendedattributes")

units_ids <- get_ids("units")

unitgrps_ids <- get_ids("unitgroups")

# making one API all data request by id and data type ------------------------------
# Function to make an API request
make_api_request <- function(id, data_type) {
  
  # id <- mediums_ids[1]
  # 
  # data_type <- "mediums"
  
  if (data_type == "activities") {
    
    url <- str_c(base_url, "v1/activities/", id)
    
  } else if (data_type == "analyticalgroups") {
    
    url <- str_c(base_url, "v1/analyticalgroups/", id)
    
  } else if (data_type == "collectionmethods") {
    
    url <- str_c(base_url, "v1/collectionmethods/", id)
    
  } else if (data_type == "extendedattributes") {
    
    url <- str_c(base_url, "v1/extendedattributes/", id)
    
  } else if (data_type == "fieldvisits") {
    
    url <- str_c(base_url, "v1/fieldvisits/", id)
    
  } else if (data_type == "filters") {
    
    url <- str_c(base_url, "v1/filters/", id)
    
  } else if (data_type == "laboratories") {
    
    url <- str_c(base_url, "v1/laboratories/", id)
    
  } else if(data_type == "mediums"){
    
    #get all observation IDs out (for editing)
    url <- str_c(base_url, "v1/mediums/")
    
  } else if (data_type == "observations"){
    
    url <- str_c(base_url, "v2/observations/", id)
    
  } else if (data_type == "projects") {
    
    url <- str_c(base_url, "v1/projects/", id)
    
  } else if (data_type == "samplingagency") {
    
    id <- "65d94fac-aac5-498f-bc73-b63a322ce350"
    
    url <- str_c(base_url, "v1/extendedattributes/", id, "/dropdownlistitems")
    
  } else if (data_type == "samplinglocations") {
    
    url <- str_c(base_url, "v1/samplinglocations/", id)
    
  } else if (data_type == "samplinglocationgroups") {
    
    url <- str_c(base_url, "v1/samplinglocationgroups/", id)
    
  } else if (data_type == "specimens") {
    
    url <- str_c(base_url, "v1/specimens/", id)
    
  } else {
    
    return("The code is currently set to only make requests for observations, field visits, and samplinglocations")
    
  }
  
  data_body <- list()
  
  response <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  
  if (status_code(response) != 200) {
    
    return("Failed to fetch data: ", status_code(response))
    #stop("Failed to fetch data: ", status_code(response))
  }
  
  # content <- content(response, as = "text", encoding = "UTF-8")
  # data <- fromJSON(content)
  # return(content)
  
  data <- fromJSON(rawToChar(response$content)) #%>% unnest_wider(auditAttributes, names_repair = "universal")
  
  # # Write the raw JSON content to a temporary file
  # temp_file <- tempfile()
  # writeBin(content(response, as = "raw"), temp_file)
  # 
  # # Stream the JSON data from the temporary file
  # con <- file(temp_file, open = "r")
  # data <- stream_in(con)
  # close(con)
  # 
  # # Delete the temporary file
  # unlink(temp_file)
  
  if(data_type %in% c("mediums", "samplingagency")){
    
    data <- data$domainObjects
    
  }
  
  return(data)
}

spcmns_sample <- make_api_request(spcmns_ids[1], "specimens")

obs_sample <- make_api_request(obs_ids[1], "observations")

actvty_sample <- make_api_request(actvty_ids[1], "activities")

vsts_sample <- make_api_request(vsts_ids[1], "fieldvisits")

locs_sample <- make_api_request(locs_ids[1], "samplinglocations")

locgrps_sample <- make_api_request(locgrps_ids[1], "samplinglocationgroups")

prjcts_sample <- make_api_request(prjcts_ids[1], "projects")

analyticalgrps_sample <- make_api_request(analyticalgrps_ids[1], "analyticalgroups")

fltrs_sample <- make_api_request(fltrs_ids[1], "filters")

labs_sample <- make_api_request(labs_ids[1], "laboratories")

mediums_sample <- make_api_request("", "mediums")

collectionmthds_sample <- make_api_request(collectionmthds_ids[1], "collectionmethods")

extendedattrbts_sample <- make_api_request(extendedattrbts_ids[1], "extendedattributes")

samplingagency_sample <- make_api_request("65d94fac-aac5-498f-bc73-b63a322ce350", "samplingagency")

# making one API history request by id and data type ------------------------------
# Function to make an API request for data history
make_api_request_history <- function(id, data_type) {
  
  if (data_type == "activities") {
    
    url <- str_c(base_url, "v1/activities/", id, "/history")
    
  } else if (data_type == "analyticalgroups") {
    
    url <- str_c(base_url, "v1/analyticalgroups/", id, "/history")
    
  } else if (data_type == "collectionmethods") {
    
    url <- str_c(base_url, "v1/collectionmethods/", id, "/history")
    
  } else if (data_type == "extendedattributes") {
    
    url <- str_c(base_url, "v1/extendedattributes/", id, "/history")
    
  } else if (data_type == "filters"){
    
    url <- str_c(base_url, "v1/filters/", id, "/history")
    
  } else if (data_type == "fieldvisits") {
    
    url <- str_c(base_url, "v1/fieldvisits/", id, "/history")
    
  } else if (data_type == "laboratories") {
    
    url <- str_c(base_url, "v1/laboratories/", id, "/history")
    
  } else if(data_type == "mediums"){
    
    #get all observation IDs out (for editing)
    url <- str_c(base_url, "v1/mediums/", id, "/history")
    
  } else if (data_type == "observations"){
    
    url <- str_c(base_url, "v2/observations/", id, "/history")
    
  } else if (data_type == "projects") {
    
    url <- str_c(base_url, "v1/projects/", id, "/history")
    
  } else if (data_type == "samplinglocations") {
    
    url <- str_c(base_url, "v1/samplinglocations/", id, "/history")
    
  } else if (data_type == "samplinglocationgroups") {
    
    url <- str_c(base_url, "v1/samplinglocationgroups/", id, "/history")
    
  } else if (data_type == "specimens") {
    
    url <- str_c(base_url, "v1/specimens/", id, "/history")
    
  } else {
    
    return("The code is currently set to only make requests for observations, field visits, and samplinglocations")
    
  }
  
  data_body <- list()
  
  response <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  
  if (status_code(response) != 200) {
    
    return("Failed to fetch data: ", status_code(response))
    #stop("Failed to fetch data: ", status_code(response))
  }
  
  # content <- content(response, as = "text", encoding = "UTF-8")
  # data <- fromJSON(content)
  # return(content)
  
  data <- fromJSON(rawToChar(response$content))$domainObjects #%>% unnest_wider(auditAttributes, names_repair = "universal")
  
  # # Write the raw JSON content to a temporary file
  # temp_file <- tempfile()
  # writeBin(content(response, as = "raw"), temp_file)
  # 
  # # Stream the JSON data from the temporary file
  # con <- file(temp_file, open = "r")
  # data <- stream_in(con)
  # close(con)
  # 
  # # Delete the temporary file
  # unlink(temp_file)
  
  return(data)
}

spcmns_sample_history <- make_api_request_history(spcmns_ids[1], "specimens")

obs_sample_history <- make_api_request_history(obs_ids[1], "observations")

actvty_sample_history <- make_api_request_history(actvty_ids[1], "activities")

vsts_sample_history <- make_api_request_history(vsts_ids[1], "fieldvisits")

locs_sample_history_old <- locs_sample_history

#Deleted on Apr 7 2025 at 12 pm
#Have to keep track of how long this stays deleted
#Still available on Apr 9 2025 at 12 pm
sahil_test_location <- "2e85c760-d78e-42c7-9cee-59b4197fa3a2"

locs_sample_history <- make_api_request_history(sahil_test_location, "samplinglocations")

locgrps_sample_history <- make_api_request_history(locgrps_ids[1], "samplinglocationgroups")

projects_sample_history <- make_api_request_history(prjcts_ids[1], "projects")

analyticalgrps_sample_history <- make_api_request_history(analyticalgrps_ids[1], "analyticalgroups")

fltrs_sample_history <- make_api_request_history(fltrs_ids[1], "filters")

labs_sample_history <- make_api_request_history(labs_ids[1], "laboratories")

mediums_sample_history <- make_api_request_history(mediums_ids[1], "mediums")

collectionmthds_sample_history <- make_api_request_history(collectionmthds_ids[1], "collectionmethods")

#NOT WORKING
extendedattrbts_sample_history <- make_api_request_history(extendedattrbts_ids[1], "extendedattributes")

# making one API request for location summary info ------------------------
# Function to make an API request for data history
# currently summary info is only available for locations
make_api_request_summary <- function(id, data_type) {
  
  # data_type <- "samplinglocations"
  # 
  # ids <- locs_ids[1]
  
  url <- str_c(base_url, "v1/", data_type, "/", id, "/summary")

  data_body <- list()
  
  response <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  
  if (status_code(response) != 200) {
    
    print("Failed to fetch data: ", status_code(response))
    return()
    #stop("Failed to fetch data: ", status_code(response))
  }

  data <- fromJSON(rawToChar(response$content))
  
  return(data)
}

loc_summary <- make_api_request_summary(locs_ids[2], "samplinglocations")
#loc_summary <- make_api_request_summary("4e2db011-1813-40c5-a5d3-a08ed346abe9", "samplinglocations")

# getting relevant parameters (other than id) to download for given variable --------
gen_list_rel_var <- function(data_type){
  
  if (data_type == "activities"){
    
    rel_var <- c("fieldVisit.id", "fieldVisit.project.id", "activity.samplingLocation.id", "auditAttributes.creationUserProfileId")
    
  } else if (data_type == "analyticalgroups"){
    
    rel_var <- c("name", "type", "auditAttributes.creationUserProfileId")
    
  } else if (data_type == "collectionmethods"){
    
    rel_var <- c("customId", "identifierOrganization", "name")
    
  } else if (data_type == "extendedattributes"){
    
    rel_var <- c("customId", "dataType")
    
  } else if (data_type == "fieldvisits"){
    
    rel_var <- c("project.id", "startTime", "auditAttributes.creationUserProfileId", "samplingLocation.id")
    
  } else if (data_type == "filters"){
    
    rel_var <- c("customId", "samplingLocations.id", "auditAttributes.creationUserProfileId")
    
  } else if (data_type == "laboratories"){
    
    rel_var <- c("customId", "name")
    
  } else if (data_type == "mediums"){
    
    rel_var <- c("customId")
    
  } else if (data_type == "observations"){
    
    rel_var <- c("activity.id", "fieldVisit.id", "fieldVisit.project.id", "activity.samplingLocation.id", "auditAttributes.creationUserProfileId")
    
  } else if (data_type == "projects"){
    
    rel_var <- c("customId", "name", "description", "type", "auditAttributes.creationUserProfileId")
    
  } else if (data_type == "samplinglocations"){
    
    rel_var <- c("customId", "name", "type.customId", "description", "attachments.attachment.comment", "elevation.value", "elevation.unit.customId", "longitude", "latitude", "horizontalCollectionMethod", "auditAttributes.creationUserProfileId",  "auditAttributes.modificationUserProfileId", "auditAttributes.creationTime", "auditAttributes.modificationTime", "samplingLocationGroups.id", "samplingLocationGroups.name", "samplingLocationGroups.description", "samplingLocationGroups.locationGroupType.customId")
    
  } else if (data_type == "samplinglocationgroups"){
    
    rel_var <- c("name", "description", "auditAttributes.creationUserProfileId")
    
  } else if (data_type == "specimens"){
    
    rel_var <- c("activity.id", "observations.id", "fieldVisit.id", "fieldVisit.project.id", "activity.samplingLocation.id", "auditAttributes.creationUserProfileId")
    
  } 
  
}

# using API on a list of ids to get id relationship with relevant variable------------------------------
get_var_profile_by_id <- function(data_type, ids) {
  
  # data_type <- "mediums"
  # # 
  # ids <- "all"
  # 
  rel_var <- gen_list_rel_var(data_type)
  
  # 
  # rel_var <- "auditAttributes.creationUserProfileId"
  
  if(identical(ids, "all")) {
    
    ids <- get_ids(data_type)
    
    #ids <- get_ids("fieldvisits")
    
  }
  
  i <- 1
  
  for (id in ids){
    
    #id <- locs_ids[23] 
    
    if(data_type == "mediums"){
      break
    }
    
    temp_tibble <- make_api_request(id, data_type) %>% unlist() %>%
      keep(names(.) %in% c("id", rel_var))
    
    if (data_type == "samplinglocations"){
      
      temp_tibble2 <- make_api_request_summary(id, "samplinglocations") %>% unlist()
      
      temp_tibble <- c(temp_tibble, temp_tibble2)
      
    }
    
    temp_tibble <- as_tibble(matrix(temp_tibble, ncol = length(temp_tibble),
                                    dimnames = list("", names(temp_tibble)), byrow = TRUE))
    
    # id <- ids[195]
    # 
    # temp_tibble1 <- make_api_request(id, data_type) %>% unlist() %>% 
    #   keep(names(.) %in% c("id", rel_var))
    # 
    # temp_tibble1 <- as_tibble(matrix(temp_tibble1, ncol = length(temp_tibble1), 
    #   dimnames = list("", names(temp_tibble1)), byrow = TRUE))
    # 
    # id <- ids[196]
    # 
    # temp_tibble2 <- make_api_request(id, data_type) %>% unlist() %>% 
    #   keep(names(.) %in% c("id", rel_var))
    # 
    # temp_tibble2 <- as_tibble(matrix(temp_tibble2, ncol = length(temp_tibble2), 
    #   dimnames = list("", names(temp_tibble2)), byrow = TRUE))
    # 
    # temp_tibble <- bind_rows(temp_tibble1, temp_tibble2)
    
    # Check if data is empty, break the loop if it is
    if (is.null(temp_tibble)) {
      next
    }
    
    if (length(temp_tibble) == 0) {
      next
    }
    
    if (i==1){
      
      data_full <- temp_tibble
      
      #print(dim(data_full))
      
      #print(str(data_full))
      
    } else {
      
      data_full <- bind_rows(data_full, temp_tibble)
      
      #print(dim(data_full))
      
      #print(str(data_full))
      
    }
    
    i <- i + 1
    
    # Add a sleep to avoid hitting the rate limit
    #Sys.sleep(1)
    
    print(i)
    
  }
  
  if(data_type == "mediums"){
  
    data_full <- make_api_request("", data_type) %>%
      keep(names(.) %in% c("id", rel_var))
  
  }
  
  return(data_full) 
  
}

spcmns_var_profiles_by_id <- get_var_profile_by_id("specimens", "all")

obs_var_profiles_by_id <- get_var_profile_by_id("observations", "all")

actvty_var_profiles_by_id <- get_var_profile_by_id("activities", "all")

vsts_var_profiles_by_id <- get_var_profile_by_id("fieldvisits", "all")

proj_var_profiles_by_id <- get_var_profile_by_id("projects", "all")

locs_var_profiles_by_id <- get_var_profile_by_id("samplinglocations", "all")

fltrs_var_profiles_by_id <- get_var_profile_by_id("filters", "all")

labs_var_profiles_by_id <- get_var_profile_by_id("laboratories", "all")

mediums_var_profiles_by_id <- get_var_profile_by_id("mediums", "all")

collectionmthds_var_profiles_by_id <- get_var_profile_by_id("collectionmethods", "all")

extendedattrbts_var_profiles_by_id <- get_var_profile_by_id("extendedattributes", "all")

dropdownlist_extendedattrbts <- function(data_type){
  
  id_data_type <- get_var_profile_by_id("extendedattributes", "all") %>%
    dplyr::filter(customId == data_type) %>% dplyr::select(id) %>% unlist()
 
  data_dropdown <- make_api_request(id_data_type, data_type)  
  
  return(data_dropdown)
  
}

samplingagency_var_profiles_by_id <- dropdownlist_extendedattrbts("samplingagency")

analyticalgrps_var_profiles_by_id <- get_var_profile_by_id("analyticalgroups", "all")

#write.csv(locs_var_profiles_by_id, "test_location_summary_enmods.csv")

#locgrps_var_profiles_by_id <- get_var_profile_by_id("samplinglocationgroups", "all")

#write.csv(locs_var_profiles_by_id, "test_location_summary_enmods.csv")

# test_locs_var_profiles_by_id <- get_var_profile_by_id("samplinglocations", "de360ca3-0708-4d84-93a2-c0ccce2dc8b2")

# updating older lists using the id by id approach --------
# Note that the pipeline below runs only for observations, field visits, and locations 
update_var <- function(data_type, ref_file) {
  
  last_run <- file.info(".RData")$mtime %>% as.POSIXct(format="%Y-%m-%d %H:%M:%S %Z", tz="America/Los_Angeles") %>% format("%Y-%m-%dT00:00:00%z")
  
  if(data_type != "observations"){
    
    url <- str_c(base_url, "v1/", data_type, "?startModificationTime=", last_run)
    
  } else {
    
    url <- str_c(base_url, "v2/", data_type, "?startModificationTime=", last_run)
    
  }
  
  data_body <- list()
  
  x_mdfd <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), 
                body = data_body, encode = 'json')
  
  #mdfd_var <- fromJSON(rawToChar(x_mdfd$content))$domainObjects
  
  #mdfd_var_ids <- mdfd_var$id
  
  total = fromJSON(rawToChar(x_mdfd$content))$totalCount
  
  print(total)
  
  if (total > 1000) { #if there are more than 1000 records loop
    
    # locs <- fromJSON(rawToChar(x_mdfd$content))$domainObjects %>%
    #   tibble::rownames_to_column("original_row_name") %>% as_tibble()
    #mdfd_var <- fromJSON(rawToChar(x_mdfd$content))$domainObjects %>% as_tibble() #%>% 
      #dplyr::select(id, customId, name, auditAttributes, type, latitude, 
      #              longitude, horizontalCollectionMethod, description) %>% 
      #unnest_wider(type, names_repair = "universal") %>% 
      #unnest_wider(auditAttributes, names_repair = "universal")
    
    var_ids <- fromJSON(rawToChar(x_mdfd$content))$domainObjects$id
    
    number_loops = ceiling(total/1000)
    
    #i = 2
    
    for (i in seq(2,number_loops)) {
      cursor = fromJSON(rawToChar(x_mdfd$content))$cursor
      tempURL = paste0(url, "&cursor=", cursor)
      
      x_mdfd <- GET(tempURL, config = c(add_headers(.headers = 
        c('Authorization' = token))), body = data_body, encode = 'json')
      
      temp_var_ids <- fromJSON(rawToChar(x_mdfd$content))$domainObjects$id 
      
      # #temp_locs <- fromJSON(rawToChar(x_mdfd$content))$domainObjects %>%
      # #  tibble::rownames_to_column("original_row_name") %>% as_tibble()
      # temp_mdfd_var <- fromJSON(rawToChar(x_mdfd$content))$domainObjects %>% 
      #   as_tibble() #%>% dplyr::select(id, customId, name, auditAttributes, 
      # type, latitude, longitude, horizontalCollectionMethod, description) %>% 
      #   #unnest_wider(type, names_repair = "universal") %>% 
      #   #unnest_wider(auditAttributes, names_repair = "universal")
      
      var_ids <- append(var_ids, temp_var_ids)
      
      #rownames(temp_mdfd_var) <- NULL
      
      #rownames(mdfd_var) <- NULL
      
      #mdfd_var <- bind_rows(mdfd_var, temp_mdfd_var)
      
      print(i)
    }
    
  } else {
    
     #mdfd_var <- fromJSON(rawToChar(x_mdfd$content))$domainObjects %>% as_tibble() #%>% 
      #dplyr::select(id, customId, name, auditAttributes, type, latitude, longitude,   horizontalCollectionMethod, description) %>% 
      #unnest_wider(type, names_repair = "universal") %>% 
      #unnest_wider(auditAttributes, names_repair = "universal")
     
     var_ids <- fromJSON(rawToChar(x_mdfd$content))$domainObjects$id
    
  }
  
  #Now need to do an id by id retrieval where I get the whole corresponding dataset
  mdfd_var_profile <- get_var_profile_by_id(data_type, var_ids)
  
  # Replace rows in locs with rows from mdfd_locs based on location_id
  updated_var <- ref_file %>%
    anti_join(mdfd_var_profile, by = "id") %>%  # Remove matching IDs
    bind_rows(mdfd_var_profile)
  
  return(updated_var)
}

# #reminding the code when the pipeline output was last saved
last_run <- file.info(".RData")$mtime %>% as.POSIXct(format="%Y-%m-%d %H:%M:%S %Z", tz="America/Los_Angeles") %>% format("%Y-%m-%dT00:00:00%z")

spcmns_test <- update_var("specimens", spcmns_var_profiles_by_id)

obs_test <- update_var("observations", obs_var_profiles_by_id)

vsts_test <- update_var("fieldvisits", vsts_var_profiles_by_id)

locs_test <- update_var("samplinglocations", locs_var_profiles_by_id)


# Note that the pipeline below runs only for observations, field visits, and locations 
# keeping downloaded data up to date --------------------------------------
gen_updated_data_file <- function(data_type){
  
  #data_type <- "observations"
  
  if (data_type == "observations"){
    
    filenm <- "obs_var_profiles_by_id"
    
  } else if (data_type == "specimens"){
   
    filenm <- "spcmns_var_profiles_by_id"    
     
  } else if (data_type == "fieldvisits"){
    
    filenm <- "vsts_var_profiles_by_id"    
    
  } else if (data_type == "samplinglocations"){
    
    filenm <- "locs_var_profiles_by_id"    
    
  } else if (data_type == "analyticalgroups"){
    
    filenm <- "analyticalgrps_var_profiles_by_id" 
    
  } else if (data_type == "activities"){
    
    filenm <- "actvty_var_profiles_by_id" 
    
  } else if (data_type == "projects"){
    
    filenm <- "proj_var_profiles_by_id" 
    
  } else if (data_type == "filters"){
    
    filenm <- "fltrs_var_profiles_by_id" 
    
  }
  
  if (exists(filenm) && !is.null(filenm) && str_detect(data_type, ("observations|fieldvisits|specimens|samplinglocations"))) {
    
    #use the current list of ids to filter out the ones that are now deleted
    updated_ids <- tibble(id = get_ids(data_type))
    
    #filter the current file for removed ids deleted in the online database
    current_file <- get(filenm) %>% inner_join(updated_ids, by = "id")
    
    #updating the relevant list with new data
    updated_file <- update_var(data_type, current_file)
    
  } else {
    
    updated_file <- get_var_profile_by_id(data_type, "all")
    
  }
  
  return(updated_file)
  
}

spcmns_var_profiles_by_id <- gen_updated_data_file("specimens")

obs_var_profiles_by_id <- gen_updated_data_file("observations")

vsts_var_profiles_by_id <- gen_updated_data_file("fieldvisits")

locs_var_profiles_by_id <- gen_updated_data_file("samplinglocations")

# getting user id associated with a username -------------------------

get_user_id_by_name <- function(frstnm, lstnm){
  
  #first let's try to get a list of all users; perhaps some info there
  
  #get user lists out (for editing)
  url <- str_c(base_url, "v2/users/")
  data_body <- list()
  
  x_usrs <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  
  #names(fromJSON(rawToChar(x_usrs$content))$domainObjects)
  
  usrs <- fromJSON(rawToChar(x_usrs$content))$domainObjects %>% select(id, customId, userProfile, accessGroups) %>% unnest_wider(userProfile, names_repair = "universal")

  usr_id <- usrs %>% dplyr::filter(firstName == frstnm & lastName == lstnm) %>% dplyr::select(id...3) %>% unlist()
  
  return(usr_id)
  
}

usr_id <- get_user_id_by_name("Sahil", "Bhandari")

# finding id of a specific data type associated with a username --------------------------------
get_id_by_user_profile <- function(frstnm, lstnm, data_type){
  
  usr_id <- get_user_id_by_name(frstnm, lstnm)
  
  data_full <- gen_updated_data_file(data_type)
  
  data_select <- data_full %>% as_tibble() %>% 
    dplyr::select(id, auditAttributes.creationUserProfileId) %>% 
    dplyr::filter(auditAttributes.creationUserProfileId == usr_id) %>% 
    dplyr::select(id) %>% unlist()
  
  return(data_select)
  
}

spcmns_id_del <- get_id_by_user_profile("Sahil", "Bhandari", "specimens")

analyticalgrps_id_del <- get_id_by_user_profile("Sahil", "Bhandari", "analyticalgroups")

obs_id_del <- get_id_by_user_profile("Sahil", "Bhandari", "observations")

actvty_id_del <- get_id_by_user_profile("Sahil", "Bhandari", "activities")

vsts_id_del <- get_id_by_user_profile("Sahil", "Bhandari", "fieldvisits")

prjcts_id_del <- get_id_by_user_profile("Sahil", "Bhandari", "projects")

locs_id_del <- get_id_by_user_profile("Sahil", "Bhandari", "samplinglocations")

fltrs_id_del <- get_id_by_user_profile("Sahil", "Bhandari", "filters")

# finding ids of observations associated with a given field activity --------
get_var_id_by_var <- function(data_type_ref_var, data_type_ref_var_id, data_type_var_new){
  
  if(data_type_ref_var == "activities"){
    
    if(data_type_var_new == "observations"){
      
      #get all observation IDs out (for editing)
      url <- str_c(base_url, "v2/observations?limit=1000&activityIds=", data_type_ref_var_id)
      
    } else if(data_type_var_new == "specimens"){
      
      #get all observation IDs out (for editing)
      url <- str_c(base_url, "v1/specimens?limit=1000&activityIds=", data_type_ref_var_id)
      
    }
    
  }
  
  if(data_type_ref_var == "analyticalgroups"){
    
    if(data_type_var_new == "specimens") {
      
      #get all observation IDs out (for editing)
      url <- str_c(base_url, "v1/specimens?limit=1000&analyticalGroupIds=", data_type_ref_var_id) 
      
    }
    
  }
  
  # data_body <- list()
  # 
  # data_select <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), 
  #                    body = data_body, encode = 'json')
  # 
  # var_select <- fromJSON(rawToChar(data_select$content))$domainObjects
  # 
  # var_select_ids <- var_select$id
  
  var_select_ids <- get_ids_for_url(url)
  
  return(var_select_ids)  
  
}

obs_id_del <- get_var_id_by_var("activities", actvty_id_del, "observations")

spcmns_id_del <- get_var_id_by_var("analyticalgroups", analyticalgrps_id_del, "specimens")

spcmns_id_del <- get_var_id_by_var("activities", "a6041d8b-6e34-40d4-965e-7cea218b5630", "specimens")

# finding ids of observations associated with a given field visit --------
get_var_id_by_vsts <- function(data_type, vsts_id){
  
  if(data_type == "observations") {
    
    #get all observation IDs out (for editing)
    url <- str_c(base_url, "v2/observations?limit=1000&fieldVisitId=", vsts_id) 
    
  } else {
  
    #get all observation IDs out (for editing)
    url <- str_c(base_url, "v1/", data_type, "?limit=1000&fieldVisitId=", vsts_id)   
    
  }
  
  # data_body <- list()
  # 
  # data_select <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), 
  #               body = data_body, encode = 'json')
  # 
  # var_select <- fromJSON(rawToChar(data_select$content))$domainObjects
  # 
  # var_select_ids <- var_select$id
  # 
  # return(var_select_ids)  
  
  var_select_ids <- get_ids_for_url(url)
    
  return(var_select_ids)
  
}

actvty_id_del <- get_var_id_by_vsts("activities", vsts_id_del)

obs_id_del <- get_var_id_by_vsts("observations", vsts_id_del)

# finding ids of field visits/observations associated with a given location --------
get_var_id_by_locs <- function(data_type, locs_ids){
  
  if(length(locs_ids) == 1) {
  
   if(data_type == "observations"){
    
    #get all observation IDs out (for editing)
    url <- str_c(base_url, "v2/observations?limit=1000&samplingLocationIds=", locs_ids)
    
    } else {
      
      url <- str_c(base_url, "v1/", data_type, "?limit=1000&samplingLocationIds=", locs_ids)
      
    }
    
  } else {
    
      var_select_ids <- c()
      
      for(locs_id in locs_ids){
        
        var_select_ids <- c(var_select_ids, get_var_id_by_locs(data_type, locs_id))
        
      }
      
      return(var_select_ids)
      
    }
    
    #print("This coding pipeline currently runs only for observations and field visits.")
    #return("This coding pipeline currently runs only for observations and field visits.")
  
  # data_body <- list()
  # 
  # data_select <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), 
  #                    body = data_body, encode = 'json')
  # 
  # var_select <- fromJSON(rawToChar(data_select$content))$domainObjects
  # 
  # var_select_ids <- var_select$id
  
  var_select_ids <- get_ids_for_url(url)
  
  return(var_select_ids)  
  
}

#locs_id_del <- "ca75e340-e1ba-451e-8fc7-531a61036279"

obs_id_del <- get_var_id_by_locs("observations", locs_id_del)

vsts_id_del <- get_var_id_by_locs("fieldvisits", locs_id_del)

# finding ids of activities/field visits/observations associated with a given project --------
get_var_id_by_prjcts <- function(data_type, prjcts_ids){
  
  if(length(prjcts_ids) == 1) {
    
    if(data_type == "fieldvisits"){
      
      #get all observation IDs out (for editing)
      url <- str_c(base_url, "v1/fieldvisits?limit=1000&projectIds=", prjcts_ids)
      
    } else if(data_type == "observations"){
      
      #get all observation IDs out (for editing)
      url <- str_c(base_url, "v2/observations?limit=1000&projectIds=", prjcts_ids)
      
    } else if(data_type == "activities"){
      
      #get all observation IDs out (for editing)
      url <- str_c(base_url, "v1/activities?limit=1000&projectIds=", prjcts_ids)
      
    } 
    
  } else {
    
    var_select_ids <- c()
    
    for(prjcts_id in prjcts_ids){
      
      var_select_ids <- c(var_select_ids, get_var_id_by_prjcts(data_type, prjcts_id))
      
    }
    
    return(var_select_ids)
    
  }
  
  #print("This coding pipeline currently runs only for observations and field visits.")
  #return("This coding pipeline currently runs only for observations and field visits.")
  
  data_body <- list()
  
  data_select <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), 
                     body = data_body, encode = 'json')
  
  var_select <- fromJSON(rawToChar(data_select$content))$domainObjects
  
  var_select_ids <- var_select$id
  
  return(var_select_ids)  
  
}

#locs_id_del <- "ca75e340-e1ba-451e-8fc7-531a61036279"

obs_id_del <- get_var_id_by_prjcts("observations", prjcts_id_del)

actvty_id_del <- get_var_id_by_prjcts("activities", prjcts_id_del)

vsts_id_del <- get_var_id_by_prjcts("fieldvisits", prjcts_id_del)

# finding ids of locations associated with a given saved filter -----------

get_locs_id_by_fltr <- function(fltr_id){
  
  make_api_request(fltr_id, "filters")
  
  locs_id <- make_api_request(fltr_id, "filters")$samplingLocations$id
  
}

locs_id_del <- get_locs_id_by_fltr(fltrs_id_del)

# deleting data of a specific data type with specific id -----------------------------------
del_data_by_id <- function(ids, data_type){
  
  i = 1
  
  if(length(ids) == 1){
    
    if(data_type == "observations"){
      
      url <- str_c(base_url, "v2/", data_type, "/", ids)
      
    } else if (data_type == "specimens") {
      
      url <- str_c(base_url, "v1/", data_type, "/", ids)
      
    } else if (data_type == "analyticalgroups") {
      
      #first need to get all observations associated with this activity
      spcmns_id_del <- get_var_id_by_var("analyticalgroups", ids, "specimens")
      
      if(!is.null(spcmns_id_del) & !all(is.na(spcmns_id_del))){
        
        #next need to delete all observations associated with this field visit
        del_data_by_id(spcmns_id_del, "specimens")
        
      }
      
      url <- str_c(base_url, "v1/", data_type, "/", ids)
      
    } else if (data_type == "activities"){
      
      #first need to get all observations associated with this activity
      obs_id_del <- get_var_id_by_var("activities", ids, "observations")
      
      #need to delete all specimen related to this activity
      spcmns_id_del <- get_var_id_by_var("activities", ids, "specimens")
      
      if(!is.null(obs_id_del) & !all(is.na(obs_id_del))){
      
      #next need to delete all observations associated with this field visit
      del_data_by_id(obs_id_del, "observations")
        
      }
      
      if(!is.null(spcmns_id_del) & !all(is.na(spcmns_id_del))){
        
        #next need to delete all observations associated with this field visit
        del_data_by_id(spcmns_id_del, "specimens")
        
      }
      
      url <- str_c(base_url, "v1/", data_type, "/", ids)
      
      #print(url)
      
    } else if (data_type == "fieldvisits"){
      
      #first need to get all observations associated with this field visit
      obs_id_del <- get_var_id_by_vsts("observations", ids)
      
      #need to delete all specimen related to this activity
      spcmns_id_del <- get_var_id_by_vsts("specimens", ids)
      
      if(!is.null(obs_id_del) & !all(is.na(obs_id_del))){
      
      #next need to delete all observations associated with this field visit
      del_data_by_id(obs_id_del, "observations")
        
      }
      
      if(!is.null(spcmns_id_del) & !all(is.na(spcmns_id_del))){
        
        #next need to delete all observations associated with this field visit
        del_data_by_id(spcmns_id_del, "specimens")
        
      }
      
      #first need to get all activities associated with this field visit
      actvty_id_del <- get_var_id_by_vsts("activities", ids)
      
      if(!is.null(actvty_id_del) & !all(is.na(actvty_id_del))){
      
      #next need to delete all activities associated with this field visit
      del_data_by_id(actvty_id_del, "activities")
        
      }
      
      url <- str_c(base_url, "v1/", data_type, "/", ids)
      
    } else if (data_type == "projects"){
      
      #first need to get all observations associated with this field visit
      obs_id_del <- get_var_id_by_prjcts("observations", ids)
      
      #print(obs_id_del)
      
      if(!is.null(obs_id_del) & !all(is.na(obs_id_del))){
      
      #next need to delete all observations associated with this field visit
      del_data_by_id(obs_id_del, "observations")
        
      }
      
      #first need to get all activities associated with this field visit
      actvty_id_del <- get_var_id_by_prjcts("activities", ids)
      
      #print(actvty_id_del)
      
      if(!is.null(actvty_id_del) & !all(is.na(actvty_id_del))){
      
      #next need to delete all activities associated with this field visit
      del_data_by_id(actvty_id_del, "activities")
        
      }
      
      #next need to get all field visits associated with this location
      vsts_id_del <- get_var_id_by_prjcts("fieldvisits", ids)
      
      #print(vsts_id_del)
      
      if(!is.null(vsts_id_del) & !all(is.na(vsts_id_del))){
      
      #next need to delete all field visits associated with this location
      del_data_by_id(vsts_id_del, "fieldvisits")
        
      }
      
      url <- str_c(base_url, "v1/", data_type, "/", ids)
      
    } else if (data_type == "samplinglocations"){
      
      #first need to get all observations associated with this location
      obs_id_del <- get_var_id_by_locs("observations", ids)
      
      #first need to get all observations associated with this location
      spcmns_id_del <- get_var_id_by_locs("specimens", ids)
      
      #first need to get all activities associated with this field visit
      actvty_id_del <- get_var_id_by_locs("activities", ids)
      
      #next need to get all field visits associated with this location
      vsts_id_del <- get_var_id_by_locs("fieldvisits", ids)
      
      ##next need to get all field visits associated with this location
      #prjcts_id_del <- get_var_id_by_locs("projects", ids)
      
      if(!is.null(obs_id_del) & !all(is.na(obs_id_del))){

      #first need to delete all observations associated with this location
      del_data_by_id(obs_id_del, "observations")
        
      }
      
      if(!is.null(spcmns_id_del) & !all(is.na(spcmns_id_del))){
        
        #next need to delete all observations associated with this field visit
        del_data_by_id(spcmns_id_del, "specimens")
        
      }
      
      if(!is.null(actvty_id_del) & !all(is.na(actvty_id_del))){
      
      #next need to delete all activities associated with this field visit
      del_data_by_id(actvty_id_del, "activities")
        
      }
      
      if(!is.null(vsts_id_del) & !all(is.na(vsts_id_del))){
        
      #next need to delete all field visits associated with this location
      del_data_by_id(vsts_id_del, "fieldvisits")
        
      }
      
      #if(!is.null(prjcts_id_del) & !all(is.na(prjcts_id_del))){
        
      ##next need to delete all field visits associated with this location
      #del_data_by_id(prjcts_id_del, "projects")
        
      #}
      
      url <- str_c(base_url, "v1/", data_type, "/", ids)
      
    } else if (data_type == "filters"){
      
      locs_id <- get_locs_id_by_fltr(ids)
      
      if(!is.null(locs_id) & !all(is.na(locs_id))){
          
        for(locs_id_del in locs_id){
        
          del_data_by_id(locs_id_del, "samplinglocations")
        
        }
        
      }
      
      url <- str_c(base_url, "v1/", data_type, "/", ids)
  
    }
    
  } else {
    
    for(id in ids){
      
      print(i)
      
      del_data_by_id(id, data_type)
      
      i = i + 1
      
    }
    
    return()
    
  }
    
    # else {
    #   
    #   print("The coding pipeline has only been developed for observations and field visits.")
    #   
    #   return("The coding pipeline has only been developed for observations and field visits.")
    #}
  
  data_body <- list()

  #Deleting the relevant data
  x_del <- DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  
  # k = 1
  # 
  # print(k)
  # 
  #x_del <- del_ids_for_url(url)
  
  return(x_del)
  
} 

x_actvty_del <- del_data_by_id(actvty_id_del, "activities")

x_vsts_del <- del_data_by_id(vsts_id_del, "fieldvisits")

#check if the data was actually deleted
#vsts_del <- fromJSON(rawToChar(x_vsts_del$content))

x_prjcts_del <- del_data_by_id(prjcts_id_del, "projects")

x_locs_del <- del_data_by_id(locs_id_del, "samplinglocations")

x_spcmns_del <- del_data_by_id(spcmns_id_del, "specimens")

x_analyticalgrps_del <- del_data_by_id(analyticalgrps_id_del, "analyticalgroups")

x_fltrs_del <- del_data_by_id(fltrs_id_del, "filters")

# deleting data associated with a specific user ---------------------------
del_data_by_user <- function(frstnm, lstnm){
  
  #first get user id associated with name
  usr_id <- get_user_id_by_name(frstnm, lstnm)
  
  #could write a section for specimens but that will be deleted too
  
  ##next get field visit activity data associated with name
  actvty_id_del <- get_id_by_user_profile(frstnm, lstnm, "activities")
  
  #next delete all activities 
  x_actvty_del <- del_data_by_id(actvty_id_del, "activities")
  
  #next get field visit data associated with name
  vsts_id_del <- get_id_by_user_profile(frstnm, lstnm, "fieldvisits")
  
  #next delete everything within the relevant field visits (incl. observations) 
  x_vsts_del <- del_data_by_id(vsts_id_del, "fieldvisits")
  
  #next get project data associated with name
  prjcts_id_del <- get_id_by_user_profile(frstnm, lstnm, "projects")
  
  #delete projects
  x_prjcts_del <- del_data_by_id(prjcts_id_del, "projects")
  
  #next get location data associated with name
  locs_id_del <- get_id_by_user_profile(frstnm, lstnm, "samplinglocations")
  
  #next delete sampling locations associated with this user
  x_locs_del <- del_data_by_id(locs_id_del, "samplinglocations")
  
  #next get saved filter data associated with name
  fltrs_id_del <- get_id_by_user_profile(frstnm, lstnm, "savedfilters")
  
  #delete all saved filters associated with this user
  x_fltrs_del <- del_data_by_id(fltrs_id_del, "savedfilters")
  
  return(x_locs_del)
}

x_usr_del <- del_data_by_user("Sahil", "Bhandari")

usr_del <- fromJSON(rawToChar(x_usr_del$content))


# Using the Export feature in Swagger -------------------------------------
start_time <- proc.time()

obs_ids <- get_ids("observations")

obs_test_data <- list()

j <- 1

j_max <- ceiling(length(obs_ids)/197)

for(j in 1:j_max){
  
  obs_test_data[[j]] <- obs_ids[(j-1)*197+1]
  
  i <- 2
  
  while(i <= 197) {
    
    obs_test_data[[j]] <- str_c(obs_test_data[[j]], ",", obs_ids[(j-1)*197+i])
    
    i = i + 1
    
  }
  
  j = j + 1
  
  #print(j)
  
}

temp_data <- tibble() 

for(i in 1:length(obs_test_data)){#length(obs_test_data)

#export_obs_data <- function(){
  
url <- str_c(base_url, "v2/services/export/observations?ids=", obs_test_data[[i]])
#str_c(obs_ids[1], ",", obs_ids[2])

data_body <- list()

x_observations <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), 
              body = data_body, encode = "json")

#mdfd_var <- fromJSON(rawToChar(x_mdfd$content))$domainObjects

#mdfd_var_ids <- mdfd_var$id

temp_data <- bind_rows(temp_data, read_csv(rawToChar(x_observations$content), show_col_types = FALSE, col_types = cols(`Activity Name` = col_character())))

#str(temp_data)

#print(temp_data)

#print(i)

}

end_time <- proc.time()

elapsed_time <- end_time - start_time

print(elapsed_time)

# total = fromJSON(rawToChar(x_observations$content))$totalCount
# 
# print(total)
# 
# if (total > 1000) { #if there are more than 1000 records loop
#   
#   # locs <- fromJSON(rawToChar(x_mdfd$content))$domainObjects %>%
#   #   tibble::rownames_to_column("original_row_name") %>% as_tibble()
#   #mdfd_var <- fromJSON(rawToChar(x_mdfd$content))$domainObjects %>% as_tibble() #%>% 
#   #dplyr::select(id, customId, name, auditAttributes, type, latitude, 
#   #              longitude, horizontalCollectionMethod, description) %>% 
#   #unnest_wider(type, names_repair = "universal") %>% 
#   #unnest_wider(auditAttributes, names_repair = "universal")
#   
#   var_ids <- fromJSON(rawToChar(x_observations$content))$domainObjects$id
#   
#   number_loops = ceiling(total/1000)
#   
#   #i = 2
#   
#   for (i in seq(2,number_loops)) {
#     cursor = fromJSON(rawToChar(x_observations$content))$cursor
#     tempURL = paste0(url, "&cursor=", cursor)
#     
#     x_observations <- GET(tempURL, config = c(add_headers(.headers = c('Authorization' = token))), 
#                   body = data_body, encode = 'json')
#     
#     temp_var_ids <- fromJSON(rawToChar(x_observations$content))$domainObjects$id 
#     
#     # #temp_locs <- fromJSON(rawToChar(x_mdfd$content))$domainObjects %>%
#     # #  tibble::rownames_to_column("original_row_name") %>% as_tibble()
#     # temp_mdfd_var <- fromJSON(rawToChar(x_mdfd$content))$domainObjects %>% 
#     #   as_tibble() #%>% dplyr::select(id, customId, name, auditAttributes, 
#     # type, latitude, longitude, horizontalCollectionMethod, description) %>% 
#     #   #unnest_wider(type, names_repair = "universal") %>% 
#     #   #unnest_wider(auditAttributes, names_repair = "universal")
#     
#     var_ids <- append(var_ids, temp_var_ids)
#     
#     #rownames(temp_mdfd_var) <- NULL
#     
#     #rownames(mdfd_var) <- NULL
#     
#     #mdfd_var <- bind_rows(mdfd_var, temp_mdfd_var)
#     
#     print(i)
#   }
#   
# } else {
#   
#   #mdfd_var <- fromJSON(rawToChar(x_mdfd$content))$domainObjects %>% as_tibble() #%>% 
#   #dplyr::select(id, customId, name, auditAttributes, type, latitude, longitude,   horizontalCollectionMethod, description) %>% 
#   #unnest_wider(type, names_repair = "universal") %>% 
#   #unnest_wider(auditAttributes, names_repair = "universal")
#   
#   var_ids <- fromJSON(rawToChar(x_observations$content))$domainObjects$id
#   
# }
# 
# return(var_ids)
# 
# }
# 
# obs_ids_exported <- export_obs_data()
# 
# end_time <- proc.time()
# 
# elapsed_time <- end_time - start_time
# print(elapsed_time)

# bringing in watershed group data--------
# bcdc_search("Freshwater atlas watershed groups") 
# bcdc_get_record("51f20b1a-ab75-42de-809d-bf415a0f9c62")
# bcdc_tidy_resources('51f20b1a-ab75-42de-809d-bf415a0f9c62')
# bcdc_freshwater_atlas_watershed_groups <- bcdc_query_geodata("51f20b1a-ab75-42de-809d-bf415a0f9c62") %>%
#   collect()
# plot(st_geometry(bcdc_freshwater_atlas_watershed_groups))
# 
# # Plot using ggplot2
# p2 <- ggplot() +
#   geom_sf(data = bcdc_freshwater_atlas_watershed_groups, fill = "blue", alpha = 0.5) +
#   #geom_sf(data = test_locs_var_profiles_by_id, fill = "red", alpha = 0.5) +
#   ggtitle("WATERSHED GROUPS")
# 
# ggsave("./plots/watershed_groups.png", p2, width = 4, height = 6, units = "in")

# url <- str_c(base_url, "v2/services/export/observations?cursor=1&limit=100")
# 
# data_body <- list()
# 
# x_observations <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), 
#                       body = data_body, encode = "json")
# 
# temp_data <- fromJSON(rawToChar(x_observations$content))

# preparing location data frame vA for ENMODS mapping------------
test_locs_var_profiles_by_id <- locs_var_profiles_by_id

# #Adding a placeholder description column that will be added in the future
# test_locs_var_profiles_by_id <- test_locs_var_profiles_by_id %>%
#   mutate(description = "")

# Removing irrelevant rows
# Removing lat/lon that are either blanks or NA
# Use case_when inside mutate to create a new 'Location Type (Broad)'
test_locs_var_profiles_by_id <- test_locs_var_profiles_by_id %>% 
  dplyr::filter(!(str_detect(`customId`, "Sahil")|str_detect(`customId`, "Manawat")|str_detect(`customId`, "Krogh"))) %>%
  dplyr::filter(!(is.na(latitude)|is.na(longitude))) %>%
  dplyr::filter(!(latitude == ""|longitude == "")) %>%
  dplyr::select(-attachments.attachment.comment) %>%
  mutate(type.customId = stringr::str_to_title(type.customId)) %>%
  mutate(type.customId = str_replace_all(type.customId, "\\b(And|Or)\\b", function(x) tolower(x))) %>%
  mutate(description = stringr::str_to_title(description)) %>%
  mutate(description = str_replace_all(description, "\\b(And|Or|To|Into|From|The|At|A|On|Of|Above|Below|With|Beyond|About|Across|After|Against|Along|Among|Around|As|Before|Behind|Beneath|Beside|Between|By|Down|During|Except|For|From|In|Inside|Into|Like|Near|Off|Onto|Out|Outside|Over|Past|Since|Through|Throughout|To|Toward|Under|Underneath|Until|Up|Upon|With|Within|Without)\\b", function(x) tolower(x))) %>%
  mutate(type.customId = if_else(type.customId == "River,Stream or Creek",
                                 "River, Stream, or Creek", type.customId  
  )) %>%
  mutate(
    `Location Type (Broad)` = case_when(
      type.customId == "River, Stream, or Creek" ~ "Water",
      #type.customId == toupper("River, Stream, or Creek") ~ "Water",
      type.customId == "River,Stream or Creek" ~ "Water",
      #type.customId == toupper("River,Stream or Creek") ~ "Water",
      type.customId == "Lake or Pond" ~ "Water",
      #type.customId == toupper("Lake or Pond") ~ "Water",
      type.customId == "Well" ~ "Water",
      #type.customId == toupper("Well") ~ "Water",
      type.customId == "Marine" ~ "Water",
      #type.customId == toupper("Marine") ~ "Water",
      type.customId == "Infiltration Pond" ~ "Water",
      #type.customId == toupper("Infiltration Pond") ~ "Water",
      type.customId == "Septic Tank" ~ "Water",
      #type.customId == toupper("Septic Tank") ~ "Water",
      type.customId == "Sanitary Sewer" ~ "Water",
      type.customId == "Storm Sewer" ~ "Water",
      #type.customId == toupper("Storm Sewer") ~ "Water",
      type.customId == "Stormsewer" ~ "Water",
      #type.customId == toupper("Stormsewer") ~ "Water",
      type.customId == "Combined Sewer" ~ "Water",
      #type.customId == toupper("Combined Sewer") ~ "Water",
      type.customId == "Spring or Hot Spring" ~ "Water",
      #type.customId == toupper("Spring or Hot Spring") ~ "Water",
      type.customId == "Outfall" ~ "Water",
      #type.customId == toupper("Outfall") ~ "Water",
      type.customId == "Air Quality" ~ "Air",
      #type.customId == toupper("Air Quality") ~ "Air",
      type.customId == "Stack" ~ "Air",
      #type.customId == toupper("Stack") ~ "Air",
      type.customId == "Landfill" ~ "Land",
      #type.customId == toupper("Landfill") ~ "Land",
      type.customId == "Ditch or Culvert" ~ "Land",
      #type.customId == toupper("Ditch or Culvert") ~ "Land",
      type.customId == "Seepage or Seepage Pools" ~ "Land",
      #type.customId == toupper("Seepage or Seepage Pools") ~ "Land",
      type.customId == "Open Burning" ~ "Land",
      #type.customId == toupper("Open Burning") ~ "Land",
      type.customId == "Tile Field" ~ "Land",
      #type.customId == toupper("Tile Field") ~ "Land",
      type.customId == "Irrigation Spray/Sludge" ~ "Land",
      #type.customId == toupper("Irrigation Spray/Sludge") ~ "Land",
      type.customId == "Terrestrial" ~ "Land",
      #type.customId == toupper("Terrestrial") ~ "Land",
      type.customId == "Estuary" ~ "Land",
      #type.customId == toupper("Estuary") ~ "Land",
      type.customId == "Estuarine" ~ "Land",
      #type.customId == toupper("Estuarine") ~ "Land",
      type.customId == "Land - Farm" ~ "Land",
      #type.customId == toupper("Land - Farm") ~ "Land",
      TRUE ~ "Other"  # Default case (like else)
    )
  ) %>% mutate(type.customId = if_else(!is.na(type.customId), paste0(toupper(substr(type.customId, 1, 1)), substr(type.customId, 2, nchar(type.customId))), type.customId),
               description = if_else(!is.na(description), paste0(toupper(substr(description, 1, 1)), substr(description, 2, nchar(description))), description) 
  ) %>% rename(`Location GUID` = id, 
               `Location Identifier` = customId,
               `Location Name` = name, 
               `Creator GUID` = auditAttributes.creationUserProfileId,
               `Location Type (Detailed)` = type.customId,
               `Location Description` = description,
               Latitude = latitude,
               Longitude = longitude,
               `Spatial Data Source` = horizontalCollectionMethod,
               `Location Group GUID` = samplingLocationGroups.id,
               `Location Group Name` = samplingLocationGroups.name,
               `Location Group Type` = samplingLocationGroups.locationGroupType.customId,
               `Location Group Description` = samplingLocationGroups.description,
               `Number of Observations` = observationCount,
               `Number of Field Visits` = fieldVisitCount,
               `Elevation (in metre)` = elevation.value,
               `Elevation Unit`= elevation.unit.customId,
               `Latest Field Visit GUID` = latestFieldVisit.id,
               `Latest Field Visit Date/Time` = latestFieldVisit.startTime)

test_locs_var_profiles_by_id <- test_locs_var_profiles_by_id %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = "EPSG:4326", remove = FALSE) %>%
  st_transform(crs = "EPSG:3005") 

# # Plot using ggplot2
# p1 <- ggplot() +
#   #geom_sf(data = bcdc_freshwater_atlas_watershed_groups, fill = "blue", alpha = 0.5) +
#   geom_sf(data = test_locs_var_profiles_by_id, fill = "red", alpha = 0.5) +
#   ggtitle("ENMODS POINTS")
# 
# ggsave("./plots/enmods_points.png", p1, width = 4, height = 6, units = "in")


# preparing location data frame vB for ENMODS mapping------------
test_locs_var_profiles_by_id_vB <- locs_var_profiles_by_id

# #Adding a placeholder description column that will be added in the future
# test_locs_var_profiles_by_id <- test_locs_var_profiles_by_id %>%
#   mutate(description = "")

# Removing irrelevant rows
# Removing lat/lon that are either blanks or NA
# Use case_when inside mutate to create a new 'Location Type (Broad)'
# test_locs_var_profiles_by_id_vB_backup <- test_locs_var_profiles_by_id_vB

test_locs_var_profiles_by_id_vB <- test_locs_var_profiles_by_id_vB %>% 
  dplyr::filter(!(str_detect(`customId`, "Sahil")|str_detect(`customId`, "Manawat")|str_detect(`customId`, "Krogh"))) %>%
  dplyr::filter(!(is.na(latitude)|is.na(longitude))) %>%
  dplyr::filter(!(latitude == ""|longitude == "")) %>%
  #dplyr::select(-attachments.attachment.comment) %>%
  mutate(type.customId = stringr::str_to_title(type.customId)) %>%
  mutate(type.customId = str_replace_all(type.customId, "\\b(And|Or)\\b", function(x) tolower(x))) %>%
  mutate(description = stringr::str_to_title(description)) %>%
  mutate(description = str_replace_all(description, "\\b(And|Or|To|Into|From|The|At|A|On|Of|Above|Below|With|Beyond|About|Across|After|Against|Along|Among|Around|As|Before|Behind|Beneath|Beside|Between|By|Down|During|Except|For|From|In|Inside|Into|Like|Near|Off|Onto|Out|Outside|Over|Past|Since|Through|Throughout|To|Toward|Under|Underneath|Until|Up|Upon|With|Within|Without)\\b", function(x) tolower(x))) %>%
  mutate(type.customId = if_else(type.customId == "River,Stream or Creek",
                                 "River, Stream, or Creek", type.customId  
  )) %>%
  mutate(
    `Location Type (Broad)` = case_when(
      type.customId == "River, Stream, or Creek" ~ "Water",
      #type.customId == toupper("River, Stream, or Creek") ~ "Water",
      type.customId == "River,Stream or Creek" ~ "Water",
      #type.customId == toupper("River,Stream or Creek") ~ "Water",
      type.customId == "Lake or Pond" ~ "Water",
      #type.customId == toupper("Lake or Pond") ~ "Water",
      type.customId == "Well" ~ "Water",
      #type.customId == toupper("Well") ~ "Water",
      type.customId == "Marine" ~ "Water",
      #type.customId == toupper("Marine") ~ "Water",
      type.customId == "Infiltration Pond" ~ "Water",
      #type.customId == toupper("Infiltration Pond") ~ "Water",
      type.customId == "Septic Tank" ~ "Waste",
      #type.customId == toupper("Septic Tank") ~ "Waste",
      type.customId == "Sanitary Sewer" ~ "Waste",
      type.customId == "Storm Sewer" ~ "Waste",
      #type.customId == toupper("Storm Sewer") ~ "Waste",
      type.customId == "Stormsewer" ~ "Waste",
      #type.customId == toupper("Stormsewer") ~ "Waste",
      type.customId == "Combined Sewer" ~ "Waste",
      #type.customId == toupper("Combined Sewer") ~ "Waste",
      type.customId == "Spring or Hot Spring" ~ "Water",
      #type.customId == toupper("Spring or Hot Spring") ~ "Water",
      type.customId == "Outfall" ~ "Waste",
      #type.customId == toupper("Outfall") ~ "Waste",
      type.customId == "Air Quality" ~ "Air",
      #type.customId == toupper("Air Quality") ~ "Air",
      type.customId == "Stack" ~ "Waste",
      #type.customId == toupper("Stack") ~ "Waste",
      type.customId == "Landfill" ~ "Land",
      #type.customId == toupper("Landfill") ~ "Land",
      type.customId == "Ditch or Culvert" ~ "Land",
      #type.customId == toupper("Ditch or Culvert") ~ "Land",
      type.customId == "Seepage or Seepage Pools" ~ "Land",
      #type.customId == toupper("Seepage or Seepage Pools") ~ "Land",
      type.customId == "Open Burning" ~ "Land",
      #type.customId == toupper("Open Burning") ~ "Land",
      type.customId == "Tile Field" ~ "Land",
      #type.customId == toupper("Tile Field") ~ "Land",
      type.customId == "Irrigation Spray/Sludge" ~ "Waste",
      #type.customId == toupper("Irrigation Spray/Sludge") ~ "Waste",
      type.customId == "Terrestrial" ~ "Land",
      #type.customId == toupper("Terrestrial") ~ "Land",
      type.customId == "Estuary" ~ "Land",
      #type.customId == toupper("Estuary") ~ "Land",
      type.customId == "Estuarine" ~ "Land",
      #type.customId == toupper("Estuarine") ~ "Land",
      type.customId == "Land - Farm" ~ "Land",
      #type.customId == toupper("Land - Farm") ~ "Land",
      TRUE ~ "Other"  # Default case (like else)
    )
  ) %>% mutate(type.customId = if_else(!is.na(type.customId), paste0(toupper(substr(type.customId, 1, 1)), substr(type.customId, 2, nchar(type.customId))), type.customId),
               description = if_else(!is.na(description), paste0(toupper(substr(description, 1, 1)), substr(description, 2, nchar(description))), description) 
  ) %>% rename(`Location GUID` = id, 
               `Location Identifier` = customId,
               `Location Name` = name, 
               `Creator GUID` = auditAttributes.creationUserProfileId,
               `Location Type (Detailed)` = type.customId,
               `Location Description` = description,
               Latitude = latitude,
               Longitude = longitude,
               `Spatial Data Source` = horizontalCollectionMethod,
               `Location Group GUID` = samplingLocationGroups.id,
               `Location Group Name` = samplingLocationGroups.name,
               `Location Group Type` = samplingLocationGroups.locationGroupType.customId,
               `Location Group Description` = samplingLocationGroups.description,
               `Number of Observations` = observationCount,
               `Number of Field Visits` = fieldVisitCount,
               `Elevation (in metre)` = elevation.value,
               `Elevation Unit`= elevation.unit.customId,
               `Latest Field Visit GUID` = latestFieldVisit.id,
               `Latest Field Visit Date/Time` = latestFieldVisit.startTime)

test_locs_var_profiles_by_id_vB <- test_locs_var_profiles_by_id_vB %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = "EPSG:4326", remove = FALSE) %>%
  st_transform(crs = "EPSG:3005")

test_locs_var_profiles_by_id_vB <- test_locs_var_profiles_by_id_vB %>% st_join(bcdc_freshwater_atlas_watershed_groups %>% dplyr::select(-(AREA_HA:FEATURE_LENGTH_M)), join = st_intersects, left = FALSE)

test_locs_var_profiles_by_id_vB <- test_locs_var_profiles_by_id_vB %>%
  rename(`Watershed Group GUID` = id,
         `Watershed Group ID` = WATERSHED_GROUP_ID, 
         `Watershed Group Code` = WATERSHED_GROUP_CODE,
         `Watershed Group Name` = WATERSHED_GROUP_NAME
  )

# #fixing dates to be imported into ArcGIS
# test_locs_var_profiles_by_id <- test_locs_var_profiles_by_id %>%
#   mutate(`Latest Field Visit Date/Time` = ifelse(is.na(`Latest Field Visit Date/Time`), "0001-01-01T00:13:00.000-08:00", `Latest Field Visit Date/Time`))

#https://doc.arcgis.com/en/arcgis-online/manage-data/work-with-date-fields.htm
test_locs_var_profiles_by_id_vB$`Latest Field Visit Date/Time`<- ymd_hms(test_locs_var_profiles_by_id_vB$`Latest Field Visit Date/Time`, tz = "Etc/GMT+8")

test_locs_var_profiles_by_id_vB$`Latest Field Visit Date/Time`<- test_locs_var_profiles_by_id_vB$`Latest Field Visit Date/Time` %>% 
  format("%Y/%m/%d %H:%M:%S")

#Using below, realized that ArcGIS does not like NAs
# test_date_arcgis_import <- test_locs_var_profiles_by_id %>% 
#   dplyr::filter(!is.na(`Latest Field Visit Date/Time`))
# 
# write.csv(test_date_arcgis_import, "test_date_arcgis_import.csv")

# Replace all NA values with an empty string ""
test_locs_var_profiles_by_id_vB <- test_locs_var_profiles_by_id_vB %>%
  mutate(across(everything(), ~ replace(., is.na(.), "")))

# test_locs_var_profiles_by_id <- test_locs_var_profiles_by_id %>%
#   mutate(`Latest Field Visit Date/Time` = ifelse(is.na(`Latest Field Visit Date/Time`), "", `Latest Field Visit Date/Time`))

test_locs_var_profiles_by_id_vB <- test_locs_var_profiles_by_id_vB %>% st_set_geometry(NULL)

write.csv(test_locs_var_profiles_by_id_vB, str_c("location_enmods_watersheds_R_export_", today(), ".csv"), row.names = FALSE)

test_locs_data_var_profiles_by_id_vB <- test_locs_var_profiles_by_id_vB %>% dplyr::filter(`Latest Field Visit Date/Time`!="")

write.csv(test_locs_data_var_profiles_by_id_vB, str_c("location_enmods_data_watersheds_R_export_", today(), ".csv"), row.names = FALSE)

# #A more complicated manual way to do the reformatting of dates!
# #Shows how powerful lubridate is!
# # Remove milliseconds and parse with as.POSIXct
# test_locs_var_profiles_by_id$`Latest Field Visit Date/Time`<- sub("\\.\\d{3}", "", test_locs_var_profiles_by_id$`Latest Field Visit Date/Time`)
# 
# test_locs_var_profiles_by_id$`Latest Field Visit Date/Time`<- sub(":(?!.*:)", "", test_locs_var_profiles_by_id$`Latest Field Visit Date/Time`, perl = TRUE)
# 
# test_locs_var_profiles_by_id$`Latest Field Visit Date/Time`<- as.POSIXct(test_locs_var_profiles_by_id$`Latest Field Visit Date/Time`, format="%Y-%m-%dT%H:%M:%S%z", tz="America/Los_Angeles")


# # Plot using ggplot2
# p1 <- ggplot() +
#   #geom_sf(data = bcdc_freshwater_atlas_watershed_groups, fill = "blue", alpha = 0.5) +
#   geom_sf(data = test_locs_var_profiles_by_id, fill = "red", alpha = 0.5) +
#   ggtitle("ENMODS POINTS")
# 
# ggsave("./plots/enmods_points.png", p1, width = 4, height = 6, units = "in")



# Creating an AMS authorizations layer ------------------------------------

temp_ams_authorizations <- readxl::read_xlsx("all_ams_authorizations.xlsx", 
                  sheet = "All_Authorizations") %>%
  mutate(Longitude = -Longitude) %>%
  rename(`Authorization Status` = State, 
         `Facility Type` = `WDR Schedule One or Two`) %>%
  dplyr::select(`Authorization Number`, `Company`, 
                Latitude, Longitude, `Waste Type`, `Authorization Type`, 
                `Facility Type`, `Facility Type - Description`,
                `Authorization Status`, `Issue Date`, `Expiry Date`, 
                )

write.csv(temp_ams_authorizations, "authorizations_cleaned.csv", 
          row.names = FALSE)



# Downloading all observations with flags ---------------------------------
# https://bcenv-enmods-test.aqsamples.ca/observations?sort=-observedTime&flags=ALL
# Swagger says the following about this type of query:
# Filter query results if present.
# Acceptable values are either.
# 1) This string enclosed in quotes: 'ALL'. This will return all flagged Observations.
# 2) One or more ids. The ids can refer to Standards or ValidationRules. This will be a more granular filter

# Importing Stats Canada provincial boundary files
# Read the shapefile (update path accordingly)
canada_provinces <- st_transform(st_read("lpr_000a21a_e/lpr_000a21a_e.shp"), "EPSG:4326")
plot(canada_provinces)

locs_missing_lat_long <- locs_var_profiles_by_id %>% 
  dplyr::filter(is.na(latitude)|is.na(longitude)|
                  (latitude == "")|(longitude == ""))

locs_var_profiles_by_id_province <- locs_var_profiles_by_id %>% 
  dplyr::filter(!(is.na(latitude)|is.na(longitude)|
                    (latitude == "")|(longitude == ""))) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326", remove = FALSE)  %>% st_join(canada_provinces %>% dplyr::select(PRENAME, PRFNAME), join = st_intersects, left = FALSE)

locs_var_profiles_by_id_qaqc <- locs_var_profiles_by_id_province %>% 
  dplyr::filter(PRENAME == "British Columbia")

locs_suspect_lat_long <- locs_var_profiles_by_id_province %>% 
  dplyr::filter(PRENAME != "British Columbia") %>% 
  dplyr::select(-c(PRENAME, PRFNAME))

# Example: Geocoding a single address
result_name <- geo(address = locs_suspect_lat_long$name, method = "osm")
print(result_name)

# Example: Geocoding a single address
result_description <- geo(address = locs_suspect_lat_long$description, method = "osm")
print(result_description)



# observations ------------------------------------------------------------
# getting a complete list of observation IDs ------------------------------

#get all observation IDs out (for editing)
url <- str_c(base_url, "v2/observations?limit=1000")
data_body <- list()

x_obs <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

obs <- fromJSON(rawToChar(x_obs$content))$domainObjects %>% unnest_wider(auditAttributes, names_repair = "universal")

obs_ids <- obs$id

# running of API to get id by id based detailed retrieval ------------------------------
#
# Function to make an API request
make_api_request <- function(obs_id) {
  url <- str_c(base_url, "v2/observations/", obs_id)
  
  data_body <- list()
  
  response <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

  if (status_code(response) != 200) {

    return()
    #stop("Failed to fetch data: ", status_code(response))
  }

  # content <- content(response, as = "text", encoding = "UTF-8")
  # data <- fromJSON(content)
  # return(content)
  
  data <- fromJSON(rawToChar(response$content)) #%>% unnest_wider(auditAttributes, names_repair = "universal")

  # # Write the raw JSON content to a temporary file
  # temp_file <- tempfile()
  # writeBin(content(response, as = "raw"), temp_file)
  # 
  # # Stream the JSON data from the temporary file
  # con <- file(temp_file, open = "r")
  # data <- stream_in(con)
  # close(con)
  # 
  # # Delete the temporary file
  # unlink(temp_file)

  return(data)
}

i <- 1
#i <- 2110

# obs_id <- obs_ids[1]
 
# temp_tibble <- make_api_request(obs_id) %>% unlist() %>% 
#   keep(names(.) %in% c("id", "customId", "auditAttributes.creationUserProfileId"))

for (obs_id in obs_ids){
#for (src in source_names[(2110: length(source_names))]){

  temp_tibble <- make_api_request(obs_id) %>% unlist() %>% 
    keep(names(.) %in% c("id", "customId", "auditAttributes.creationUserProfileId"))  
  
  #str(temp_tibble)
  
  # Check if data is empty, break the loop if it is
  if (is.null(temp_tibble)) {
    next
  }

  if (length(temp_tibble) == 0) {
    next
  }

  # temp_tibble <- temp_tibble %>% unnest_wider(col = c(auditAttributes, activity, collectionMethod, observedProperty, specimen, samplingLocation, numericResult, medium, depth, labResultDetails, analysisMethod, fieldVisit, importHistoryEventSimples, validationWarnings, resultGrade, resultStatus, statistics, extendedAttributes), names_repair = "universal")
    
  if (i==1){
    obs_data_full <- temp_tibble
  } else {
    obs_data_full <- rbind(obs_data_full, temp_tibble)
  }

  i <- i + 1

  # Add a sleep to avoid hitting the rate limit
  #Sys.sleep(1)

  print(i)
}

rm(temp_tibble)


# getting user id associated with a specific user -------------------------

#first let's try to get a list of all users; perhaps some info there

#get user lists out (for editing)
url <- str_c(base_url, "v2/users/")
data_body <- list()

x_usrs <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#names(fromJSON(rawToChar(x_usrs$content))$domainObjects)

usrs <- fromJSON(rawToChar(x_usrs$content))$domainObjects %>% select(id, customId, userProfile, accessGroups) %>% unnest_wider(userProfile, names_repair = "universal")

usr_id <- usrs %>% dplyr::filter(firstName == "Sahil" & lastName == "Bhandari") %>% dplyr::select(id...3) %>% unlist()

# finding obs id associated with this user --------------------------------
obs_select <- obs_data_full %>% as_tibble() %>% dplyr::select(id, 
  auditAttributes.creationUserProfileId) %>% dplyr::filter(auditAttributes.creationUserProfileId == 
                                                             usr_id) %>% dplyr::select(id) %>% unlist()

# deleting observation with specific id -----------------------------------
url <- str_c(base_url, "v2/observations/", obs_select)
data_body <- list()

#Getting a list of relevant observations
x_obs_del <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#Confirm deleting the relevant activities
obs_del <- fromJSON(rawToChar(x_obs_del$content)) %>% unlist() %>% 
  keep(names(.) %in% c("id", "customId", "auditAttributes.creationUserProfileId"))

#Deleting the relevant activities
x_obs_del <- DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#it worked!
obs_del <- fromJSON(rawToChar(x_obs_del$content))












# field visits ------------------------------------------------------------
# getting a complete list of field visit IDs ------------------------------
#get field visits ids out (for editing)
url <- str_c(base_url, "v1/fieldvisits?limit=1000")
data_body <- list()

x_vsts <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

vsts <- fromJSON(rawToChar(x_vsts$content))$domainObjects %>% as_tibble() %>% 
  dplyr::select(id, project, auditAttributes, samplingLocation) %>% 
  unnest_wider(project, names_repair = "universal") %>% unnest_wider(auditAttributes, 
                                                                     names_repair = "universal") %>% unnest_wider(samplingLocation, names_repair = "universal")

vsts_ids <- vsts$id...1

# running of API to get id by id based detailed retrieval ------------------------------
#
# Function to make an API request
make_api_request <- function(vsts_id) {
  url <- str_c(base_url, "v1/fieldvisits/", vsts_id)
  
  data_body <- list()
  
  response <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  
  if (status_code(response) != 200) {
    
    return()
    #stop("Failed to fetch data: ", status_code(response))
  }
  
  # content <- content(response, as = "text", encoding = "UTF-8")
  # data <- fromJSON(content)
  # return(content)
  
  data <- fromJSON(rawToChar(response$content)) #%>% unnest_wider(auditAttributes, names_repair = "universal")
  
  # # Write the raw JSON content to a temporary file
  # temp_file <- tempfile()
  # writeBin(content(response, as = "raw"), temp_file)
  # 
  # # Stream the JSON data from the temporary file
  # con <- file(temp_file, open = "r")
  # data <- stream_in(con)
  # close(con)
  # 
  # # Delete the temporary file
  # unlink(temp_file)
  
  return(data)
}

i <- 1
#i <- 2110

# vsts_id <- vsts_ids[1]
# 
# temp_tibble <- make_api_request(vsts_id) %>% unlist() %>%
#   keep(names(.) %in% c("id", "auditAttributes.creationUserProfileId"))

for (vsts_id in vsts_ids){
  #for (src in source_names[(2110: length(source_names))]){
  
  temp_tibble <- make_api_request(vsts_id) %>% unlist() %>% 
    keep(names(.) %in% c("id", "auditAttributes.creationUserProfileId"))  
  
  #str(temp_tibble)
  
  # Check if data is empty, break the loop if it is
  if (is.null(temp_tibble)) {
    next
  }
  
  if (length(temp_tibble) == 0) {
    next
  }
  
  # temp_tibble <- temp_tibble %>% unnest_wider(col = c(auditAttributes, activity, collectionMethod, observedProperty, specimen, samplingLocation, numericResult, medium, depth, labResultDetails, analysisMethod, fieldVisit, importHistoryEventSimples, validationWarnings, resultGrade, resultStatus, statistics, extendedAttributes), names_repair = "universal")
  
  if (i==1){
    vsts_data_full <- temp_tibble
  } else {
    vsts_data_full <- rbind(vsts_data_full, temp_tibble)
  }
  
  i <- i + 1
  
  # Add a sleep to avoid hitting the rate limit
  #Sys.sleep(1)
  
  print(i)
}

rm(temp_tibble)

#testing of imported files
test_field_visits <- get_var_id_by_locs("fieldvisits", "4e2db011-1813-40c5-a5d3-a08ed346abe9")

test_field_visits_profiles <- get_var_profile_by_id("fieldvisits", test_field_visits)

orig_test_field_visits <- read_csv("../ENMODS_import_testing/orig_test_field_visits.csv") %>% rename(orig_field_visit_ID = "...1") 

orig_test_field_visits$`Field Visit Start Time` <- orig_test_field_visits$`Field Visit Start Time` %>% as.POSIXct(format="%Y-%m-%dT%H:%M:%S%z", tz = "Etc/GMT+8")#, tz="America/Los_Angeles") #%>% 
  #format("%Y-%m-%dT%H:%M:%S%z")

orig_test_field_visits_date_count <- orig_test_field_visits %>%
  mutate(field_visit_date = date(`Field Visit Start Time`)) %>%
  dplyr::select(field_visit_date) %>%
  unique()

# getting user id associated with a specific user -------------------------
#first let's try to get a list of all users; perhaps some info there

#get user lists out (for editing)
url <- str_c(base_url, "v2/users/")
data_body <- list()

x_usrs <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#names(fromJSON(rawToChar(x_usrs$content))$domainObjects)

usrs <- fromJSON(rawToChar(x_usrs$content))$domainObjects %>% select(id, customId, userProfile, accessGroups) %>% unnest_wider(userProfile, names_repair = "universal")

usr_id <- usrs %>% dplyr::filter(firstName == "Sahil" & lastName == "Bhandari") %>% dplyr::select(id...3) %>% unlist()

# finding obs id associated with this user --------------------------------
vsts_select <- vsts_data_full %>% as_tibble() %>% dplyr::select(id, auditAttributes.creationUserProfileId) %>% dplyr::filter(auditAttributes.creationUserProfileId == usr_id) %>% dplyr::select(id) %>% unlist()

# deleting observation with specific id -----------------------------------
url <- str_c(base_url, "v1/fieldvisits/", vsts_select)
data_body <- list()

#Getting a list of relevant observations
x_vsts_del <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#Confirm deleting the relevant activities
vsts_del <- fromJSON(rawToChar(x_vsts_del$content)) %>% unlist() %>% 
  keep(names(.) %in% c("id", "auditAttributes.creationUserProfileId"))

#Deleting the relevant activities
x_vsts_del <- DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

vsts_del <- fromJSON(rawToChar(x_vsts_del$content))





# Misc pieces of code -----------------------------------------------------
#make_api_requests using details of the above filtered field visit incl. project.id, samplingLocation.id, startTime, and auditAttributes.creationUserProfileId

# vsts_details <- get_var_profile_by_id("fieldvisits", "all") %>%
#   dplyr::filter(id == vsts_id)

#vsts_details <- vsts_var_profiles_by_id %>% dplyr::filter(id == vsts_id_del)

#get_user_id_by_name("Sahil", "Bhandari")

#for these ones, run details by obs id and get the ones with the field visit id matching

data_select <- data_full %>% as_tibble() %>% dplyr::select(id, fieldVisit.id) %>% 
  dplyr::filter(fieldVisit.id == vsts_id) %>% dplyr::select(id) %>% unlist()

return(data_select)

# Exporting AQS data frames using JSONLite ------------------------------------
library(jsonlite)

#json_data_test <- toJSON(actvty, pretty = TRUE)

#write(json_data_test, file = "data_test.json")

#getting a test location data in the JSON format expectation
json_locs_data <- locs_var_profiles_by_id %>%
  dplyr::select(customId, name) %>%
  #dplyr::filter(`Location Identifier` == "E273048") %>%
  #mutate(is_sampled_recent = ...) %>% 
  #mutate(is_new = ) %>%
  #dplyr::filter(is_new|is_recent) %>%
  rename(disp_name = name) %>%
  mutate(disp_name = str_c(customId, " - ", disp_name)) %>%
  dplyr::select(disp_name)

#added the items keyword to store variables in an array
json_data_test <- toJSON(list(items = json_locs_data), pretty = TRUE)

#writing the created JSON file
write(json_data_test, file = "test_locs_data.json")

#labs
json_labs_data <- labs_var_profiles_by_id

json_data_test <- toJSON(list(items = json_labs_data), pretty = TRUE)

#writing the created JSON file
write(json_data_test, file = "test_labs_data.json")

#sampling agency
json_samplingagncy_data <- samplingagency_var_profiles_by_id# %>%
  #dplyr::filter(description == "") %>%
  #dplyr::select(id, customId)

json_data_test <- toJSON(list(items = json_samplingagncy_data), pretty = TRUE)

#writing the created JSON file
write(json_data_test, file = "test_samplingagncy_data.json")

#collection methods
json_collectionmthds_data <- collectionmthds_var_profiles_by_id

json_data_test <- toJSON(list(items = json_collectionmthds_data), pretty = TRUE)

#writing the created JSON file
write(json_data_test, file = "test_collectionmthds_data.json")

#projects
json_proj_data <- proj_var_profiles_by_id %>%
  dplyr::select(id, customId, name, description, type) %>%
  dplyr::filter(type!="ROUTINE_MONITORING")

json_data_test <- toJSON(list(items = json_proj_data), pretty = TRUE)

#writing the created JSON file
write(json_data_test, file = "test_proj_data.json")

#mediums
json_mediums_data <- mediums_var_profiles_by_id

json_data_test <- toJSON(list(items = json_mediums_data), pretty = TRUE)

#writing the created JSON file
write(json_data_test, file = "test_mediums_data.json")

# Spreadsheet to JSON -----------------------------------------------------
json_analysisPackagesCoC_data <- read_csv("../COC_form/json_list_extracts/analysisPackagesCoC - April-3-2025(Sheet1).csv", col_types = cols(preservative.name = col_character(), comments = col_character(), ...9 = col_skip())) 

json_analysisPackagesCoC_data <- json_analysisPackagesCoC_data %>%
  dplyr::select(-c(preservative.name)) %>%
  rename(bottle = items.bottle, 
         filter = items.field.filter,
         preservative = items.preservative
  ) %>%
  group_by(matrix, bottle, filter, preservative, comments) %>%
  #rowwise() %>%
  mutate(observedProperties = list(tibble(label = test.label, 
                                          analysisGroup = items.analysisGroup))) %>%
  ungroup() %>%
  dplyr::select(-c(items.analysisGroup, test.label)) %>% unique()

json_data_test <- toJSON(list(items = json_analysisPackagesCoC_data), pretty = TRUE)

#writing the created JSON file
write(json_data_test, file = "test_analysisPackages_data.json")


