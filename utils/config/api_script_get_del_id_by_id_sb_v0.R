library(httr)
library(jsonlite)
library(tidyverse)

#get the API token from your environment file
readRenviron(paste0(getwd(), "./.Renviron"))
token <- Sys.getenv("api_token")
base_url = 'https://bcenv-enmods-test.aqsamples.ca/api/'

# universal functions -----------------------------------------------------
# getting a complete list of IDs ------------------------------

get_ids <- function(data_type){
  
  if(data_type == "observations"){
    
    #get all observation IDs out (for editing)
    url <- "https://bcenv-enmods-test.aqsamples.ca/api/v2/observations?limit=1000"
    
  } else if (data_type == "fieldvisits") {
    
    url <- "https://bcenv-enmods-test.aqsamples.ca/api/v1/fieldvisits?limit=1000"
    
  } else if (data_type == "locations") {
    
    url <- "https://bcenv-enmods-test.aqsamples.ca/api/v1/samplinglocations?limit=1000"
    
  } else {
    
    return("The coding pipeline has been developed only for observations, field visits, and locations.")
    
  }
  
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

obs_ids <- get_ids("observations")

vsts_ids <- get_ids("fieldvisits")

locs_ids <- get_ids("locations")

# making one API request by id and data type ------------------------------
# Function to make an API request
make_api_request <- function(id, data_type) {
  
  if (data_type == "observations"){
    
    url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v2/observations/", id)
    
  } else if (data_type == "fieldvisits") {
    
    url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/fieldvisits/", id)
    
  } else if (data_type == "locations") {
    
    url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/samplinglocations/", id)
    
  } else {
    
    return("The code is currently set to only make requests for observations, field visits, and locations")
    
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
  
  return(data)
}

obs_sample <- make_api_request("00687032-16d3-4ef2-adfa-13f37835fadd", "observations")

vsts_sample <- make_api_request(vsts_ids[1], "fieldvisits")

locs_sample <- make_api_request("766b4e26-1eed-4716-bf85-ceafe3e38104", "locations")

# using API on a list of ids to get id relationship with relevant variable------------------------------

get_var_profile_by_id <- function(data_type, rel_var) {
  
  # data_type <- "fieldvisits"
  # 
  # rel_var <- "auditAttributes.creationUserProfileId"
  
  ids <- get_ids(data_type)
  
  #ids <- get_ids("fieldvisits")
  
  i <- 1
  
  for (id in ids){
    
    #id <- ids[1] 
    
    temp_tibble <- make_api_request(id, data_type) %>% unlist() %>%
      keep(names(.) %in% c("id", rel_var))

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
    Sys.sleep(1)
    
    print(i)
    
  }
    
  return(data_full) 
  
}

obs_var_profiles_by_id <- get_var_profile_by_id("observations", c("fieldVisit.id", "auditAttributes.creationUserProfileId"))

vsts_var_profiles_by_id <- get_var_profile_by_id("fieldvisits", c("startTime", "endTime", 
  "project.id", "auditAttributes.creationUserProfileId", "samplingLocation.id"))

locs_var_profiles_by_id <- get_var_profile_by_id("locations", "auditAttributes.creationUserProfileId")

# getting user id associated with a username -------------------------

get_user_id_by_name <- function(frstnm, lstnm){
  
  #first let's try to get a list of all users; perhaps some info there
  
  #get user lists out (for editing)
  url <- "https://bcenv-enmods-test.aqsamples.ca/api/v2/users/"
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
  
  data_full <- get_var_profile_by_id(data_type, "auditAttributes.creationUserProfileId")
  
  data_select <- data_full %>% as_tibble() %>% dplyr::select(id, 
    auditAttributes.creationUserProfileId) %>% dplyr::filter(
      auditAttributes.creationUserProfileId == usr_id) %>% dplyr::select(id) %>% unlist()
  
  return(data_select)
  
}

vsts_id_del <- get_id_by_user_profile("Sahil", "Bhandari", "fieldvisits")

# finding ids of observations associated with a given field visit --------
get_obs_id_by_vsts <- function(vsts_id){
  
  #get all observation IDs out (for editing)
  url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v2/observations?limit=1000&fieldVisitId=", vsts_id_del) 
  data_body <- list()
  
  data_select <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), 
                body = data_body, encode = 'json')
  
  obs_select <- fromJSON(rawToChar(data_select$content))$domainObjects
  
  obs_select_ids <- obs_select$id
  
  return(obs_select_ids)  
  
}

obs_id_del <- get_obs_id_by_vsts(vsts_id_del)

# deleting data of a specific data type with specific id -----------------------------------
del_data_by_id <- function(id, data_type){
  
  if(data_type == "observations"){
    
    url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v2/", data_type, "/", id)
    
  } else if (data_type == "fieldvisits"){
    
    #first need to delete all observations associated with this field visit
    
    #first need to get all observations associated with this field visit
    obs_id_del <- get_obs_id_by_vsts(id)
    
    del_data_by_id(obs_id_del, "observations")
    
    url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/", data_type, "/", id)
    
  } else {
    
    print("The coding pipeline has only been developed for observations and field visits.")
    
    return("The coding pipeline has only been developed for observations and field visits.")
    
  }
  
  data_body <- list()
  
  #Deleting the relevant data
  x_del <- DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  
  return(x_del)
  
} 

x_vsts_del <- del_data_by_id(vsts_id_del, "fieldvisits")

vsts_del <- fromJSON(rawToChar(x_vsts_del$content))

# deleting data associated with a specific user ---------------------------
del_data_by_user <- function(frstnm, lstnm){
  
  #first get user id associated with name
  usr_id <- get_user_id_by_name(frstnm, lstnm)
  
  #next get field visit data associated with name
  vsts_id_del <- get_id_by_user_profile(frstnm, lstnm, "fieldvisits")
  
  #next delete everything within the relevant field visits (incl. observations) 
  x_vsts_del <- del_data_by_id(vsts_id_del, "fieldvisits")
  
  #next get field visit data associated with name
  locs_id_del <- get_id_by_user_profile(frstnm, lstnm, "locations")
  
  #next delete sampling locations associated with this user
  x_locs_del <- del_data_by_id(frstnm, lstnm)
  
  return(x_locs_del)
}

x_usr_del <- del_data_by_user("Sahil", "Bhandari")

usr_del <- fromJSON(rawToChar(x_usr_del$content))





# observations ------------------------------------------------------------
# getting a complete list of observation IDs ------------------------------

#get all observation IDs out (for editing)
url <- "https://bcenv-enmods-test.aqsamples.ca/api/v2/observations?limit=1000"
data_body <- list()

x_obs <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

obs <- fromJSON(rawToChar(x_obs$content))$domainObjects %>% unnest_wider(auditAttributes, names_repair = "universal")

obs_ids <- obs$id

# running of API to get id by id based detailed retrieval ------------------------------
#
# Function to make an API request
make_api_request <- function(obs_id) {
  url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v2/observations/", obs_id)
  
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
  Sys.sleep(1)

  print(i)
}

rm(temp_tibble)


# getting user id associated with a specific user -------------------------

#first let's try to get a list of all users; perhaps some info there

#get user lists out (for editing)
url <- "https://bcenv-enmods-test.aqsamples.ca/api/v2/users/"
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
url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v2/observations/", obs_select)
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
url <- "https://bcenv-enmods-test.aqsamples.ca/api/v1/fieldvisits?limit=1000"
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
  url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/fieldvisits/", vsts_id)
  
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
  Sys.sleep(1)
  
  print(i)
}

rm(temp_tibble)

# getting user id associated with a specific user -------------------------

#first let's try to get a list of all users; perhaps some info there

#get user lists out (for editing)
url <- "https://bcenv-enmods-test.aqsamples.ca/api/v2/users/"
data_body <- list()

x_usrs <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#names(fromJSON(rawToChar(x_usrs$content))$domainObjects)

usrs <- fromJSON(rawToChar(x_usrs$content))$domainObjects %>% select(id, customId, userProfile, accessGroups) %>% unnest_wider(userProfile, names_repair = "universal")

usr_id <- usrs %>% dplyr::filter(firstName == "Sahil" & lastName == "Bhandari") %>% dplyr::select(id...3) %>% unlist()

# finding obs id associated with this user --------------------------------
vsts_select <- vsts_data_full %>% as_tibble() %>% dplyr::select(id, auditAttributes.creationUserProfileId) %>% dplyr::filter(auditAttributes.creationUserProfileId == usr_id) %>% dplyr::select(id) %>% unlist()

# deleting observation with specific id -----------------------------------
url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/fieldvisits/", vsts_select)
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

# vsts_details <- get_var_profile_by_id("fieldvisits", c("startTime", "endTime", 
#  "project.id", "auditAttributes.creationUserProfileId", "samplingLocation.id")) %>%
#   dplyr::filter(id == vsts_id)

#vsts_details <- vsts_var_profiles_by_id %>% dplyr::filter(id == vsts_id_del)

#get_user_id_by_name("Sahil", "Bhandari")

#for these ones, run details by obs id and get the ones with the field visit id matching

data_select <- data_full %>% as_tibble() %>% dplyr::select(id, fieldVisit.id) %>% 
  dplyr::filter(fieldVisit.id == vsts_id) %>% dplyr::select(id) %>% unlist()

return(data_select)


