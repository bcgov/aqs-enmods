library(httr)
library(jsonlite)
library(tidyverse)

#get all location from dev

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
    
  } else if (data_type == "specimens") {
    
    url <- "https://bcenv-enmods-test.aqsamples.ca/api/v1/specimens?limit=1000"
    
  } else if (data_type == "activities") {
    
    url <- "https://bcenv-enmods-test.aqsamples.ca/api/v1/activities?limit=1000"
    
  } else if (data_type == "fieldvisits") {
    
    url <- "https://bcenv-enmods-test.aqsamples.ca/api/v1/fieldvisits?limit=1000"
    
  } else if (data_type == "samplinglocations") {
    
    url <- "https://bcenv-enmods-test.aqsamples.ca/api/v1/samplinglocations?limit=1000"
    
  } else if (data_type == "projects") {
    
    url <- "https://bcenv-enmods-test.aqsamples.ca/api/v1/projects?limit=1000"
    
  } else if (data_type == "analyticalgroups") {
    
    url <- "https://bcenv-enmods-test.aqsamples.ca/api/v1/analyticalgroups?limit=1000"
    
  } else {
    
    return("The coding pipeline has been developed only for observations, field visits, and samplinglocations.")
    
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

spcmns_ids <- get_ids("specimens")

obs_ids <- get_ids("observations")

actvty_ids <- get_ids("activities")

vsts_ids <- get_ids("fieldvisits")

locs_ids <- get_ids("samplinglocations")

prjcts_ids <- get_ids("projects")

analyticalgrps_ids <- get_ids("analyticalgroups")

# making one API all data request by id and data type ------------------------------
# Function to make an API request
make_api_request <- function(id, data_type) {
  
  if (data_type == "observations"){
    
    url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v2/observations/", id)
    
  } else if (data_type == "specimens") {
    
    url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/specimens/", id)
    
  } else if (data_type == "activities") {
    
    url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/activities/", id)
    
  } else if (data_type == "fieldvisits") {
    
    url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/fieldvisits/", id)
    
  } else if (data_type == "samplinglocations") {
    
    url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/samplinglocations/", id)
    
  } else if (data_type == "projects") {
    
    url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/projects/", id)
    
  } else if (data_type == "analyticalgroups") {
    
    url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/analyticalgroups/", id)
    
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
  
  return(data)
}

spcmns_sample <- make_api_request(spcmns_ids[1], "specimens")

obs_sample <- make_api_request(obs_ids[1], "observations")

actvty_sample <- make_api_request(actvty_ids[1], "activities")

vsts_sample <- make_api_request(vsts_ids[1], "fieldvisits")

locs_sample <- make_api_request(locs_ids[1], "samplinglocations")

prjcts_sample <- make_api_request(prjcts_ids[1], "projects")

analyticalgrps_sample <- make_api_request(analyticalgrps_ids[1], "analyticalgroups")

# making one API history request by id and data type ------------------------------
# Function to make an API request for data history
make_api_request_history <- function(id, data_type) {
  
  if (data_type == "observations"){
    
    url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v2/observations/", id, "/history")
    
  } else if (data_type == "specimens") {
    
    url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/specimens/", id, "/history")
    
  } else if (data_type == "activities") {
    
    url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/activities/", id, "/history")
    
  } else if (data_type == "fieldvisits") {
    
    url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/fieldvisits/", id, "/history")
    
  } else if (data_type == "samplinglocations") {
    
    url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/samplinglocations/", id, "/history")
    
  } else if (data_type == "projects") {
    
    url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/projects/", id, "/history")
    
  } else if (data_type == "analyticalgroups") {
    
    url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/analyticalgroups/", id, "/history")
    
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
  
  return(data)
}

spcmns_sample_history <- make_api_request(spcmns_ids[1], "specimens")

obs_sample_history <- make_api_request(obs_ids[1], "observations")

actvty_sample_history <- make_api_request(actvty_ids[1], "activities")

vsts_sample_history <- make_api_request(vsts_ids[1], "fieldvisits")

locs_sample_history <- make_api_request(locs_ids[1], "samplinglocations")

projects_sample_history <- make_api_request(prjcts_ids[1], "projects")

analyticalgrps_sample_history <- make_api_request(analyticalgrps_ids[1], "analyticalgroups")

# making one API request for location summary info ------------------------
# Function to make an API request for data history
# currently summary info is only available for locations
make_api_request_summary <- function(id, data_type) {
  
  url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/", data_type, "/", id, "/summary")
  
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

loc_summary <- make_api_request_summary("ca75e340-e1ba-451e-8fc7-531a61036279", "samplinglocations")

# getting relevant parameters (other than id) to download for given variable --------
gen_list_rel_var <- function(data_type){
  
  if (data_type == "specimens"){
    
    rel_var <- c("activity.id", "observations.id", "fieldVisit.id", "fieldVisit.project.id", "activity.samplingLocation.id", "auditAttributes.creationUserProfileId")
    
  } else if (data_type == "observations"){
    
    rel_var <- c("activity.id", "fieldVisit.id", "fieldVisit.project.id", "activity.samplingLocation.id", "auditAttributes.creationUserProfileId")
    
  } else if (data_type == "activities"){
    
    rel_var <- c("fieldVisit.id", "fieldVisit.project.id", "activity.samplingLocation.id", "auditAttributes.creationUserProfileId")
    
  } else if (data_type == "fieldvisits"){
    
    rel_var <- c("project.id", "auditAttributes.creationUserProfileId", "samplingLocation.id")
    
  } else if (data_type == "projects"){
    
    rel_var <- c("auditAttributes.creationUserProfileId")
    
  } else if (data_type == "samplinglocations"){
    
    rel_var <- c("customId", "name", "type.customId", "attachments.attachment.comment", "elevation.value", "elevation.unit.customId", "longitude", "latitude", "horizontalCollectionMethod", "auditAttributes.creationUserProfileId")
    
  } else if (data_type == "analyticalgroups"){
    
    rel_var <- c("name", "type", "auditAttributes.creationUserProfileId")
    
  }
  
}

# using API on a list of ids to get id relationship with relevant variable------------------------------
get_var_profile_by_id <- function(data_type, ids) {
  
  rel_var <- gen_list_rel_var(data_type)
  
  # data_type <- "fieldvisits"
  # 
  # rel_var <- "auditAttributes.creationUserProfileId"
  
  if(identical(ids, "all")) {
    
    ids <- get_ids(data_type)
    
    #ids <- get_ids("fieldvisits")
    
  }
  
  i <- 1
  
  for (id in ids){
    
    #id <- ids[1] 
    
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
  
  return(data_full) 
  
}

spcmns_var_profiles_by_id <- get_var_profile_by_id("specimens", "all")

obs_var_profiles_by_id <- get_var_profile_by_id("observations", "all")

actvty_var_profiles_by_id <- get_var_profile_by_id("activities", "all")

vsts_var_profiles_by_id <- get_var_profile_by_id("fieldvisits", "all")

proj_var_profiles_by_id <- get_var_profile_by_id("projects", "all")

locs_var_profiles_by_id <- get_var_profile_by_id("samplinglocations", "all")

#write.csv(locs_var_profiles_by_id, "test_location_summary_enmods.csv")

analyticalgrps_var_profiles_by_id <- get_var_profile_by_id("analyticalgroups", "all")

# test_locs_var_profiles_by_id <- get_var_profile_by_id("samplinglocations", "de360ca3-0708-4d84-93a2-c0ccce2dc8b2")

# updating older lists using the id by id approach --------
# Note that the pipeline below runs only for observations, field visits, and locations 
update_var <- function(data_type, ref_file) {
  
  last_run <- file.info(".RData")$mtime %>% as.POSIXct(format="%Y-%m-%d %H:%M:%S %Z", tz="America/Los_Angeles") %>% format("%Y-%m-%dT00:00:00%z")
  
  if(data_type != "observations"){
    
    url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/", data_type, "?startModificationTime=", last_run)
    
  } else {
    
    url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v2/", data_type, "?startModificationTime=", last_run)
    
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
    
  }
  
  if (exists(filenm) && !is.null(filenm)) {
    
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
  
  data_full <- gen_updated_data_file(data_type)
  
  data_select <- data_full %>% as_tibble() %>% dplyr::select(id, 
                                                             auditAttributes.creationUserProfileId) %>% dplyr::filter(
                                                               auditAttributes.creationUserProfileId == usr_id) %>% dplyr::select(id) %>% unlist()
  
  return(data_select)
  
}

spcmns_id_del <- get_id_by_user_profile("Sahil", "Bhandari", "specimens")

analyticalgrps_id_del <- get_id_by_user_profile("Sahil", "Bhandari", "analyticalgroups")

obs_id_del <- get_id_by_user_profile("Sahil", "Bhandari", "observations")

actvty_id_del <- get_id_by_user_profile("Sahil", "Bhandari", "activities")

vsts_id_del <- get_id_by_user_profile("Sahil", "Bhandari", "fieldvisits")

prjcts_id_del <- get_id_by_user_profile("Sahil", "Bhandari", "projects")

locs_id_del <- get_id_by_user_profile("Sahil", "Bhandari", "samplinglocations")

# finding ids of observations associated with a given field activity --------
get_var_id_by_var <- function(data_type_ref_var, data_type_ref_var_id, data_type_var_new){
  
  if(data_type_ref_var == "activities"){
    
    if(data_type_var_new == "observations"){
      
      #get all observation IDs out (for editing)
      url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v2/observations?limit=1000&activityIds=", data_type_ref_var_id) 
      
    }    
    
  }
  
  if(data_type_ref_var == "analyticalgroups"){
    
    if(data_type_var_new == "specimens") {
      
      #get all observation IDs out (for editing)
      url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/specimens?limit=1000&analyticalGroupIds=", data_type_ref_var_id) 
      
    }
    
  }
  
  data_body <- list()
  
  data_select <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), 
                     body = data_body, encode = 'json')
  
  var_select <- fromJSON(rawToChar(data_select$content))$domainObjects
  
  var_select_ids <- var_select$id
  
  return(var_select_ids)  
  
}

obs_id_del <- get_var_id_by_var("activities", actvty_id_del, "observations")

spcmns_id_del <- get_var_id_by_var("analyticalgroups", analyticalgrps_id_del, "specimens")

# finding ids of observations associated with a given field visit --------
get_var_id_by_vsts <- function(data_type, vsts_id){
  
  if(data_type == "observations") {
    
    #get all observation IDs out (for editing)
    url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v2/observations?limit=1000&fieldVisitId=", vsts_id) 
    
  } else {
    
    #get all observation IDs out (for editing)
    url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/", data_type, "?limit=1000&fieldVisitId=", vsts_id)   
    
  }
  
  data_body <- list()
  
  data_select <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), 
                     body = data_body, encode = 'json')
  
  var_select <- fromJSON(rawToChar(data_select$content))$domainObjects
  
  var_select_ids <- var_select$id
  
  return(var_select_ids)  
  
}

actvty_id_del <- get_var_id_by_vsts("activities", vsts_id_del)

obs_id_del <- get_var_id_by_vsts("observations", vsts_id_del)

# finding ids of field visits/observations associated with a given location --------
get_var_id_by_locs <- function(data_type, locs_ids){
  
  if(length(locs_ids) == 1) {
    
    if(data_type == "fieldvisits"){
      
      #get all observation IDs out (for editing)
      url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/fieldvisits?limit=1000&samplingLocationIds=", locs_ids)
      
    } else if(data_type == "observations"){
      
      #get all observation IDs out (for editing)
      url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v2/observations?limit=1000&samplingLocationIds=", locs_ids)
      
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
  
  data_body <- list()
  
  data_select <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), 
                     body = data_body, encode = 'json')
  
  var_select <- fromJSON(rawToChar(data_select$content))$domainObjects
  
  var_select_ids <- var_select$id
  
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
      url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/fieldvisits?limit=1000&projectIds=", prjcts_ids)
      
    } else if(data_type == "observations"){
      
      #get all observation IDs out (for editing)
      url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v2/observations?limit=1000&projectIds=", prjcts_ids)
      
    } else if(data_type == "activities"){
      
      #get all observation IDs out (for editing)
      url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/activities?limit=1000&projectIds=", prjcts_ids)
      
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

# deleting data of a specific data type with specific id -----------------------------------
del_data_by_id <- function(ids, data_type){
  
  if(length(ids) == 1){
    
    if(data_type == "observations"){
      
      url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v2/", data_type, "/", ids)
      
    } else if (data_type == "specimens") {
      
      url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/", data_type, "/", ids)
      
    } else if (data_type == "analyticalgroups") {
      
      #first need to get all observations associated with this activity
      spcmns_id_del <- get_var_id_by_var("analyticalgroups", ids, "specimens")
      
      if(!is.null(spcmns_id_del) & !all(is.na(spcmns_id_del))){
        
        #next need to delete all observations associated with this field visit
        del_data_by_id(spcmns_id_del, "specimens")
        
      }
      
      url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/", data_type, "/", ids)
      
    } else if (data_type == "activities"){
      
      #first need to get all observations associated with this activity
      obs_id_del <- get_var_id_by_var("activities", ids, "observations")
      
      if(!is.null(obs_id_del) & !all(is.na(obs_id_del))){
        
        #next need to delete all observations associated with this field visit
        del_data_by_id(obs_id_del, "observations")
        
      }
      
      url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/", data_type, "/", ids)
      
    } else if (data_type == "fieldvisits"){
      
      #first need to get all observations associated with this field visit
      obs_id_del <- get_var_id_by_vsts("observations", ids)
      
      if(!is.null(obs_id_del) & !all(is.na(obs_id_del))){
        
        #next need to delete all observations associated with this field visit
        del_data_by_id(obs_id_del, "observations")
        
      }
      
      #first need to get all activities associated with this field visit
      actvty_id_del <- get_var_id_by_vsts("activities", ids)
      
      if(!is.null(actvty_id_del) & !all(is.na(actvty_id_del))){
        
        #next need to delete all activities associated with this field visit
        del_data_by_id(actvty_id_del, "observations")
        
      }
      
      url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/", data_type, "/", ids)
      
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
      
      url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/", data_type, "/", ids)
      
    } else if (data_type == "samplinglocations"){
      
      #first need to get all observations associated with this location
      obs_id_del <- get_var_id_by_locs("observations", ids)
      
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
      
      url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/", data_type, "/", ids)
      
    }
    
  } else {
    
    for(id in ids){
      
      del_data_by_id(id, data_type)
      
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
  #Sys.sleep(1)
  
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
  #Sys.sleep(1)
  
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

# vsts_details <- get_var_profile_by_id("fieldvisits", "all") %>%
#   dplyr::filter(id == vsts_id)

#vsts_details <- vsts_var_profiles_by_id %>% dplyr::filter(id == vsts_id_del)

#get_user_id_by_name("Sahil", "Bhandari")

#for these ones, run details by obs id and get the ones with the field visit id matching

data_select <- data_full %>% as_tibble() %>% dplyr::select(id, fieldVisit.id) %>% 
  dplyr::filter(fieldVisit.id == vsts_id) %>% dplyr::select(id) %>% unlist()

return(data_select)


