#KJ code to use API requests to access test data off ENMODS
#Building multiple pipelines

#BS coming in:
#Did some manual tests
#Cannot delete a location without deleting associated observations, 
#samples (specimens), and field visits
#Writing code to get and delete things created by a user

library(httr)
library(jsonlite)
library(tidyverse)

#get the API token from your environment file
readRenviron(paste0(getwd(), "./.Renviron"))
token <- Sys.getenv("api_token")
base_url = 'https://bcenv-enmods-test.aqsamples.ca/api/'

# Testing if user data is available ---------------------------------------
# analytical groups -------------------------------------------------------
#code to get analytical group data out
#UserProfile is present 

#get analytical groups out (for editing)
url <- "https://bcenv-enmods-test.aqsamples.ca/api/v1/analyticalgroups/"
data_body <- list()

x_ag <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

ag <- fromJSON(rawToChar(x_ag$content))$domainObjects %>% select(id, name, description, analyticalGroupItems)

op <- unlist(ag$analyticalGroupItems)

#unfortunately, the api call does not export user details

# projects ----------------------------------------------------------------
#code to get project data out
#UserProfile is present 

#get projects out (for editing)
url <- "https://bcenv-enmods-test.aqsamples.ca/api/v1/projects/"
data_body <- list()

x_prjcts <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

prjcts <- fromJSON(rawToChar(x_prjcts$content))$domainObjects %>% unnest_wider(auditAttributes, names_repair = "universal")

# locations ---------------------------------------------------------------
#TL; DR: was able to extract the most important columns
#User Profile data is however missing but workaround available
#extract individual location data by id and user profile data is present there
#then join all individual location files

#maximum request is 1000
#url <- "https://bcenv-enmods.aqsamples.ca/api/v1/samplinglocations?limit=1000"
url <- "https://bcenv-enmods-test.aqsamples.ca/api/v1/samplinglocations?limit=1000"
#url <- "https://bcenv-enmods-test.aqsamples.ca/api/v1/samplinglocations"

data_body <- list()

x_locs<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

total = fromJSON(rawToChar(x_locs$content))$totalCount

# locs <- fromJSON(rawToChar(x_locs$content))$domainObjects %>% as_tibble() %>% 
#   dplyr::select(id:longitude, horizontalCollectionMethod, description) %>% 
#   unnest_wider(type, names_repair = "universal") %>% unnest_wider(auditAttributes, 
#                                                                   names_repair = "universal")

if (total > 1000) { #if there are more than 1000 records loop
  
  element_id <- fromJSON(rawToChar(x_locs$content))$domainObjects$id
  
  # locs <- fromJSON(rawToChar(x_locs$content))$domainObjects %>%
  #   tibble::rownames_to_column("original_row_name") %>% as_tibble()
  locs <- fromJSON(rawToChar(x_locs$content))$domainObjects %>% as_tibble() %>% 
    dplyr::select(id, customId, name, auditAttributes, type, latitude, 
                  longitude, horizontalCollectionMethod, description) %>% 
    unnest_wider(type, names_repair = "universal") %>% unnest_wider(auditAttributes, 
                                                                    names_repair = "universal")
  
  number_loops = ceiling(total/1000)
  
  #i = 2
  
  for (i in seq(2,number_loops)) {
    cursor = fromJSON(rawToChar(x_locs$content))$cursor
    tempURL = paste0(url, "&cursor=", cursor)
    
    x_locs<-GET(tempURL, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
    
    temp_element_id <- fromJSON(rawToChar(x_locs$content))$domainObjects$id 
    
    #temp_locs <- fromJSON(rawToChar(x_locs$content))$domainObjects %>%
    #  tibble::rownames_to_column("original_row_name") %>% as_tibble()
    temp_locs <- fromJSON(rawToChar(x_locs$content))$domainObjects %>% as_tibble() %>% 
      dplyr::select(id, customId, name, auditAttributes, type, latitude, 
                    longitude, horizontalCollectionMethod, description) %>%
      unnest_wider(type, names_repair = "universal") %>% unnest_wider(auditAttributes, 
                                                                      names_repair = "universal")
    
    element_id <- append(element_id, temp_element_id)
    
    rownames(temp_locs) <- NULL
    
    rownames(locs) <- NULL
    
    locs <- rbind(locs, temp_locs)
    
    print(i)
  }
  
} else {
  
  element_id <- fromJSON(rawToChar(x_locs$content))$domainObjects$id
  
  locs <- fromJSON(rawToChar(x_locs$content))$domainObjects %>% as_tibble() %>% 
    dplyr::select(id, customId, name, auditAttributes, type, latitude, 
                  longitude, horizontalCollectionMethod, description) %>% 
    unnest_wider(type, names_repair = "universal") %>% unnest_wider(auditAttributes, 
                                                                    names_repair = "universal")
}

location_id <- element_id

#testing one location so we can test the call v1/samplinglocations/{id}
#it contains userProfile info which means we can get around the missing user profile in the 
#bulk location download
url <- "https://bcenv-enmods-test.aqsamples.ca/api/v1/samplinglocations/766b4e26-1eed-4716-bf85-ceafe3e38104"

data_body <- list()

x_loc<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#this way I can run this loop on all ids one by one and get user info
test_loc <- fromJSON(rawToChar(x_loc$content)) %>% unlist()

# location types ----------------------------------------------------------
#code to get location type data out
#UserProfile is present 

#maximum request is 1000
#url <- "https://bcenv-enmods.aqsamples.ca/api/v1/samplinglocations?limit=1000"
url <- "https://bcenv-enmods-test.aqsamples.ca/api/v1/samplinglocationtypes?limit=1000"
#url <- "https://bcenv-enmods-test.aqsamples.ca/api/v1/samplinglocationtypes"

data_body <- list()

x_loc_types<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

total = fromJSON(rawToChar(x_loc_types$content))$totalCount

if (total > 1000) { #if there are more than 1000 records loop
  
  element_id <- fromJSON(rawToChar(x_loc_types$content))$domainObjects$id
  
  loc_types <- fromJSON(rawToChar(x_loc_types$content))$domainObjects %>% as_tibble() %>% 
    dplyr::select(id, customId, auditAttributes) %>% unnest_wider(auditAttributes, 
                                                                    names_repair = "universal")
  
  number_loops = ceiling(total/1000)
  
  #i = 2
  
  for (i in seq(2,number_loops)) {
    cursor = fromJSON(rawToChar(x_loc_types$content))$cursor
    tempURL = paste0(url, "&cursor=", cursor)
    
    x_loc_types<-GET(tempURL, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
    
    temp_element_id <- fromJSON(rawToChar(x_loc_types$content))$domainObjects$id 
    
    temp_loc_types <- fromJSON(rawToChar(x_loc_types$content))$domainObjects %>% as_tibble() %>% 
      dplyr::select(id, customId, auditAttributes) %>% unnest_wider(auditAttributes, 
                                                                          names_repair = "universal")
    
    element_id <- append(element_id, temp_element_id)
    
    rownames(temp_locs) <- NULL
    
    rownames(locs) <- NULL
    
    locs <- rbind(loc_types, temp_loc_types)
    
    print(i)
  }
  
} else {
  
  element_id <- fromJSON(rawToChar(x_loc_types$content))$domainObjects$id
  
  loc_types <- fromJSON(rawToChar(x_loc_types$content))$domainObjects %>% as_tibble() %>% 
    dplyr::select(id, customId, auditAttributes) %>% unnest_wider(auditAttributes, 
                                                                        names_repair = "universal")
}

location_type_id <- element_id


# field visits -------------------------------------------------------------
#code to get field trip data out
##User Profile data is however missing but workaround available
#extract individual field visit data by id and user profile data is present there
#then join all individual field visit files 

#get field visits data out (for editing)
url <- "https://bcenv-enmods-test.aqsamples.ca/api/v1/fieldvisits?limit=1000"
data_body <- list()

x_vsts <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

vsts <- fromJSON(rawToChar(x_vsts$content))$domainObjects %>% as_tibble() %>% 
  dplyr::select(id, project, planningStatus, auditAttributes, samplingLocation) %>% 
  unnest_wider(project, names_repair = "universal") %>% unnest_wider(auditAttributes, 
  names_repair = "universal") %>% unnest_wider(samplingLocation, names_repair = "universal")

#testing one field visit so we can test the call v1/fieldvisits/{id}
#it contains userProfile info which means we can get around the missing user profile in the 
#bulk field visit download
url <- "https://bcenv-enmods-test.aqsamples.ca/api/v1/fieldvisits/d6701c75-c826-40b0-b734-7d83b5388486"

data_body <- list()

x_vst <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#this way I can run this loop on all ids one by one and get user info
test_vst <- fromJSON(rawToChar(x_vst$content)) %>% unlist()

# field visit activities -------------------------------------------------------------
#code to get activity data out
#UserProfile is present 

#get field visits data out (for editing)
url <- "https://bcenv-enmods-test.aqsamples.ca/api/v1/activities?limit=1000"
data_body <- list()

x_actvty <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#names(fromJSON(rawToChar(x_actvty$content))$domainObjects)

actvty <- fromJSON(rawToChar(x_actvty$content))$domainObjects %>% as_tibble() %>% 
  dplyr::select(id, customId, auditAttributes, fieldVisit) %>% unnest_wider(auditAttributes, names_repair = "universal")

# specimens ---------------------------------------------------------------
#code to get specimen data out
#User Profile data is however missing but workaround available
#extract individual specimen data by id and user profile data is present there
#then join all individual specimen files

#get field visits data out (for editing)
url <- "https://bcenv-enmods-test.aqsamples.ca/api/v1/specimens?limit=1000"
data_body <- list()

x_spcmns <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

names(fromJSON(rawToChar(x_spcmns$content))$domainObjects)

spcmns <- fromJSON(rawToChar(x_spcmns$content))$domainObjects %>% as_tibble() %>% 
  dplyr::select(id, name, filtered, laboratory, auditAttributes, activity, status) %>% unnest_wider(auditAttributes, names_repair = "universal") #%>% unnest_wider(laboratory, names_repair = "universal") %>% unnest_wider(activity, names_repair = "universal")

#testing one specimen so we can test the call v1/fieldvisits/{id}
#it contains userProfile info which means we can get around the missing user profile in the 
#bulk specimen download
url <- "https://bcenv-enmods-test.aqsamples.ca/api/v1/specimens/41f81b50-932a-4fe1-a83d-f36c9265a695"

data_body <- list()

x_spcmn <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#this way I can run this loop on all ids one by one and get user info
test_spcmn <- fromJSON(rawToChar(x_spcmn$content)) %>% unlist()

# observations ------------------------------------------------------------
#code to get observation data out
#User Profile data is however missing but workaround available
#extract individual obs data by id and user profile data is present there
#then join all individual obs files

#get observations out (for editing)
url <- "https://bcenv-enmods-test.aqsamples.ca/api/v2/observations?limit=1000"
data_body <- list()

x_obs <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

obs <- fromJSON(rawToChar(x_obs$content))$domainObjects %>% unnest_wider(auditAttributes, names_repair = "universal")

#testing one obs so we can test the call v2/observations/{id}
#it contains userProfile info which means we can get around the missing user profile in the 
#bulk observation download
#url <- "https://bcenv-enmods-test.aqsamples.ca/api/v2/observations/ad45eeda-49e6-4186-bb5c-3e1a827877e7"
#no way to stack two observation ids and get both their data at the same time
url <- "https://bcenv-enmods-test.aqsamples.ca/api/v2/observations/9903f2a8-737d-41cb-bcf0-6f7007ba2dc0"

data_body <- list()

x_obs <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#this way I can run this loop on all ids one by one and get user info
test_obs <- fromJSON(rawToChar(x_obs$content)) %>% unlist()





# Implementing extraction and deletion of specific user related data -------------------------------------------------------------------
# deleting things in the following order allows smooth deletion
# analytical groups -------------------------------------------------------

#first let's try to get a list of all users; perhaps some info there

#get user lists out (for editing)
url <- "https://bcenv-enmods-test.aqsamples.ca/api/v2/users/"
data_body <- list()

x_usrs <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#names(fromJSON(rawToChar(x_usrs$content))$domainObjects)

usrs <- fromJSON(rawToChar(x_usrs$content))$domainObjects %>% select(id, customId, userProfile, accessGroups) %>% unnest_wider(userProfile, names_repair = "universal")

usr_id <- usrs %>% dplyr::filter(firstName == "Sahil" & lastName == "Bhandari") %>% dplyr::select(id...3) %>% unlist()

#get analytical groups out (for editing)
url <- "https://bcenv-enmods-test.aqsamples.ca/api/v1/analyticalgroups/"
data_body <- list()

x_ag <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#names(fromJSON(rawToChar(x_ag$content))$domainObjects)

ag <- fromJSON(rawToChar(x_ag$content))$domainObjects %>% select(id, name, description, type,  analyticalGroupItems, auditAttributes)

ag_select <- ag %>% dplyr::select(id, auditAttributes) %>% unnest_wider(auditAttributes) %>% dplyr::filter(creationUserProfileId == usr_id) %>%
  dplyr::select(id) %>% unlist()

#delete analytical group out with specific id
url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/analyticalgroups/", ag_select)
data_body <- list()

#Getting a list of relevant AG
x_ag_del <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#Confirm deleting the relevant AG
ag_del <- fromJSON(rawToChar(x_ag_del$content)) %>% unlist() %>% 
  keep(names(.) %in% c("id", "name", "type", "auditAttributes.creationUserProfileId"))

#Deleting the relevant AG
x_ag_del <- DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#Getting a list of relevant AG
x_ag_del <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#Confirm deleting the relevant AG
ag_del <- fromJSON(rawToChar(x_ag_del$content)) %>% unlist() %>% 
  keep(names(.) %in% c("id", "name", "type", "auditAttributes.creationUserProfileId"))



# field visit activities

# field visit activities --------------------------------------------------

#first let's try to get a list of all users; perhaps some info there

#get user lists out (for editing)
url <- "https://bcenv-enmods-test.aqsamples.ca/api/v2/users/"
data_body <- list()

x_usrs <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#names(fromJSON(rawToChar(x_usrs$content))$domainObjects)

usrs <- fromJSON(rawToChar(x_usrs$content))$domainObjects %>% select(id, customId, userProfile, accessGroups) %>% unnest_wider(userProfile, names_repair = "universal")

usr_id <- usrs %>% dplyr::filter(firstName == "Sahil" & lastName == "Bhandari") %>% dplyr::select(id...3) %>% unlist()

#get field visits data out (for editing)
url <- "https://bcenv-enmods-test.aqsamples.ca/api/v1/activities?limit=1000"
data_body <- list()

x_actvty <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#names(fromJSON(rawToChar(x_actvty$content))$domainObjects)

actvty <- fromJSON(rawToChar(x_actvty$content))$domainObjects %>% as_tibble() %>% 
  dplyr::select(id, customId, auditAttributes, fieldVisit) %>% unnest_wider(auditAttributes, names_repair = "universal")

actvty_select <- actvty %>% dplyr::select(id, creationUserProfileId) %>% dplyr::filter(creationUserProfileId == usr_id) %>% dplyr::select(id) %>% unlist()

#delete activities out with specific id
url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/activities/", actvty_select)
data_body <- list()

#Getting a list of relevant activities
x_actvty_del <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#Confirm deleting the relevant activities
actvty_del <- fromJSON(rawToChar(x_actvty_del$content)) %>% unlist() %>% 
  keep(names(.) %in% c("id", "customId", "fieldVisit.id", "auditAttributes.creationUserProfileId"))

#Deleting the relevant activities
x_actvty_del <- DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#Getting a list of relevant AG
x_actvty_del <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#Confirm deleting the relevant AG
actvty_del <- fromJSON(rawToChar(x_actvty_del$content)) %>% unlist() %>% 
  keep(names(.) %in% c("id", "name", "type", "auditAttributes.creationUserProfileId"))


# observations ------------------------------------------------------------

#first let's try to get a list of all users; perhaps some info there

#get user lists out (for editing)
url <- "https://bcenv-enmods-test.aqsamples.ca/api/v2/users/"
data_body <- list()

x_usrs <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#names(fromJSON(rawToChar(x_usrs$content))$domainObjects)

usrs <- fromJSON(rawToChar(x_usrs$content))$domainObjects %>% select(id, customId, userProfile, accessGroups) %>% unnest_wider(userProfile, names_repair = "universal")

usr_id <- usrs %>% dplyr::filter(firstName == "Sahil" & lastName == "Bhandari") %>% dplyr::select(id...3) %>% unlist()

#testing one observation so we can test the call v2/fieldvisits/{id}
#it contains userProfile info which means we can get around the missing user profile in the 
#bulk field visit download
url <- "https://bcenv-enmods-test.aqsamples.ca/api/v2/observations/9903f2a8-737d-41cb-bcf0-6f7007ba2dc0"

data_body <- list()

x_obs <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#this way I can run this loop on all ids one by one and get user info
test_obs <- fromJSON(rawToChar(x_obs$content)) %>% unlist()

# #get field visits data out (for editing)
# url <- "https://bcenv-enmods-test.aqsamples.ca/api/v2/observations?limit=1000"
# data_body <- list()
# 
# x_obs <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
# 
# obs <- fromJSON(rawToChar(x_obs$content))$domainObjects %>% as_tibble() %>% 
#   dplyr::select(id, project, auditAttributes, samplingLocation) %>% 
#   unnest_wider(project, names_repair = "universal") %>% unnest_wider(auditAttributes, 
#   names_repair = "universal") %>% unnest_wider(samplingLocation, names_repair = "universal")
# 
# obs_select <- obs %>% dplyr::select(id, creationUserProfileId) %>% dplyr::filter(creationUserProfileId == usr_id) %>% dplyr::select(id) %>% unlist()

# #delete observations out with specific id
# url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v2/observations/", obs_select)
# data_body <- list()

#Getting a list of relevant observations
x_obs_del <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#Confirm deleting the relevant activities
obs_del <- fromJSON(rawToChar(x_obs_del$content)) %>% unlist() %>% 
  keep(names(.) %in% c("id", "customId", "auditAttributes.creationUserProfileId"))

#Deleting the relevant activities
x_obs_del <- DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

obs_del <- fromJSON(rawToChar(x_obs_del$content))

# field visits -------------------------------------------------------------

#first let's try to get a list of all users; perhaps some info there

#get user lists out (for editing)
url <- "https://bcenv-enmods-test.aqsamples.ca/api/v2/users/"
data_body <- list()

x_usrs <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#names(fromJSON(rawToChar(x_usrs$content))$domainObjects)

usrs <- fromJSON(rawToChar(x_usrs$content))$domainObjects %>% select(id, customId, userProfile, accessGroups) %>% unnest_wider(userProfile, names_repair = "universal")

usr_id <- usrs %>% dplyr::filter(firstName == "Sahil" & lastName == "Bhandari") %>% dplyr::select(id...3) %>% unlist()

#testing one field visit so we can test the call v1/fieldvisits/{id}
#it contains userProfile info which means we can get around the missing user profile in the 
#bulk field visit download
url <- "https://bcenv-enmods-test.aqsamples.ca/api/v1/fieldvisits/d6701c75-c826-40b0-b734-7d83b5388486"

data_body <- list()

x_vst <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#this way I can run this loop on all ids one by one and get user info
test_vst <- fromJSON(rawToChar(x_vst$content)) %>% unlist()


# #get field visits data out (for editing)
# url <- "https://bcenv-enmods-test.aqsamples.ca/api/v1/fieldvisits?limit=1000"
# data_body <- list()
# 
# x_vsts <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
# 
# vsts <- fromJSON(rawToChar(x_vsts$content))$domainObjects %>% as_tibble() %>% 
#   dplyr::select(id, project, auditAttributes, samplingLocation) %>% 
#   unnest_wider(project, names_repair = "universal") %>% unnest_wider(auditAttributes, 
#   names_repair = "universal") %>% unnest_wider(samplingLocation, names_repair = "universal")
# 
# vst_select <- vsts %>% dplyr::select(id, creationUserProfileId) %>% dplyr::filter(creationUserProfileId == usr_id) %>% dplyr::select(id) %>% unlist()

# #delete activities out with specific id
# url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/activities/", vst_select)
# data_body <- list()
# 
#Getting a list of relevant activities
x_vst_del <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#Confirm deleting the relevant activities
vst_del <- fromJSON(rawToChar(x_vst_del$content)) %>% unlist() %>% 
  keep(names(.) %in% c("id", "project.id", "auditAttributes.creationUserProfileId"))

#Deleting the relevant activities
x_vst_del <- DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

vst_del <- fromJSON(rawToChar(x_vst_del$content)) %>% unlist()

# $message
# [1] "This item cannot be deleted because it is referenced by an item of type Observation"
# 
# $errorCode
# [1] "gaia.domain.exceptions.DeleteReferencedObjectException"
# 
# $localizationKey
# [1] "serverErrors.cannotDeleteBecauseOfReferenceByTypeX"
# 
# $localizationParameters
# [1] "Observation"
# 
# $requestId
# [1] "NginxRequestId-da277eb0ff1eba71bd74dd06669c8f5f"

#Getting a list of relevant AG
x_vst_del <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#Confirm deleting the relevant AG
vst_del <- fromJSON(rawToChar(x_vst_del$content)) %>% unlist() %>% 
  keep(names(.) %in% c("id", "name", "type", "auditAttributes.creationUserProfileId"))

# projects -------------------------------------------------------

#first let's try to get a list of all users; perhaps some info there

#get user lists out (for editing)
url <- "https://bcenv-enmods-test.aqsamples.ca/api/v2/users/"
data_body <- list()

x_usrs <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#names(fromJSON(rawToChar(x_usrs$content))$domainObjects)

usrs <- fromJSON(rawToChar(x_usrs$content))$domainObjects %>% select(id, customId, userProfile, accessGroups) %>% unnest_wider(userProfile, names_repair = "universal")

usr_id <- usrs %>% dplyr::filter(firstName == "Sahil" & lastName == "Bhandari") %>% 
  dplyr::select(id...3) %>% unlist()

#get projects out (for editing)
url <- "https://bcenv-enmods-test.aqsamples.ca/api/v1/projects/"
data_body <- list()

x_prjcts <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

prjcts <- fromJSON(rawToChar(x_prjcts$content))$domainObjects %>% unnest_wider(auditAttributes, names_repair = "universal")

prjct_select <- prjcts %>% dplyr::select(id, creationUserProfileId) %>% 
  dplyr::filter(creationUserProfileId == usr_id) %>%
  dplyr::select(id) %>% unlist()

#delete project with specific id
url <- str_c("https://bcenv-enmods-test.aqsamples.ca/api/v1/projects/", prjct_select)
data_body <- list()

#Getting a list of relevant AG
x_prjct_del <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#Confirm deleting the relevant AG
prjct_del <- fromJSON(rawToChar(x_prjct_del$content)) %>% unlist() %>% 
  keep(names(.) %in% c("id", "customId", "auditAttributes.creationUserProfileId"))

#Deleting the relevant project
x_prjct_del <- DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

status_prjct_del <- fromJSON(rawToChar(x_prjct_del$content)) %>% unlist()
# message 
# "This project cannot be deleted because it's still used by a field visit" 
# errorCode 
# "gaia.domain.exceptions.DataConflictException" 
# localizationKey 
# "views.project.errors.cannotDeleteBecauseUsedInFieldVisit" 
# requestId 
# "NginxRequestId-3211ec1974aa162397c03b662cb47b90"

#Getting a list of relevant projects
x_prjct_del <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#Confirm deleting the relevant AG
prjct_del <- fromJSON(rawToChar(x_prjct_del$content)) %>% unlist()