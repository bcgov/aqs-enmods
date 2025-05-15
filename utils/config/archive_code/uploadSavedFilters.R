#Start of a function that allows the system administrator to upload saved filters
#to the system.

library(httr)
library(jsonlite)
library(dplyr)
library(writexl)
library(readxl)

#get the API token from your environment file
readRenviron(paste0(getwd(), "./.Renviron"))
token <- Sys.getenv("api_test_token")
base_url = Sys.getenv("url_test")

#read the sheet with the saved filters you want to upload
monitoring_groups <- read.csv("./data/EMS_Monitoring_Groups_March_21_2025.csv", stringsAsFactors = F)

#get guids for all locations
#maximum request is for 1000 locations at a time
url <- paste0(base_url, "v1/samplinglocations?limit=1000")
data_body <- list()

x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

total_locations = fromJSON(rawToChar(x$content))$totalCount

if (total_locations > 1000) { #if there are more than 10000 records loop
  
  element_id <- fromJSON(rawToChar(x$content))$domainObjects$id
  element_customId <- fromJSON(rawToChar(x$content))$domainObjects$customId
  
  number_loops = ceiling(total_locations/1000)
  
  for (i in seq(2,number_loops)) {
    cursor = fromJSON(rawToChar(x$content))$cursor
    tempURL = paste0(url, "&cursor=", cursor)
    
    x<-GET(tempURL, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
    
    temp_element_id <- fromJSON(rawToChar(x$content))$domainObjects$id
    temp_element_customId <- fromJSON(rawToChar(x$content))$domainObjects$customId
    
    element_id <- append(element_id, temp_element_id)
    element_customId <- append(element_customId, temp_element_customId)
    
    
    print(i)
  }
  
} else {
  
  element_id <- fromJSON(rawToChar(x$content))$domainObjects$id
  element_customId <- fromJSON(rawToChar(x$content))$domainObjects$customId
}

locations_w_guids <- data.frame("customId" = element_customId, "GUID" = element_id) #31,029 in total check against EnMoDS count

#join the monitoring groups with the location guids and drop locations that do not exist in enmods
monitoring_groups <-  merge(monitoring_groups, locations_w_guids, by.x = 'Location.ID', by.y = 'customId', all.x = T) #11,052

#locations with no join (not in EnMoDS) - D* P* type codes
locations_not_in_EnMoDS <- monitoring_groups %>% filter(is.na(GUID))

#filter out any that don't have a guid 
monitoring_groups <- monitoring_groups %>% filter(!is.na(GUID)) #10,602 

#nget the list of saved filters 
list_of_filter <- unique(monitoring_groups$NAME)

#make a saved filter for each monitoring group from EMS
for (i in seq(1, length(list_of_filter))) {
  one_filter <- monitoring_groups %>% filter(NAME == list_of_filter[i]) #get only the data for the ith filter
  locations_in_filter <- lapply(one_filter$GUID, function(x) list(id = x)) #make a list of location guids
  
  filter_des <- one_filter$DESCRIPTION[1] #get the filter description - i = 107 two descriptions for a single filter name not unique?
  
  #Make the new saved filter
  url <- paste0(base_url, "v1/filters/")
  data_body <- list('customId' = list_of_filter[i],
                    'description' = filter_des,
                    'samplingLocations' =  locations_in_filter
  )
  
  y<-POST(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  
  fromJSON(rawToChar(y$content))
  
  print(paste("uploading saved filter", i))
}
