#Use this script to delete everything from a given location but keep the location
#record. This could be made into a function and used for loop through multiple locations
#there might be errors to do with attachments it hasn't been fully tested.

#this script is delete evryting from a given location

library(httr)
library(jsonlite)

#Get the api token and set the url
readRenviron(paste0(getwd(), "./.Renviron"))
token <- Sys.getenv("api_test_token")
base_url = 'https://bcenv-enmods-test.aqsamples.ca/api/'

#the custom ID of the location to remove everything from
loc_id <- "E273483"

#Start by deleting all observations using the location ID as the anchor. Get all location IDs

url <- paste0(base_url, "v1/samplinglocations?customId=", loc_id)
data_body <- list()
x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
print(paste("Location name:", fromJSON(rawToChar(x$content))$domainObjects$name))
loc_guid <- fromJSON(rawToChar(x$content))$domainObjects$id


#Delete all specimens which also deletes all observations for those specimens
#First get a list of specimen IDs for this location up to 1000 at a time
url <- paste0(base_url, 'v1/specimens?samplingLocationIds=', loc_guid, '&limit=1000')
x <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#in the case where there are more than 1000 specimen ID
total_no_specimens <- fromJSON(rawToChar(x$content))$total

print(paste0("removing specimens: ", total_no_specimens))

#keep trying until they are all gone
while(total_no_specimens > 0) {
  
  url <- paste0(base_url, 'v1/specimens?samplingLocationIds=', loc_guid, '&limit=1000')
  x <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  
  #in the case where there are more than 1000 specimen ID
  total_no_specimens <- fromJSON(rawToChar(x$content))$total
  
  print(total_no_specimens)
  
  specimen_ids <- fromJSON(rawToChar(x$content))$domainObjects$id
  
  i = 1
  while (i <= length(specimen_ids)) {
    
    #delete all observations for 200 specimens at once
    if (i%%200 == 0 | i == 1) {
      
      #if the number of specimens is not a nice round 1000 this catches it
      max_ix <- length(specimen_ids) - (i/200)*200
      if(max_ix > 200) {max_ix <- 200}
      
      #make the index for those specimen IDs that are going to be removed
      ix <- seq(1, max_ix) #limit is more than 200 and less than 250 has to do with length of URL
      
      url <- paste0(base_url, 'v2/observations?specimenIds=', paste(specimen_ids[ix], collapse = ","))
      
      data_body <- list()
      
      DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
    }
    
    #delete each specimen one by one... slow 
    url <- paste0(base_url, 'v1/specimens/', specimen_ids[i])
    DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
    print(paste0("specimen: ", i))
    i = i + 1 
  }
  
  #update the total number of specimens remaining
  url <- paste0('https://bcenv-training.aqsamples.com/api/v1/specimens?samplingLocationIds=', loc_id, '&limit=1000')
  x <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  total_no_specimens <- fromJSON(rawToChar(x$content))$total
  
  #if total no specimens is 0 it returns null so catch it
  if (is.null(total_no_specimens)) {total_no_specimens <- 0}
  
}

#activities
#delete is limited to 2000, get is limited to 1000

url <- paste0(base_url, 'v1/activities?samplingLocationIds=', loc_guid, '&limit=1000')
x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#in the case where there are more than 1000 activities
total_no_activities <- fromJSON(rawToChar(x$content))$total

print(paste0("removing activities: ", total_no_activities))

#keep trying until they are all gone
while(total_no_activities > 0) {
  
  url <- paste0(base_url, 'v1/activities?samplingLocationIds=', loc_guid, '&limit=1000')
  x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  
  #in the case where there are more than 1000 activities
  total_no_activities <- fromJSON(rawToChar(x$content))$total
  
  #get the id of the activities to delete
  activity_id <- fromJSON(rawToChar(x$content))$domainObjects$id
  
  #delete the activities in calls of 200 each
  ix = seq(0, total_no_activities, 200)
  
  #start index at 1
  ix[1] = ix[1] + 1
  
  #if the total number of activities is less than 1000 this gets the last index
  if (total_no_activities%%100 != 0) {ix[length(ix)+1] = total_no_activities}
  
  #catch and remove anything over 1000
  if (length(ix > 6)) {ix = ix[c(1,2,3,4,5,6)]}
  
  #remove NA that happen when there are less than six elements
  ix <- ix[!is.na(ix)]
  
  for (i in seq(1,length(ix)-1)) { #loop ends at five as only 1000 in total then go to higher level loop
    
    #in the case where there is an activity result it needs to be deleted before the activity can be deleted
    url <- paste0(base_url, 'v2/observations?activityIds=', paste(activity_id[seq(ix[i], ix[i+1])], collapse = ","))
    DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
    
    #maybe that weird error is caused by any use of EA? like depth lower?
    
    #now delete the activites
    url <- paste0(base_url, 'v1/activities?ids=', paste(activity_id[seq(ix[i], ix[i+1])], collapse = ","))
    x<-DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
    
    print(i)
    
  }
  
  url <- paste0(base_url, 'v1/activities?samplingLocationIds=', loc_guid, '&limit=1000')
  x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  
  #update the total to exit the loop
  total_no_activities <- fromJSON(rawToChar(x$content))$total
  
  print(paste("Number of reaminig activities:" ,total_no_activities))
  
}

### delete field visits
url <- paste0(base_url, 'v1/fieldvisits?samplingLocationIds=', loc_guid, '&limit=1000')

x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#in the case where there are more than 1000 field visits
total_no_field_visits <- fromJSON(rawToChar(x$content))$total

print(paste0("removing field_visits: ", total_no_field_visits))

#keep trying until they are all gone
while(total_no_field_visits > 0) {
  
  #get field vist IDs max of 1000 per request
  url <- paste0(base_url, 'v1/fieldvisits?samplingLocationIds=', loc_guid, '&limit=1000')
  x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  
  #get the id of the activities to delete
  field_visit_id <- fromJSON(rawToChar(x$content))$domainObjects$id
  
  #delete the activities in calls of 200 each
  ix = seq(0, total_no_field_visits, 200)
  
  #start index at 1
  ix[1] = ix[1] + 1
  
  #if the total number of activities is less than 1000 this gets the last index
  if (total_no_field_visits%%100 != 0) {ix[length(ix)+1] = total_no_field_visits}
  
  #catch and remove anything over 1000
  if (length(ix > 6)) {ix = ix[c(1,2,3,4,5,6)]}
  
  #remove NA that happen when there are less than six elements
  ix <- ix[!is.na(ix)]
  
  for (i in seq(1,length(ix)-1)) { #loop ends at five as only 1000 in total then go to higher level loop
    
    #in the case where there is an field result it needs to be deleted before the visit can be deleted or maybe not,,,?
    #url <- paste0('https://bcenv-training.aqsamples.com/api/v2/observations?fieldVisitId=', paste(field_visit_id[seq(ix[i], ix[i+1])], collapse = ","))
    #x<-DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
    
    #delete any attachments this is hard
    
    
    #now delete the field visit itself
    url <- paste0(base_url, 'v1/fieldvisits?ids=', paste(field_visit_id[seq(ix[i], ix[i+1])], collapse = ","))
    x<-DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
    
    print(i)
    
  }
  
  url <- paste0(base_url,'v1/fieldvisits?samplingLocationIds=', loc_guid, '&limit=1000')
  x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  
  #update the total to exit the loop
  total_no_field_visits <- fromJSON(rawToChar(x$content))$total
  
  print(paste("Number of reaminig field visits:" ,total_no_field_visits))
  
}



