#This series of functions are used to delete location and location related
#records like saved filters and location groups.

delete_location_obs <- function(base_url, loc_id, token) { 
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
      
      #now delete the activities
      url <- paste0(base_url, 'v1/activities?ids=', paste(activity_id[seq(ix[i], ix[i+1])], collapse = ","))
      x<-DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
      #print(x) #sometimes that weird delete error comes back and I can't delete activities no solution known
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

}

delete_all_records <- function(base_url, token) {
  #this function will delete all observations and location records
  
  #Start by deleting all observations using the location ID as the anchor. Get all location IDs
  url <- paste0(base_url, "v1/samplinglocations?limit=1000")
  data_body <- list()
  x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  total = fromJSON(rawToChar(x$content))$totalCount
  
  #if there are more than 1000 records loop to get all locations from the instance
  if (total > 1000) {
    
    element <- fromJSON(rawToChar(x$content))$domainObjects %>% 
      unnest(cols = c(type), names_sep = "_") %>%
      select(id, customId, name, type_customId, description, latitude, longitude, auditAttributes)
    
    number_loops = ceiling(total/1000)
    
    for (i in seq(2,number_loops)) {
      cursor = fromJSON(rawToChar(x$content))$cursor
      
      tempURL = paste0(url, "&cursor=", cursor)
      
      x<-GET(tempURL, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
      
      temp_element <- fromJSON(rawToChar(x$content))$domainObjects %>% 
        unnest(cols = c(type), names_sep = "_") %>%
        select(id, customId, name, type_customId, description, latitude, longitude, auditAttributes)
      
      element <- rbind(element, temp_element)
      
      print(paste("Gett location records:", i*1000)) #for every 1000s locations
    }
    
  } else {
    
    element <- fromJSON(rawToChar(x$content))$domainObjects
    
  }
  
  all_locations <- element
  
  #start a loop here to remove all observations, specimens, activities, field visits
  for (j in seq(1, nrow(all_locations))) {
    
    #delete everything from a location. Everything needs to be recursively deleted one level at a time starting at the bottom
    loc_id = all_locations$id[j]
    
    #obs - the system limits to 500 obs per delete call and seems to ignore the limit argument
    #url <- paste0('https://bcenv-training.aqsamples.com/api/v2/observations?samplingLocationIds=', loc_id)
    
    #data_body <- list()
    
    #DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
    
    #specimens
    #First get a list of specimen IDs for this location
    url <- paste0(base_url, 'v1/specimens?samplingLocationIds=', loc_id, '&limit=1000')
    x <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
    
    #in the case where there are more than 1000 specimen ID
    total_no_specimens <- fromJSON(rawToChar(x$content))$total
    
    print(paste("removing specimens for location:", all_locations$customId[j]))
    print(paste0("removing specimens: ", total_no_specimens))
    
    #keep trying until they are all gone
    while(total_no_specimens > 0) {
      
      url <- paste0(base_url, 'v1/specimens?samplingLocationIds=', loc_id, '&limit=1000')
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
          ix <- seq(1, max_ix) #limit is more than 200 and less than 250
          
          url <- paste0(base_url, 'v2/observations?specimenIds=', paste(specimen_ids[ix], collapse = ","))
          
          data_body <- list()
          
          DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
        }
        
        #delete each specimen one by one... slow AF
        url <- paste0(base_url, 'v1/specimens/', specimen_ids[i])
        x<-DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
        print(paste0("specimen: ", i))
        i = i + 1 
      }
      
      #update the total number of specimens remaining
      url <- paste0(base_url, 'v1/specimens?samplingLocationIds=', loc_id, '&limit=1000')
      x <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
      total_no_specimens <- fromJSON(rawToChar(x$content))$total
      
    }
    
    #activities
    #delete is limited to 2000, get is limited to 1000
    
    url <- paste0(base_url, 'v1/activities?samplingLocationIds=', loc_id, '&limit=1000')
    
    x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
    
    #in the case where there are more than 1000 activities
    total_no_activities <- fromJSON(rawToChar(x$content))$total
    
    print(paste("removing activities for location:", all_locations$customId[j]))
    print(paste0("removing activities: ", total_no_activities))
    
    #keep trying until they are all gone
    while(total_no_activities > 0) {
      
      url <- paste0(base_url, 'v1/activities?samplingLocationIds=', loc_id, '&limit=1000')
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
      
      url <- paste0(base_url, 'v1/activities?samplingLocationIds=', loc_id, '&limit=1000')
      x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
      
      #update the total to exit the loop
      total_no_activities <- fromJSON(rawToChar(x$content))$total
      
      print(paste("Number of reaminig activities:" ,total_no_activities))
      
    }
    
    ###
    
    #field visits
    url <- paste0(base_url, 'v1/fieldvisits?samplingLocationIds=', loc_id, '&limit=1000')
    
    x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
    
    #in the case where there are more than 1000 field visits
    total_no_field_visits <- fromJSON(rawToChar(x$content))$total
    
    print(paste0("removing field_visits: ", total_no_field_visits))
    
    #keep trying until they are all gone
    while(total_no_field_visits > 0) {
      
      #get field vist IDs max of 1000 per request
      url <- paste0(base_url, 'v1/fieldvisits?samplingLocationIds=', loc_id, '&limit=1000')
      x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
      
      #get the id of the activities to delete
      field_visit_id <- fromJSON(rawToChar(x$content))$domainObjects$id
      
      #loop through each field visit and delete all field results this must be done
      #one by one for some reason
      for (h in seq(1, length(field_visit_id))) {
        url <- paste0(base_url, 'v2/observations?fieldVisitId=', field_visit_id[h])
        DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
      }
      
      
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
        
        
        #delete any attachments this is hard
        
        
        #now delete the field visit itself
        url <- paste0(base_url, 'v1/fieldvisits?ids=', paste(field_visit_id[seq(ix[i], ix[i+1])], collapse = ","))
        x<-DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
        
        print(i)
        
      }
      
      url <- paste0(base_url, 'v1/fieldvisits?samplingLocationIds=', loc_id, '&limit=1000')
      x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
      
      #update the total to exit the loop
      total_no_field_visits <- fromJSON(rawToChar(x$content))$total
      
      print(paste("Number of reaminig field visits:" ,total_no_field_visits))
      
    }
    
    
    #Delete location record, note saved filters must be removed first
    
    url <- paste0(base_url, 'v1/samplinglocations/', loc_id)
    
    data_body <- list()
    
    x<-DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
    
    print(paste0("location removed: ", all_locations$customId[j]))
    
  }
}

delete_all_records_keep_locations <- function(base_url, token) {
  #this function will delete all observations and location records
  
  #Start by deleting all observations using the location ID as the anchor. Get all location IDs
  url <- paste0(base_url, "v1/samplinglocations?limit=1000")
  data_body <- list()
  x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  total = fromJSON(rawToChar(x$content))$totalCount
  
  #if there are more than 1000 records loop to get all locations from the instance
  if (total > 1000) {
    
    element <- fromJSON(rawToChar(x$content))$domainObjects %>% 
      unnest(cols = c(type), names_sep = "_") %>%
      select(id, customId, name, type_customId, description, latitude, longitude, auditAttributes)
    
    number_loops = ceiling(total/1000)
    
    for (i in seq(2,number_loops)) {
      cursor = fromJSON(rawToChar(x$content))$cursor
      
      tempURL = paste0(url, "&cursor=", cursor)
      
      x<-GET(tempURL, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
      
      temp_element <- fromJSON(rawToChar(x$content))$domainObjects %>% 
        unnest(cols = c(type), names_sep = "_") %>%
        select(id, customId, name, type_customId, description, latitude, longitude, auditAttributes)
      
      element <- rbind(element, temp_element)
      
      print(paste("Gett location records:", i*1000)) #for every 1000s locations
    }
    
  } else {
    
    element <- fromJSON(rawToChar(x$content))$domainObjects
    
  }
  
  all_locations <- element
  
  #start a loop here to remove all observations, specimens, activities, field visits
  for (j in seq(1, nrow(all_locations))) {
    
    #delete everything from a location. Everything needs to be recursively deleted one level at a time starting at the bottom
    loc_id = all_locations$id[j]
    
    #obs - the system limits to 500 obs per delete call and seems to ignore the limit argument
    #url <- paste0('https://bcenv-training.aqsamples.com/api/v2/observations?samplingLocationIds=', loc_id)
    
    #data_body <- list()
    
    #DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
    
    #specimens
    #First get a list of specimen IDs for this location
    url <- paste0(base_url, 'v1/specimens?samplingLocationIds=', loc_id, '&limit=1000')
    x <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
    
    #in the case where there are more than 1000 specimen ID
    total_no_specimens <- fromJSON(rawToChar(x$content))$total
    
    print(paste("removing specimens for location:", all_locations$customId[j]))
    print(paste0("removing specimens: ", total_no_specimens))
    
    #keep trying until they are all gone
    while(total_no_specimens > 0) {
      
      url <- paste0(base_url, 'v1/specimens?samplingLocationIds=', loc_id, '&limit=1000')
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
          ix <- seq(1, max_ix) #limit is more than 200 and less than 250
          
          url <- paste0(base_url, 'v2/observations?specimenIds=', paste(specimen_ids[ix], collapse = ","))
          
          data_body <- list()
          
          DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
        }
        
        #delete each specimen one by one... slow AF
        url <- paste0(base_url, 'v1/specimens/', specimen_ids[i])
        x<-DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
        print(paste0("specimen: ", i))
        i = i + 1 
      }
      
      #update the total number of specimens remaining
      url <- paste0(base_url, 'v1/specimens?samplingLocationIds=', loc_id, '&limit=1000')
      x <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
      total_no_specimens <- fromJSON(rawToChar(x$content))$total
      
    }
    
    #activities
    #delete is limited to 2000, get is limited to 1000
    
    url <- paste0(base_url, 'v1/activities?samplingLocationIds=', loc_id, '&limit=1000')
    
    x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
    
    #in the case where there are more than 1000 activities
    total_no_activities <- fromJSON(rawToChar(x$content))$total
    
    print(paste("removing activities for location:", all_locations$customId[j]))
    print(paste0("removing activities: ", total_no_activities))
    
    #keep trying until they are all gone
    while(total_no_activities > 0) {
      
      url <- paste0(base_url, 'v1/activities?samplingLocationIds=', loc_id, '&limit=1000')
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
      
      url <- paste0(base_url, 'v1/activities?samplingLocationIds=', loc_id, '&limit=1000')
      x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
      
      #update the total to exit the loop
      total_no_activities <- fromJSON(rawToChar(x$content))$total
      
      print(paste("Number of reaminig activities:" ,total_no_activities))
      
    }
    
    ###
    
    #field visits
    url <- paste0(base_url, 'v1/fieldvisits?samplingLocationIds=', loc_id, '&limit=1000')
    
    x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
    
    #in the case where there are more than 1000 field visits
    total_no_field_visits <- fromJSON(rawToChar(x$content))$total
    
    print(paste0("removing field_visits: ", total_no_field_visits))
    
    #keep trying until they are all gone
    while(total_no_field_visits > 0) {
      
      #get field vist IDs max of 1000 per request
      url <- paste0(base_url, 'v1/fieldvisits?samplingLocationIds=', loc_id, '&limit=1000')
      x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
      
      #get the id of the activities to delete
      field_visit_id <- fromJSON(rawToChar(x$content))$domainObjects$id
      
      #loop through each field visit and delete all field results this must be done
      #one by one for some reason
      for (h in seq(1, length(field_visit_id))) {
        url <- paste0(base_url, 'v2/observations?fieldVisitId=', field_visit_id[h])
        DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
      }
      
      
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
        
        
        #delete any attachments this is hard
        
        
        #now delete the field visit itself
        url <- paste0(base_url, 'v1/fieldvisits?ids=', paste(field_visit_id[seq(ix[i], ix[i+1])], collapse = ","))
        x<-DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
        
        print(i)
        
      }
      
      url <- paste0(base_url, 'v1/fieldvisits?samplingLocationIds=', loc_id, '&limit=1000')
      x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
      
      #update the total to exit the loop
      total_no_field_visits <- fromJSON(rawToChar(x$content))$total
      
      print(paste("Number of reaminig field visits:" ,total_no_field_visits))
      
    }
    
  }
}

delete_location_group <- function(base_url, token, customId){
  #write function to remove a single location group but keep locations
  
  #get all location groups from the system
  url <- paste0(base_url, "v1/samplinglocationgroups")
  data_body <- list()
  x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  
  all_location_groups <- fromJSON(rawToChar(x$content))$domainObjects
  
  #find the location group named by the user
  group_guid <- all_location_groups %>% filter(name == customId)
  
  #catch if no records found with that name
  if (nrow(group_guid) < 1) {
    print("No location group found by that name")
    return(null)
  }
  
  #remove the location group
  url <- paste0(base_url, "v1/samplinglocationgroups/", group_guid$id)
  DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  
}

delete_locations_wo_obs <- function(base_url, token) {
  
  #Get all location IDs in the system
  url <- paste0(base_url, "v1/samplinglocations?limit=1000")
  data_body <- list()
  x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  total = fromJSON(rawToChar(x$content))$totalCount
  
  #if there are more than 1000 records loop to get all locations from the instance
  if (total > 1000) {
    
    element <- fromJSON(rawToChar(x$content))$domainObjects %>% 
      unnest(cols = c(type), names_sep = "_") %>%
      select(id, customId, name, type_customId, description, latitude, longitude, auditAttributes)
    
    number_loops = ceiling(total/1000)
    
    for (i in seq(2,number_loops)) {
      cursor = fromJSON(rawToChar(x$content))$cursor
      
      tempURL = paste0(url, "&cursor=", cursor)
      
      x<-GET(tempURL, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
      
      temp_element <- fromJSON(rawToChar(x$content))$domainObjects %>% 
        unnest(cols = c(type), names_sep = "_") %>%
        select(id, customId, name, type_customId, description, latitude, longitude, auditAttributes)
      
      element <- rbind(element, temp_element)
      
      print(paste("Getting location records:", i*1000)) #for every 1000s locations
    }
    
  } else {
    
    element <- fromJSON(rawToChar(x$content))$domainObjects
    
  }
  
  all_locations <- element
  
  #now start deleting them, this only works if none of them have obs it's faster
  total_no_locations <- length(unique(all_locations$id))
  
  print(paste("Total Locations to be deleted:", total_no_locations))
  
  location_ids <- unique(all_locations$id)
  
  for (i in seq(1, length(location_ids))) {
    
    url <- paste0(base_url, 'v1/samplinglocations/', location_ids[i])
    data_body <- list()
    DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
    
    print(paste0("Deleted: ", all_locations$customId[i], " ", all_locations$name[i]))
  }
  
 }

delete_all_location_groups <- function(base_url, token) {
  #write function to delete all location groups but keep location records
  
  #get all location groups from the system
  url <- paste0(base_url, "v1/samplinglocationgroups")
  data_body <- list()
  x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  
  all_location_groups <- fromJSON(rawToChar(x$content))$domainObjects
  
  
  #catch if no records found with that name
  if (nrow(all_location_groups) < 1) {
    print("No location groups found!")
    return(null)
  }
  
  #remove all the location group
  for (i in seq(1, nrow(all_location_groups))) {
    url <- paste0(base_url, "v1/samplinglocationgroups/", all_location_groups$id[i])
    DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
    print(paste("Deleted location group:", all_location_groups$name[i], all_location_groups$description[i]))
  }
  
}

delete_saved_filter <- function(base_url, token, customId) {
  
}

delete_all_saved_filters <- function(base_url, token, customId) {
    #write function to delete all saved_filters
    
    #get all location groups from the system
    url <- paste0(base_url, "v1/filters")
    data_body <- list()
    x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
    
    saved_filters <- fromJSON(rawToChar(x$content))$domainObjects
    
    #catch if no records found with that name
    if (nrow(saved_filters) < 1) {
      print("No saved filters found!")
      return(null)
    }
    
    #remove all the location group
    for (i in seq(1, nrow(saved_filters))) {
      url <- paste0(base_url, "v1/filters/", saved_filters$id[i])
      DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
      print(paste("Deleted saved filter:", saved_filters$customId[i]))
    }
    
}