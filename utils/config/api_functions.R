# GET FUNCTIONS -----------------------------------------------------------

get_profiles_for_url <- function(env, url){
  
  urlParameters <- update_baseURL_token(env)
  baseURL <- urlParameters[[1]]
  token <- urlParameters[[2]]
  
  data_body <- list()
  
  x_temp <- GET(url, config = c(add_headers(.headers = 
                                              c('Authorization' = token))), body = data_body, encode = 'json')
  
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
      
    }
    
  } else {
    
    temp <- fromJSON(rawToChar(x_temp$content))$domainObjects
    
  }
  
  return(temp)  
  
}

get_profiles <- function(env, dataType){
  
  #env <- "test"
  
  #default is "test" and for prod env, use the function parameter "prod"
  urlParameters <- update_baseURL_token(env)
  baseURL <- urlParameters[[1]]
  token <- urlParameters[[2]]
  
  #dataType <- "unitgroups"
  
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
  
  temp_profiles <- get_profiles_for_url(env, url)
  
  return(temp_profiles)
  
}

# DELETE FUNCTIONS -----------------------------------------------------------

del_profiles <- function(env, dataType){
  
  # env <- "prod"
  # 
  # dataType <- "taxonomylevels"#labs"#"observedproperties"
  
  temp_profile <- get_profiles(env, dataType)
  
  #default is "test" and for prod env, use the function parameter "prod"
  urlParameters <- update_baseURL_token(env)
  baseURL <- urlParameters[[1]]
  token <- urlParameters[[2]]
  
  if(dataType == "unitgroups"){
    
    del_profiles(env, "units")
    
    url <- str_c(baseURL, "v1/unitgroups/")
    
  } else if(dataType == "units"){
    
    del_profiles(env, "observedproperties")
    
    url <- str_c(baseURL, "v1/units/")
    
  } else if(dataType == "extendedattributes"){
    
    url <- str_c(baseURL, "v1/extendedattributes/")
    
  } else if(dataType == "observedproperties"){
    
    url <- str_c(baseURL, "v1/observedproperties/")
    
  } else if(dataType == "methods"){
    
    url <- str_c(baseURL, "v1/analysismethods/")
    
  } else if(dataType == "labs"){
    
    url <- str_c(baseURL, "v1/laboratories/")
    
  } else if(dataType == "fishtaxonomy"){
    
    url <- str_c(baseURL, "v1/taxons/")
    
  } else if(dataType == "collectionmethods"){
    
    url <- str_c(baseURL, "v1/collectionmethods/")
    
  } else if(dataType == "filters"){
    
    url <- str_c(baseURL, "v1/filters/")
    
  } else if(dataType == "projects"){
    
    url <- str_c(baseURL, "v1/projects/")
    
  } else if(dataType == "locations"){
    
    url <- str_c(baseURL, "v1/samplinglocations/")
    
  } else if(dataType == "locationgroups"){
    
    url <- str_c(baseURL, "v1/samplinglocationgroups/")
    
  } else if(dataType == "locationgrouptypes"){
    
    put_profiles("prod", "locationgrouptypes", tibble(customId = character()))
    
    return()
    
  } else if(dataType == "locationtypes"){
    
    put_profiles("prod", "locationtypes", tibble(customId = character()))
    
    return()
    
  } else if(dataType == "mediums"){
    
    mediums_required <- get_profiles("prod", "mediums") %>%
      dplyr::filter(!is.na(systemCode)) %>% 
      dplyr::select(customId)
    
    put_profiles("prod", "mediums", mediums_required)
    
    return()
    
  } else if(dataType == "taxonomylevels"){
    
    put_profiles("prod", "taxonomylevels", tibble(customId = character()))
    
    return()
    
  } else if(dataType == "detectionconditions"){
    
    url <- str_c(baseURL, "v1/detectionconditions/")
    
  } else if(dataType == "resultgrades"){
    
    put_profiles("prod", "resultgrades", tibble(customId = character()))
    
    return()
    
  } else if(dataType == "resultstatuses"){
    
    put_profiles("prod", "resultstatuses", tibble(customId = character()))
    
    return()
    
  }
  
  del_ids <- temp_profile$id
  
  i = 1
  
  for(id in del_ids){
    
    #id <- del_ids[1]
    
    data_body <- list()
    
    url_id <- str_c(url, id)
    
    #Make the unit group
    x<-DELETE(url_id, config = c(add_headers(.headers = c('Authorization' = token))), 
              body = data_body, encode = 'json')
    
    print(i)
    
    i = i + 1
    
  }
  
  return()
  
}

# PUT FUNCTIONS -----------------------------------------------------------

put_profiles <- function(env, dataType, profile){
  
  # env <- "prod"
  # 
  # dataType <- "resultgrades"
  # 
  # profile <- resultgrades
  
  #default is "test" and for prod env, use the function parameter "prod"
  urlParameters <- update_baseURL_token(env)
  baseURL <- urlParameters[[1]]
  token <- urlParameters[[2]]
  
  if(dataType == "taxonomylevels"){
    
    #update url to include dataType
    url <- str_c(baseURL, "v1/taxonomylevels")
    
  } else if(dataType == "locationgrouptypes"){
    
    #update url to include dataType
    url <- str_c(baseURL, "v1/samplinglocationgrouptypes")
    
  } else if(dataType == "locationtypes"){
    
    #update url to include dataType
    url <- str_c(baseURL, "v1/samplinglocationtypes")
    
  } else if(dataType == "mediums"){
    
    #update url to include dataType
    url <- str_c(baseURL, "v1/mediums")
    
  } else if(dataType == "resultgrades"){
    
    #update url to include dataType
    url <- str_c(baseURL, "v1/resultgrades")
    
  } else if(dataType == "resultstatuses"){
    
    #update url to include dataType
    url <- str_c(baseURL, "v1/resultstatuses")
    
  }
  
  # Convert to tibble and then to list of named lists
  json_list <- profile %>% #tibble(customId = profile)
    mutate(row = row_number()) %>%
    nest(data = c(customId)) %>%
    pull(data) %>%
    map(~.x %>% as.list())
  
  # Convert to JSON
  data_body <- toJSON(json_list, pretty = TRUE, auto_unbox = TRUE)
  #data_body = list()
  
  # PUT request
  x <- PUT(url, config = c(add_headers(.headers =
                                         c('Authorization' = token))), body = data_body,
           add_headers("Content-Type" = "application/json"),
           encode = 'json'
  )
  
  message <- fromJSON(rawToChar(x$content))
  
  return(message)
  #return()
  
}

# POST FUNCTIONS -----------------------------------------------------------

post_profiles <- function(env, dataType, profile){
  
  # env = "prod"
  # 
  # dataType = "methods"
  # 
  # profile <- methods.missing
  
  #Clean the old stuff out of the environment before posting new stuff
  if(!is.null(dim(get_profiles(env, dataType))[1])){
    
    del_profiles(env, dataType)
    
  }
  
  #default is "test" and for prod env, use the function parameter "prod"
  urlParameters <- update_baseURL_token(env)
  baseURL <- urlParameters[[1]]
  token <- urlParameters[[2]]
  
  if(dataType == "unitgroups"){
    
    #Clean the old stuff out of the environment before posting new stuff
    if(!is.null(dim(get_profiles(env, "units"))[1])){
      
      del_profiles(env, "units")
      
    }
    
    url <- paste0(baseURL, "v1/unitgroups")
    
    rel_var <- c("Sample.Unit.Group", "Convertible")
    
    #EnMoDS labels: "customId", "supportsConversion"
    
  } else if(dataType == "units"){
    
    unitGroups <- profile %>% 
      dplyr::select(Sample.Unit.Group, Convertible) %>%
      #group_by(across(everything())) %>%
      #summarize(Count = n()) %>% 
      #ungroup() %>%
      mutate(Sample.Unit.Group = case_when(
        Sample.Unit.Group == "Length" ~ "SYS-REQUIRED - Length",
        .default = Sample.Unit.Group
      )) %>% dplyr::filter(!is.na(Convertible)) %>%
      unique()
    
    post_check <- post_profiles(env, "unitgroups", unitGroups)
    
    url <- paste0(baseURL, "v1/units")
    
    #EnMoDS labels: "customId", "name", "baseMultiplier",
    # "baseOffset", "unitGroup.id", 
    # "unitGroup.supportsConversion"
    
    unitgroups_profiles <- get_profiles("prod", "unitgroups")
    
    profile <- profile %>%
      left_join(unitgroups_profiles %>% 
                  dplyr::select(id, customId) %>%
                  rename(Sample.Unit.GroupID = id), 
                by = join_by(Sample.Unit.Group == customId), 
                keep = FALSE)
    
    rel_var <- c("CONVERSION_FACTOR", "OFFSET", "Convertible",
                 "Sample.Unit.Group", "Sample.Unit.CustomId",
                 "Sample.Unit.Name", "Sample.Unit.GroupID")
    
  } else if(dataType == "extendedattributes"){
    
    url <- paste0(baseURL, "v1/extendedattributes")
    
    rel_var <- c("customId", "dataType",
                 "appliesToType", "description", "dropdownlist")
    
  } else if(dataType == "observedproperties"){
    
    #need to get unit group and unit IDs prior to importing observedProperties
    unitGroups <- get_profiles(env, "unitgroups") %>% 
      dplyr::select(id, customId) %>%
      rename("Unit.Group.Id" = "id")
    
    #add GUID to the list of observedProperties for unit groups
    profile <- left_join(profile, unitGroups, 
                         by = join_by('Sample.Unit.Group' == 'customId'), 
                         keep = FALSE)
    
    #units without groups
    units <- get_profiles("prod", "units") %>% 
      dplyr::select(id, customId) %>%
      rename("Unit.Id" = "id")
    
    #add GUID to the list of observedProperties for units
    profile <- left_join(profile, units, 
                         by = join_by('Sample.Unit' == 'customId'), 
                         keep = FALSE)
    
    
    url <- paste0(baseURL, "v1/observedproperties")
    
    rel_var <- c("Parm.Code", "NewNameID", "Description", "Analysis.Type",
                 "Unit.Group.Id", "Unit.Id", "CAS")
    
  } else if(dataType == "methods"){
    
    url <- paste0(baseURL, "v1/analysismethods")
    
    rel_var <- c("Method.Code", "Method", "Method.Description", "OPs.list")
    
  } else if(dataType == "labs"){
    
    url <- str_c(baseURL, "v1/laboratories")
    
    rel_var <- c("ID", "Name", "Description", "Address", "Point.Of.Contact", 
                 "Email", "Phone.Number")
    
  } else if(dataType == "fishtaxonomy"){
    
    url <- str_c(baseURL, "v1/taxons")
    
    taxonomylevels_profiles <- get_profiles("prod", "taxonomylevels")
    
    profile <- profile %>%
      left_join(taxonomylevels_profiles %>% 
                  dplyr::select(id, customId) %>%
                  rename(Taxonomy.Level.ID = id), 
                by = join_by(Level == customId), 
                keep = FALSE)
    
    rel_var <- c("Taxonomy.Level.ID", "Scientific Name", "Common Name", "Source", 
                 "Comments", "ITIS TSN")
    
  } else if(dataType == "collectionmethods"){
    
    url <- str_c(baseURL, "v1/collectionmethods")
    
    rel_var <- c("New EnMoDS Short Name/ID", "merged_codes", "Definition")
    
  } else if(dataType == "detectionconditions"){
    
    url <- str_c(baseURL, "v1/detectionconditions")
    
    rel_var <- c("customId", "name", "description", "systemCode")
    
  } else if(dataType == "filters"){
    
    url <- str_c(baseURL, "v1/filters")
    
    rel_var <- c("customId")
    
  } else if(dataType == "projects"){
    
    url <- str_c(baseURL, "v1/projects")
    
    rel_var <- c("ID", "Name", "Type", "StartDate", "EndDate", 
                 "Comments", "Scope")
    
  } else if(dataType == "locationgroups"){
    
    url <- str_c(baseURL, "v1/samplinglocationgroups")
    
    rel_var <- c("Permit ID", "locationgrouptypeID", "Description")
    
  }
  
  messages <- list()
  
  for(j in 1:dim(profile)[1]){
    
    #j <- 1
    
    temp_profile <- profile %>% 
      keep(names(.) %in% rel_var) %>% 
      slice(j)
    
    if(dataType == "unitgroups"){
      
      data_body <- list(
        "customId" = temp_profile$Sample.Unit.Group,
        "supportsConversion" = temp_profile$Convertible)
      
    } else if(dataType == "units"){
      
      #If the unit group supports conversion provide conversion factors
      if (temp_profile$Convertible == TRUE) {
        
        data_body <- list(
          "customId" = temp_profile$Sample.Unit.CustomId,
          "name" = temp_profile$Sample.Unit.Name,
          "baseMultiplier" = 1/temp_profile$CONVERSION_FACTOR,
          "baseOffset" = temp_profile$OFFSET,
          "unitGroup" = list("id" = temp_profile$Sample.Unit.GroupID))
        
      } else { 
        
        data_body <- list(
          "customId" = temp_profile$Sample.Unit.CustomId,
          "name" = temp_profile$Sample.Unit.Name,
          "unitGroup" = list("id" = temp_profile$Sample.Unit.GroupID))
      }
      
    } else if(dataType == "extendedattributes"){
      
      data_body <- list(
        "customId" = temp_profile$customId,
        "dataType" = temp_profile$dataType,
        "appliesToType" = temp_profile$appliesToType,
        "description" = temp_profile$description
      )
      
      if(temp_profile$dataType == 'DROP_DOWN_LIST'){
        
        data_body$dropDownListItems <- temp_profile$dropdownlist[[1]]
        
      }
      
    } else if(dataType == "observedproperties"){
      
      data_body <- list(
        "customId" = temp_profile$NewNameID,
        "name" = temp_profile$Parm.Code,
        "description" = temp_profile$Description,
        "resultType" = "NUMERIC",
        "analysisType" = temp_profile$Analysis.Type,
        "unitGroup" = list("id" = temp_profile$Unit.Group.Id),
        "defaultUnit" = list("id" = temp_profile$Unit.Id),
        "casNumber" = temp_profile$CAS
      )
      
    } else if(dataType == "methods"){
      
      data_body <- list("methodId" = temp_profile$Method.Code,
                        "name" = temp_profile$Method,
                        "description" = temp_profile$Method.Description,
                        "context" = "EMS Migration",
                        "observedProperties" = temp_profile$OPs.list[[1]]
      )
      
    } else if(dataType == "labs"){
      
      data_body <- list("customId" = temp_profile$ID,
                        "name" = temp_profile$Name,
                        "description" = temp_profile$Description,
                        "address" = temp_profile$Address,
                        "pointOfContact" = temp_profile$Point.Of.Contact,
                        "emailAddress" = temp_profile$Email,
                        "phoneNumber" = temp_profile$Phone.Number)
      
    } else if(dataType == "fishtaxonomy"){
      
      data_body <- list("scientificName" = temp_profile$`Scientific Name`,
                        "commonName" = temp_profile$`Common Name`,
                        "TaxonomyLevel" = list("id" = temp_profile$Taxonomy.Level.ID),
                        "source" = temp_profile$Source,
                        "comment" = temp_profile$Comments,
                        "itisTsn" = temp_profile$`ITIS TSN`,
                        "itisURL" = "www.google.ca")
      
    } else if(dataType == "collectionmethods"){
      
      data_body <- list("customId" = temp_profile$`New EnMoDS Short Name/ID`,
                        "identifierOrganization" = temp_profile$merged_codes,
                        "name" = temp_profile$Definition)
      
    } else if(dataType == "detectionconditions"){
      
      data_body <- list("customId" = temp_profile$customId,
                        "name" = temp_profile$name,
                        "description" = temp_profile$description,
                        "systemCode" = temp_profile$systemCode)
      
    } else if(dataType == "filters"){
      
      data_body <- list("customId" = temp_profile$customId)
      
    } else if(dataType == "projects"){
      
      data_body <- list(
        "customId" = temp_profile$ID, 
        "name" = temp_profile$Name, 
        "type" = temp_profile$Type, 
        "startTime" = temp_profile$StartDate, 
        "endTime" = temp_profile$EndDate, 
        "description" = temp_profile$Comments,
        "scopeStatement" = temp_profile$Scope)
      
    } else if(dataType == "locationgroups"){
      
      data_body <- list(
        "name" = temp_profile$`Permit ID`,
        "description" = temp_profile$Description,
        "LocationGroupType" = list("id" = temp_profile$locationgrouptypeID)
      )
      
    }
    
    #print(data_body)
    
    #Post the configuration
    x<-POST(url, config = c(add_headers(.headers = 
                                          c('Authorization' = token))), body = data_body, encode = 'json')
    
    #j <- 1
    
    messages[[j]] <- fromJSON(rawToChar(x$content))
    
    print(j)
    
  }
  
  post_check <- get_profiles(env, dataType)
  
  if(dim(post_check)[1] >= dim(profile)[1]){
    
    print("All items in reference list have likely been imported")
    
  } else {
    
    print("It seems like all items in reference list were not imported")
    
  }
  
  print(messages)
  
  return(messages)
  
}