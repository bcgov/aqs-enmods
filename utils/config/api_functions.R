# GET FUNCTIONS -----------------------------------------------------------

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
    
    #i = 2
    
    for (i in seq(2,number_loops)) {
      
      cursor = fromJSON(rawToChar(x_temp$content))$cursor
      
      temp_url = paste0(url, "&cursor=", cursor)
      
      x_temp <- GET(temp_url, config = c(add_headers(.headers = 
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
  
  #env <- "test"
  
  #default is "test" and for prod env, use the function parameter "prod"
  url_parameters <- update_base_url_token(env)
  base_url <- url_parameters[[1]]
  token <- url_parameters[[2]]
  
  #data_type <- "unit_groups"
  
  if(data_type == "units"){
    
    url <- str_c(base_url, "v1/units")
    
  } else if(data_type == "unit_groups"){
    
    url <- str_c(base_url, "v1/unitgroups")
    
  } else if(data_type == "extended_attributes"){
    
    url <- str_c(base_url, "v1/extendedattributes")
    
  } else if(data_type == "observed_properties"){
    
    url <- str_c(base_url, "v1/observedproperties")
    
  } else if(data_type == "methods"){
    
    url <- str_c(base_url, "v1/analysismethods")
    
  } else if(data_type == "labs"){
    
    url <- str_c(base_url, "v1/laboratories")
    
  } else if(data_type == "location_group_types"){
    
    url <- str_c(base_url, "v1/samplinglocationgrouptypes")
    
  } else if(data_type == "location_types"){
    
    url <- str_c(base_url, "v1/samplinglocationtypes")
    
  } else if(data_type == "location_groups"){
    
    url <- str_c(base_url, "v1/samplinglocationgroups")
    
  } else if(data_type == "locations"){
    
    url <- str_c(base_url, "v1/samplinglocations?limit=1000")
    
  } else if(data_type == "mediums"){
    
    url <- str_c(base_url, "v1/mediums")
    
  } else if(data_type == "taxonomy_levels"){
    
    url <- str_c(base_url, "v1/taxonomylevels")
    
  } else if(data_type == "detection_conditions"){
    
    url <- str_c(base_url, "v1/detectionconditions")
    
  } else if(data_type == "result_grades"){
    
    url <- str_c(base_url, "v1/resultgrades")
    
  } else if(data_type == "result_statuses"){
    
    url <- str_c(base_url, "v1/resultstatuses")
    
  } else if(data_type == "fish_taxonomy"){
    
    url <- str_c(base_url, "v1/taxons")
    
  } else if(data_type == "collection_methods"){
    
    url <- str_c(base_url, "v1/collectionmethods")
    
  } else if(data_type == "filters"){
    
    url <- str_c(base_url, "v1/filters")
    
  } else if(data_type == "projects"){
    
    url <- str_c(base_url, "v1/projects")
    
  }
  
  temp_profiles <- get_profiles_for_url(env, url)
  
  return(temp_profiles)
  
}

# DELETE FUNCTIONS -----------------------------------------------------------

del_profiles <- function(env, data_type){
  
  # env <- "prod"
  # 
  # data_type <- "taxonomy_levels"#labs"#"observed_properties"
  
  temp_profile <- get_profiles(env, data_type)
  
  #default is "test" and for prod env, use the function parameter "prod"
  url_parameters <- update_base_url_token(env)
  base_url <- url_parameters[[1]]
  token <- url_parameters[[2]]
  
  if(data_type == "unit_groups"){
    
    del_profiles(env, "units")
    
    url <- str_c(base_url, "v1/unitgroups/")
    
  } else if(data_type == "units"){
    
    del_profiles(env, "observed_properties")
    
    url <- str_c(base_url, "v1/units/")
    
  } else if(data_type == "extended_attributes"){
    
    url <- str_c(base_url, "v1/extendedattributes/")
    
  } else if(data_type == "observed_properties"){
    
    url <- str_c(base_url, "v1/observedproperties/")
    
  } else if(data_type == "methods"){
    
    url <- str_c(base_url, "v1/analysismethods/")
    
  } else if(data_type == "labs"){
    
    url <- str_c(base_url, "v1/laboratories/")
    
  } else if(data_type == "fish_taxonomy"){
    
    url <- str_c(base_url, "v1/taxons/")
    
  } else if(data_type == "collection_methods"){
    
    url <- str_c(base_url, "v1/collectionmethods/")
    
  } else if(data_type == "filters"){
    
    url <- str_c(base_url, "v1/filters/")
    
  } else if(data_type == "projects"){
    
    url <- str_c(base_url, "v1/projects/")
    
  } else if(data_type == "locations"){
    
    url <- str_c(base_url, "v1/samplinglocations/")
    
  } else if(data_type == "location_groups"){
    
    url <- str_c(base_url, "v1/samplinglocationgroups/")
    
  } else if(data_type == "location_group_types"){
    
    put_profiles("prod", "location_group_types", tibble(customId = character()))
    
    return()
    
  } else if(data_type == "location_types"){
    
    put_profiles("prod", "location_types", tibble(customId = character()))
    
    return()
    
  } else if(data_type == "mediums"){
    
    mediums_required <- get_profiles("prod", "mediums") %>%
      dplyr::filter(!is.na(systemCode)) %>% 
      dplyr::select(customId)
    
    put_profiles("prod", "mediums", mediums_required)
    
    return()
    
  } else if(data_type == "taxonomy_levels"){
    
    put_profiles("prod", "taxonomy_levels", tibble(customId = character()))
    
    return()
    
  } else if(data_type == "detection_conditions"){
    
    url <- str_c(base_url, "v1/detectionconditions/")
    
  } else if(data_type == "result_grades"){
    
    put_profiles("prod", "result_grades", tibble(customId = character()))
    
    return()
    
  } else if(data_type == "result_statuses"){
    
    put_profiles("prod", "result_statuses", tibble(customId = character()))
    
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

put_profiles <- function(env, data_type, profile){
  
  # env <- "prod"
  # 
  # data_type <- "result_grades"
  # 
  # profile <- resultgrades
  
  #default is "test" and for prod env, use the function parameter "prod"
  url_parameters <- update_base_url_token(env)
  base_url <- url_parameters[[1]]
  token <- url_parameters[[2]]
  
  if(data_type == "taxonomy_levels"){
    
    #update url to include data_type
    url <- str_c(base_url, "v1/taxonomylevels")
    
  } else if(data_type == "location_group_types"){
    
    #update url to include data_type
    url <- str_c(base_url, "v1/samplinglocationgrouptypes")
    
  } else if(data_type == "location_types"){
    
    #update url to include data_type
    url <- str_c(base_url, "v1/samplinglocationtypes")
    
  } else if(data_type == "mediums"){
    
    #update url to include data_type
    url <- str_c(base_url, "v1/mediums")
    
  } else if(data_type == "result_grades"){
    
    #update url to include data_type
    url <- str_c(base_url, "v1/resultgrades")
    
  } else if(data_type == "result_statuses"){
    
    #update url to include data_type
    url <- str_c(base_url, "v1/resultstatuses")
    
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

post_profiles <- function(env, data_type, profile){
  
  # env = "prod"
  # 
  # data_type = "methods"
  # 
  # profile <- methods.missing
  
  #Clean the old stuff out of the environment before posting new stuff
  if(!is.null(dim(get_profiles(env, data_type))[1])){
    
    del_profiles(env, data_type)
    
  }
  
  #default is "test" and for prod env, use the function parameter "prod"
  url_parameters <- update_base_url_token(env)
  base_url <- url_parameters[[1]]
  token <- url_parameters[[2]]
  
  if(data_type == "unit_groups"){
    
    #Clean the old stuff out of the environment before posting new stuff
    if(!is.null(dim(get_profiles(env, "units"))[1])){
      
      del_profiles(env, "units")
      
    }
    
    url <- paste0(base_url, "v1/unitgroups")
    
    rel_var <- c("sample_unit_group", "Convertible")
    
    #EnMoDS labels: "customId", "supportsConversion"
    
  } else if(data_type == "units"){
    
    unitGroups <- profile %>% 
      dplyr::select(sample_unit_group, Convertible) %>%
      #group_by(across(everything())) %>%
      #summarize(Count = n()) %>% 
      #ungroup() %>%
      mutate(sample_unit_group = case_when(
        sample_unit_group == "Length" ~ "SYS-REQUIRED - Length",
        .default = sample_unit_group
      )) %>% dplyr::filter(!is.na(Convertible)) %>%
      unique()
    
    post_check <- post_profiles(env, "unit_groups", unitGroups)
    
    url <- paste0(base_url, "v1/units")
    
    #EnMoDS labels: "customId", "name", "baseMultiplier",
    # "baseOffset", "unitGroup.id", 
    # "unitGroup.supportsConversion"
    
    unitgroups_profiles <- get_profiles("prod", "unit_groups")
    
    profile <- profile %>%
      left_join(unitgroups_profiles %>% 
                  dplyr::select(id, customId) %>%
                  rename(sample_unit_group_id = id), 
                by = join_by(sample_unit_group == customId), 
                keep = FALSE)
    
    rel_var <- c("CONVERSION_FACTOR", "OFFSET", "Convertible",
                 "sample_unit_group", "Sample.Unit.CustomId",
                 "Sample.Unit.Name", "sample_unit_group_id")
    
  } else if(data_type == "extended_attributes"){
    
    url <- paste0(base_url, "v1/extendedattributes")
    
    rel_var <- c("customId", "data_type",
                 "appliesToType", "description", "dropdownlist")
    
  } else if(data_type == "observed_properties"){
    
    #need to get unit group and unit IDs prior to importing observedProperties
    unitGroups <- get_profiles(env, "unit_groups") %>% 
      dplyr::select(id, customId) %>%
      rename("Unit.Group.Id" = "id")
    
    #add GUID to the list of observedProperties for unit groups
    profile <- left_join(profile, unitGroups, 
                         by = join_by('sample_unit_group' == 'customId'), 
                         keep = FALSE)
    
    #units without groups
    units <- get_profiles("prod", "units") %>% 
      dplyr::select(id, customId) %>%
      rename("Unit.Id" = "id")
    
    #add GUID to the list of observedProperties for units
    profile <- left_join(profile, units, 
                         by = join_by('Sample.Unit' == 'customId'), 
                         keep = FALSE)
    
    
    url <- paste0(base_url, "v1/observedproperties")
    
    rel_var <- c("Parm.Code", "NewNameID", "Description", "Analysis.Type",
                 "Unit.Group.Id", "Unit.Id", "CAS")
    
  } else if(data_type == "methods"){
    
    url <- paste0(base_url, "v1/analysismethods")
    
    rel_var <- c("Method.Code", "Method", "Method.Description", "OPs.list")
    
  } else if(data_type == "labs"){
    
    url <- str_c(base_url, "v1/laboratories")
    
    rel_var <- c("ID", "Name", "Description", "Address", "Point.Of.Contact", 
                 "Email", "Phone.Number")
    
  } else if(data_type == "fish_taxonomy"){
    
    url <- str_c(base_url, "v1/taxons")
    
    taxonomylevels_profiles <- get_profiles("prod", "taxonomy_levels")
    
    profile <- profile %>%
      left_join(taxonomylevels_profiles %>% 
                  dplyr::select(id, customId) %>%
                  rename(Taxonomy.Level.ID = id), 
                by = join_by(Level == customId), 
                keep = FALSE)
    
    rel_var <- c("Taxonomy.Level.ID", "Scientific Name", "Common Name", "Source", 
                 "Comments", "ITIS TSN")
    
  } else if(data_type == "collection_methods"){
    
    url <- str_c(base_url, "v1/collectionmethods")
    
    rel_var <- c("New EnMoDS Short Name/ID", "merged_codes", "Definition")
    
  } else if(data_type == "detection_conditions"){
    
    url <- str_c(base_url, "v1/detectionconditions")
    
    rel_var <- c("customId", "name", "description", "systemCode")
    
  } else if(data_type == "filters"){
    
    url <- str_c(base_url, "v1/filters")
    
    rel_var <- c("customId")
    
  } else if(data_type == "projects"){
    
    url <- str_c(base_url, "v1/projects")
    
    rel_var <- c("ID", "Name", "Type", "StartDate", "EndDate", 
                 "Comments", "Scope")
    
  } else if(data_type == "location_groups"){
    
    url <- str_c(base_url, "v1/samplinglocationgroups")
    
    rel_var <- c("Permit ID", "locationgrouptypeID", "Description")
    
  }
  
  messages <- list()
  
  for(j in 1:dim(profile)[1]){
    
    #j <- 1
    
    temp_profile <- profile %>% 
      keep(names(.) %in% rel_var) %>% 
      slice(j)
    
    if(data_type == "unit_groups"){
      
      data_body <- list(
        "customId" = temp_profile$sample_unit_group,
        "supportsConversion" = temp_profile$Convertible)
      
    } else if(data_type == "units"){
      
      #If the unit group supports conversion provide conversion factors
      if (temp_profile$Convertible == TRUE) {
        
        data_body <- list(
          "customId" = temp_profile$Sample.Unit.CustomId,
          "name" = temp_profile$Sample.Unit.Name,
          "baseMultiplier" = 1/temp_profile$CONVERSION_FACTOR,
          "baseOffset" = temp_profile$OFFSET,
          "unitGroup" = list("id" = temp_profile$sample_unit_group_id))
        
      } else { 
        
        data_body <- list(
          "customId" = temp_profile$Sample.Unit.CustomId,
          "name" = temp_profile$Sample.Unit.Name,
          "unitGroup" = list("id" = temp_profile$sample_unit_group_id))
      }
      
    } else if(data_type == "extended_attributes"){
      
      data_body <- list(
        "customId" = temp_profile$customId,
        "data_type" = temp_profile$data_type,
        "appliesToType" = temp_profile$appliesToType,
        "description" = temp_profile$description
      )
      
      if(temp_profile$data_type == 'DROP_DOWN_LIST'){
        
        data_body$dropDownListItems <- temp_profile$dropdownlist[[1]]
        
      }
      
    } else if(data_type == "observed_properties"){
      
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
      
    } else if(data_type == "methods"){
      
      data_body <- list("methodId" = temp_profile$Method.Code,
                        "name" = temp_profile$Method,
                        "description" = temp_profile$Method.Description,
                        "context" = "EMS Migration",
                        "observed_properties" = temp_profile$OPs.list[[1]]
      )
      
    } else if(data_type == "labs"){
      
      data_body <- list("customId" = temp_profile$ID,
                        "name" = temp_profile$Name,
                        "description" = temp_profile$Description,
                        "address" = temp_profile$Address,
                        "pointOfContact" = temp_profile$Point.Of.Contact,
                        "emailAddress" = temp_profile$Email,
                        "phoneNumber" = temp_profile$Phone.Number)
      
    } else if(data_type == "fish_taxonomy"){
      
      data_body <- list("scientificName" = temp_profile$`Scientific Name`,
                        "commonName" = temp_profile$`Common Name`,
                        "TaxonomyLevel" = list("id" = temp_profile$Taxonomy.Level.ID),
                        "source" = temp_profile$Source,
                        "comment" = temp_profile$Comments,
                        "itisTsn" = temp_profile$`ITIS TSN`,
                        "itisURL" = "www.google.ca")
      
    } else if(data_type == "collection_methods"){
      
      data_body <- list("customId" = temp_profile$`New EnMoDS Short Name/ID`,
                        "identifierOrganization" = temp_profile$merged_codes,
                        "name" = temp_profile$Definition)
      
    } else if(data_type == "detection_conditions"){
      
      data_body <- list("customId" = temp_profile$customId,
                        "name" = temp_profile$name,
                        "description" = temp_profile$description,
                        "systemCode" = temp_profile$systemCode)
      
    } else if(data_type == "filters"){
      
      data_body <- list("customId" = temp_profile$customId)
      
    } else if(data_type == "projects"){
      
      data_body <- list(
        "customId" = temp_profile$ID, 
        "name" = temp_profile$Name, 
        "type" = temp_profile$Type, 
        "startTime" = temp_profile$StartDate, 
        "endTime" = temp_profile$EndDate, 
        "description" = temp_profile$Comments,
        "scopeStatement" = temp_profile$Scope)
      
    } else if(data_type == "location_groups"){
      
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
  
  post_check <- get_profiles(env, data_type)
  
  if(dim(post_check)[1] >= dim(profile)[1]){
    
    print("All items in reference list have likely been imported")
    
  } else {
    
    print("It seems like all items in reference list were not imported")
    
  }
  
  print(messages)
  
  return(messages)
  
}