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
    
  } else if(data_type == "analysis_methods"){
    
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
    
  } else if(data_type == "sampling_agency"){
    
    ea_type = "Sampling Agency"
    
    id_data_type <- get_profiles(env, "extended_attributes") %>%
      dplyr::filter(customId == ea_type) %>% dplyr::select(id) %>% unlist()
    
    url <- str_c(base_url, "v1/extendedattributes/", id_data_type, "/dropdownlistitems")
    
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
    
  } else if(data_type == "analytical_groups"){
    
    url <- str_c(base_url, "v1/analyticalgroups")
    
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
  
  #env <- "prod"
  #data_type <- "result_grades"
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
    
  } else if(data_type == "analysis_methods"){
    
    url <- str_c(base_url, "v1/analysismethods/")
    
  } else if(data_type == "labs"){
    
    url <- str_c(base_url, "v1/laboratories/")
    
  } else if(data_type == "fish_taxonomy"){
    
    url <- str_c(base_url, "v1/taxons/")
    
  } else if(data_type == "collection_methods"){
    
    url <- str_c(base_url, "v1/collectionmethods/")
    
  } else if(data_type == "analytical_groups"){
    
    url <- str_c(base_url, "v1/analyticalgroups/")
    
  } else if(data_type == "projects"){
    
    url <- str_c(base_url, "v1/projects/")
    
  } else if(data_type == "locations"){
    
    url <- str_c(base_url, "v1/samplinglocations/")
    
  } else if(data_type == "location_groups"){
    
    url <- str_c(base_url, "v1/samplinglocationgroups/")
    
  } else if(data_type == "filters"){
    
    url <- str_c(base_url, "v1/filters/")
    
  } else if(data_type == "location_group_types"){
    
    put_profiles(env, "location_group_types", tibble(customId = character()))
    
    return()
    
  } else if(data_type == "location_types"){
    
    put_profiles(env, "location_types", tibble(customId = character()))
    
    return()
    
  } else if(data_type == "mediums"){
    
    mediums_required <- get_profiles(env, "mediums")
    
    if(!is.null(mediums_required)){
      
      mediums_required <- mediums_required %>% 
        dplyr::select(customId, systemCode) %>% 
        dplyr::filter(!is.na(systemCode))
      
      put_profiles(env, "mediums", mediums_required)
      
    } else {
      
      put_profiles(env, "mediums", tibble(customId = character()))
      
    }
    
    return()
    
  } else if(data_type == "taxonomy_levels"){
    
    put_profiles(env, "taxonomy_levels", tibble(customId = "No taxonomy inserted yet"))
    
    return()
    
  } else if(data_type == "detection_conditions"){
    
    url <- str_c(base_url, "v1/detectionconditions/")
    
  } else if(data_type == "result_grades"){
    
    result_grades <- get_profiles(env, "result_grades")
    
    if(!is.null(result_grades)){
    
    result_grades <- result_grades %>% 
      dplyr::select(customId, systemCode) %>% 
      dplyr::filter(!is.na(systemCode))
    
    put_profiles(env, "result_grades", result_grades)
    
    } else {
      
      put_profiles(env, "result_grades", tibble(customId = character()))
      
    }

    return()
    
  } else if(data_type == "result_statuses"){
    
    result_statuses <- get_profiles(env, "result_statuses")
    
    if(!is.null(result_statuses)){
    
      result_statuses <- result_statuses %>% 
      dplyr::select(customId, systemCode) %>% 
      dplyr::filter(!is.na(systemCode))
    
      put_profiles(env, "result_statuses", result_statuses)
    
    } else {
        
      put_profiles(env, "result_statuses", tibble(customId = character()))
      
      }
      
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
  
  if(!(data_type %in% c("result_grades", "result_statuses", 
                        "mediums"))){
  
    # Convert to tibble and then to list of named lists
  json_list <- profile %>% #tibble(customId = profile)
    mutate(row = row_number()) %>%
    nest(data = c(customId)) %>%
    pull(data) %>%
    map(~.x %>% as.list())
  
  } else {
    
    # Convert to tibble and then to list of named lists
    json_list <- profile %>% #tibble(customId = profile)
      mutate(row = row_number()) %>%
      nest(data = c(customId, systemCode)) %>%
      pull(data) %>%
      map(~.x %>% as.list())
    
  }
  
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
  # data_type = "analytical_groups"
  # 
  # profile <- analytical_groups
  
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
    
    rel_var <- c("sample_unit_group", "convertible")
    
    #EnMoDS labels: "customId", "supportsConversion"
    
  } else if(data_type == "units"){
    
    unit_groups <- profile %>% 
      dplyr::select(sample_unit_group, convertible) %>%
      #group_by(across(everything())) %>%
      #summarize(Count = n()) %>% 
      #ungroup() %>%
      mutate(sample_unit_group = case_when(
        sample_unit_group == "Length" ~ "SYS-REQUIRED - Length",
        .default = sample_unit_group
      )) %>% dplyr::filter(!is.na(convertible)) %>%
      unique()
    
    post_check <- post_profiles(env, "unit_groups", unit_groups)
    
    url <- paste0(base_url, "v1/units")
    
    #EnMoDS labels: "customId", "name", "baseMultiplier",
    # "baseOffset", "unitGroup.id", 
    # "unitGroup.supportsConversion"
    
    unitgroups_profiles <- get_profiles(env, "unit_groups")
    
    profile <- profile %>%
      left_join(unitgroups_profiles %>% 
                  dplyr::select(id, customId) %>%
                  rename(sample_unit_group_id = id), 
                by = join_by(sample_unit_group == customId), 
                keep = FALSE)
    
    rel_var <- c("conversion_factor", "offset", "convertible",
                 "sample_unit_group", "sample_unit_customid",
                 "sample_unit_name", "sample_unit_group_id")
    
  } else if(data_type == "extended_attributes"){
    
    url <- paste0(base_url, "v1/extendedattributes")
    
    rel_var <- c("customid", "data_type",
                 "applies_to_type", "description", "dropdownlist")
    
  } else if(data_type == "observed_properties"){
    
    #need to get unit group and unit IDs prior to importing observedProperties
    unit_groups <- get_profiles(env, "unit_groups") %>% 
      dplyr::select(id, customId) %>%
      rename("unit_group_id" = "id")
    
    #add GUID to the list of observedProperties for unit groups
    profile <- left_join(profile, unit_groups, 
                         by = join_by('sample_unit_group' == 'customId'), 
                         keep = FALSE)
    
    #units without groups
    units <- get_profiles(env, "units") %>% 
      dplyr::select(id, customId) %>%
      rename("unit_id" = "id")
    
    #add GUID to the list of observedProperties for units
    profile <- left_join(profile, units, 
                         by = join_by('sample_unit' == 'customId'), 
                         keep = FALSE)
    
    
    url <- paste0(base_url, "v1/observedproperties")
    
    rel_var <- c("parm_code", "newnameid", "description", "analysis_type",
                 "unit_group_id", "unit_id", "cas")
    
  } else if(data_type == "analysis_methods"){
    
    url <- paste0(base_url, "v1/analysismethods")
    
    rel_var <- c("method_code", "method", "method_description", "observed_properties_list")
    
  } else if(data_type == "labs"){
    
    url <- str_c(base_url, "v1/laboratories")
    
    rel_var <- c("id", "name", "description", "address", "contact_name", 
                 "email", "phone_number")
    
  } else if(data_type == "fish_taxonomy"){
    
    url <- str_c(base_url, "v1/taxons")
    
    taxonomylevels_profiles <- get_profiles(env, "taxonomy_levels")
    
    profile <- profile %>%
      left_join(taxonomylevels_profiles %>% 
                  dplyr::select(id, customId) %>%
                  rename(taxonomy_level_id = id), 
                by = join_by(level == customId), 
                keep = FALSE)
    
    rel_var <- c("taxonomy_level_id", "scientific_name", "common_name", "source", 
                 "comments", "itis_tsn")
    
  } else if(data_type == "collection_methods"){
    
    url <- str_c(base_url, "v1/collectionmethods")
    
    rel_var <- c("new_enmods_short_name/id", "merged_codes", "definition")
    
  } else if(data_type == "analytical_groups"){
    
    url <- str_c(base_url, "v1/analyticalgroups")
    
    #observed_properties <- get_profiles(env, "observed_properties")
    
    profile <- profile %>%
      left_join(observed_properties_get %>% 
                  dplyr::select(id, customId) %>%
                  rename(observed_property_id = id), 
                by = join_by(newnameid == customId), 
                keep = FALSE) %>% dplyr::select(-newnameid) %>% 
      group_by(op_group) %>% 
      summarize(
        analyticalgroupitems = list(
          map(observed_property_id, ~ list(observedProperty = list("id" = .x)))
        ),
        .groups = "drop"
      )
      
    rel_var <- c("op_group", "newnameid", "analyticalgroupitems")
    
  } else if(data_type == "detection_conditions"){
    
    url <- str_c(base_url, "v1/detectionconditions")
    
    rel_var <- c("customid", "name", "description", "system_code")
    
  } else if(data_type == "projects"){
    
    url <- str_c(base_url, "v1/projects")
    
    rel_var <- c("ID", "Name", "Type", "StartDate", "EndDate", 
                 "Comments", "Scope")
    
  } else if(data_type == "location_groups"){
    
    url <- str_c(base_url, "v1/samplinglocationgroups")
    
    rel_var <- c("permit_id", "location_group_type_id", "description")
    
  } else if(data_type == "filters"){
    
    url <- str_c(base_url, "v1/filters")
    
    rel_var <- c("id", "name", "comments", "sampling_locations")
    
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
        "supportsConversion" = temp_profile$convertible)
      
    } else if(data_type == "units"){
      
      #If the unit group supports conversion provide conversion factors
      if (temp_profile$convertible == TRUE) {
        
        data_body <- list(
          "customId" = temp_profile$sample_unit_customid,
          "name" = temp_profile$sample_unit_name,
          "baseMultiplier" = 1/temp_profile$conversion_factor,
          "baseOffset" = temp_profile$offset,
          "unitGroup" = list("id" = temp_profile$sample_unit_group_id))
        
      } else { 
        
        data_body <- list(
          "customId" = temp_profile$sample_unit_customid,
          "name" = temp_profile$sample_unit_name,
          "unitGroup" = list("id" = temp_profile$sample_unit_group_id))
      }
      
    } else if(data_type == "extended_attributes"){
      
      data_body <- list(
        "customId" = temp_profile$customid,
        "dataType" = temp_profile$data_type,
        "appliesToType" = temp_profile$applies_to_type,
        "description" = temp_profile$description
      )
      
      if(temp_profile$data_type == 'DROP_DOWN_LIST'){
        
        data_body$dropDownListItems <- temp_profile$dropdownlist[[1]]
        
      }
      
    } else if(data_type == "observed_properties"){
      
      data_body <- list(
        "customId" = temp_profile$newnameid,
        "name" = temp_profile$parm_code,
        "description" = temp_profile$description,
        "resultType" = "NUMERIC",
        "analysisType" = temp_profile$analysis_type,
        "unitGroup" = list("id" = temp_profile$unit_group_id),
        "defaultUnit" = list("id" = temp_profile$unit_id),
        "casNumber" = temp_profile$cas
      )
      
    } else if(data_type == "analysis_methods"){
      
      data_body <- list("methodId" = temp_profile$method_code,
                        "name" = temp_profile$method,
                        "description" = temp_profile$method_description,
                        "context" = "EMS Migration",
                        "observedProperties" = temp_profile$observed_properties_list[[1]]
      )
      
    } else if(data_type == "labs"){
      
      data_body <- list("customId" = temp_profile$id,
                        "name" = temp_profile$name,
                        "description" = temp_profile$description,
                        "address" = temp_profile$address,
                        "pointOfContact" = temp_profile$contact_name,
                        "emailAddress" = temp_profile$email,
                        "phoneNumber" = temp_profile$phone_number)
      
    } else if(data_type == "fish_taxonomy"){
      
      data_body <- list("scientificName" = temp_profile$scientific_name,
                        "commonName" = temp_profile$common_name,
                        "TaxonomyLevel" = list("id" = temp_profile$taxonomy_level_id),
                        "source" = temp_profile$source,
                        "comment" = temp_profile$comments,
                        "itisTsn" = temp_profile$itis_tsn,
                        "itisURL" = "www.google.ca")
      
    } else if(data_type == "collection_methods"){
      
      data_body <- list("customId" = temp_profile$"new_enmods_short_name/id",
                        "identifierOrganization" = temp_profile$merged_codes,
                        "name" = temp_profile$definition)
      
    } else if(data_type == "analytical_groups"){
      
      data_body <- list("name" = temp_profile$op_group,
                        "description" = "group description here",
                        "type" = "UNKNOWN",
                        "analyticalGroupItems" = temp_profile$analyticalgroupitems[[1]])
                        
    } else if(data_type == "detection_conditions"){
      
      data_body <- list("customId" = temp_profile$customid,
                        "name" = temp_profile$name,
                        "description" = temp_profile$description,
                        "systemCode" = temp_profile$system_code)
      
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
        "name" = temp_profile$permit_id,
        "description" = temp_profile$description,
        "LocationGroupType" = list("id" = temp_profile$location_group_type_id)
      )
      
    } else if(data_type == "filters"){
      
      data_body <- list("customId" = temp_profile$name,
                        "description" = temp_profile$comments,
                        "samplingLocations" = temp_profile$sampling_locations[[1]]
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