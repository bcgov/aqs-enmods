#Things to work on:
#Spell checks and updates
#Standardization of column names: Sample or Samples

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
library(readxl)

#get the API tokens from your environment file
readRenviron(paste0(getwd(), "./.Renviron"))
test_token <- Sys.getenv("test_token")
prod_token <- Sys.getenv("prod_token")
test_url <- Sys.getenv("test_url")
prod_url <- Sys.getenv("prod_url")

#function to update URL/token based on chosen environment
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

#by default global variables always reference "test" environment
url_parameters <- update_base_url_token("test")
base_url <- url_parameters[[1]]
token <- url_parameters[[2]]

### PREPROCESSING REFERENCE SHEETS ### ----

### UNITS and UNIT GROUPS ### ----

units <- read_excel("./utils/config/ReferenceLists/Units.xlsx", 
                    sheet = "Updated")

#we know there are duplicates in this data set
units <- units %>% select(c(CONVERSION_FACTOR, Convertible,
                            Sample.Unit.Group, Samples.Unit.CustomId,
                            Samples.Unit.Name))
units <- distinct(units)

#Elevation does not exist in the Reference Sheet
#Needs to be added manually even though it's metres
#This metres is therefore different from metre (m) in EnMoDS
units <- units %>% add_row(CONVERSION_FACTOR = 1, 
                           Convertible = TRUE,
                           Sample.Unit.Group = "SYS-REQUIRED - Length", 
                           Samples.Unit.CustomId = "metre",
                           Samples.Unit.Name = "elevation")

#list of unit groups
#sometimes this list may have one less or more unit groups than in ENV
#This is because AQS automatically creates Length if no unit groups present
#This added Unit Group may be called "SYS-REQUIRED - Length"
unit_groups <- units %>% 
  dplyr::select(Sample.Unit.Group, Convertible) %>% 
  group_by(across(everything())) %>%
  summarize(Count = n()) %>% 
  ungroup()

# #loop to make all the unit groups
# for (i in seq(1, dim(unit_groups)[1])) {
#   
#   #get the updated base_url and token for the prod env
#   url_parameters <- update_base_url_token("prod")
#   base_url <- url_parameters[[1]]
#   token <- url_parameters[[2]]
#   
#   url <- paste0(base_url, "v1/unitgroups")
#   data_body <- list("customId" = unit_groups$Sample.Unit.Group[i],
#                   "supportsConversion" = unit_groups$Convertible[i])
#   
#   #Make the unit group
#   x<-POST(url, config = c(add_headers(.headers = 
#     c('Authorization' = token))), body = data_body, encode = 'json')
#   
#   #get the unit group's id
#   unit_group_id <- fromJSON(rawToChar(x$content))$id
#   
#   #if the unit group already exist get the id
#   if (is.null(unit_group_id)) {
#     
#     url = paste0(base_url, 'v1/unitgroups?customId=', 
#                  unit_groups$Sample.Unit.Group[i])
#     data_body <- list()
#     x<-GET(url, config = c(add_headers(.headers = 
#       c('Authorization' = token))), body = data_body, encode = 'json')
#     unit_group_id <- fromJSON(rawToChar(x$content))$domainObjects$id
#     
#   }
#   
#   #get all the units for the i-th group
#   unit_group_units <- units %>% 
#     dplyr::filter(Sample.Unit.Group == unit_groups$Sample.Unit.Group[i])
#   
#   #loop to put all the units in the group
#   for (j in seq(1, nrow(unit_group_units))) {
#     
#     #If the unit group supports conversion provide conversion factors
#     if (unit_groups$Convertible[j] == TRUE) {
#       url <- paste0(base_url, "v1/units")
#       data_body <- list("customId" = unit_group_units$Samples.Unit.CustomId[j],
#                         "name" = unit_group_units$Samples.Unit.Name[j],
#                         "baseMultiplier" = 1/unit_group_units$CONVERSION_FACTOR[j],
#                         "baseOffset" = 0,
#                         "unitGroup" = list("id" = unit_group_id))
#       POST(url, config = c(add_headers(.headers = 
#         c('Authorization' = token))), body = data_body, encode = 'json')
#     } #else if the group does not support conversion do not provide conversion factors
#     else {
#       url <- paste0(base_url, "v1/units")
#       data_body <- list("customId" = unit_group_units$Samples.Unit.CustomId[j],
#                         "name" = unit_group_units$Samples.Unit.Name[j],
#                         "unitGroup" = list("id" = unit_group_id))
#       POST(url, config = c(add_headers(.headers = 
#         c('Authorization' = token))), body = data_body, encode = 'json')
#     }
#     
#   }
#   
# }

### OBSERVED PROPERTIES ### ----
OPs <- read_excel("./utils/config/ReferenceLists/Observed_Properties.xlsx")

#The units don't matter for non-convertable OP units. For example microbial units. So remove them from consideration.
OPs$Sample.Unit[OPs$Convertable.In.Samples == "N"] <- ""

#clean up modifers
OPs$Modifier[is.na(OPs$Modifier)] <- ""

#test merging the EMS parm code with unit group to make 'new codes'that can be safely used for upload
OPs$NewCode <- paste0(OPs$Parm.Code, "-", OPs$Sample.Unit.Group, 
                      OPs$Modifier)

#get the unique list of OP IDs
OPs_unique <- OPs %>% 
                dplyr::select(c("Parm.Code", "NewNameID", "Description", 
                                "Analysis.Type", "Sample.Unit.Group", 
                                "Sample.Unit","CAS", "NewCode")) %>% 
                unique() %>% 
                group_by(NewNameID) %>% 
                mutate(Count = n()) %>%
                ungroup()
                                          
#the total number of unique NewNameID should be the same as the total number of unique rows
newNameID_unique <- OPs_unique$NewNameID %>% unique()

#they are not! 
#current pipeline will upload the first OP into the system
#But its fine because...
#they belong to unit groups that are convertible
OPs_problematic <- OPs_unique %>% dplyr::filter(Count>1)

#need to get unit IDs prior to importing OPs
url = paste0(base_url, 'v1/unitgroups')
data_body <- list()
x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
unit_grps <- fromJSON(rawToChar(x$content))$domainObjects %>% select("id", "customId")

#units without groups
url = paste0(base_url, 'v1/units')
data_body <- list()
x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
units <- fromJSON(rawToChar(x$content))$domainObjects %>% select("id", "customId")


# Delete configuration ----------------------------------------------------

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

del_profiles <- function(env, data_type){
  
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

del_check <- del_profiles("prod", "units")

del_check <- del_profiles("prod", "unitgroups")

post_profiles <- function(env, data_type, profile){
  
  env = "prod"

  data_type = "unitgroups"

  profile <- unit_groups %>%
    dplyr::filter(customId == "ppt")

  #default is "test" and for prod env, use the function parameter "prod"
  url_parameters <- update_base_url_token(env)
  base_url <- url_parameters[[1]]
  token <- url_parameters[[2]]
  
  # profile <- profile %>% 
  #   mutate(across(everything(), ~ replace(., is.na(.), "")))
  
  if(data_type == "unitgroups"){
    
    url <- paste0(base_url, "v1/unitgroups")
    
    rel_var <- c("Sample.Unit.Group", "Convertible")
    
    #EnMoDS labels: "customId", "supportsConversion"

  } else if(data_type == "units"){
    
    url <- paste0(base_url, "v1/units")
    
    rel_var <- c("CONVERSION_FACTOR", "Convertible",
                 "Sample.Unit.Group", "Samples.Unit.CustomId",
                 "Samples.Unit.Name")
    
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
    
  }
  
  for(j in 1:dim(profile)[1]){
    
    #j <- 1
    
    temp_profile <- profile %>% 
      keep(names(.) %in% rel_var) %>%
      slice(j) %>%
      as.list()
    
    data_body <- temp_profile
    
    if(data_type == "unitgroups"){
      
      data_body <- list(
        "customId" = temp_profile$Sample.Unit.Group,
        "supportsConversion" = temp_profile$Convertible)
      
    } else if(data_type == "units"){
      
      #loop to put all the units in the group
      
      #If the unit group supports conversion provide conversion factors
      if (temp_profile$Convertible == TRUE) {
        
        data_body <- list(
          "customId" = temp_profile$Samples.Unit.CustomId,
          "name" = temp_profile$Samples.Unit.Name,
          "baseMultiplier" = 1/temp_profile$CONVERSION_FACTOR,
          "baseOffset" = 0,
          "unitGroup" = list("id" = temp_profile$Sample.Unit.GroupID))
        
      } else { 
        
        data_body <- list(
          "customId" = temp_profile$Samples.Unit.CustomId,
          "name" = temp_profile$Samples.Unit.Name,
          "unitGroup" = list("id" = temp_profile$Sample.Unit.GroupID))
        
      }
      
    }
    
    #Post the configuration
    x<-POST(url, config = c(add_headers(.headers = 
        c('Authorization' = token))), body = data_body, encode = 'json')
    
    message <- fromJSON(rawToChar(x$content))
    
    print(j)
    
  }
  
  return(message)
  
}

post_profiles("prod", "unitgroups", unit_groups)
