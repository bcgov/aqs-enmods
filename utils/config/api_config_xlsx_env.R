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
library(writexl)
library(openxlsx)

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

# UNITS and UNIT GROUPS ----
unitgroups_profiles <- get_profiles("prod", "unitgroups")

# units_profiles <- get_profiles("prod", "units") %>% 
#   dplyr::select(-auditAttributes) %>%
#   rename(Sample.Unit.Id = id, Sample.Unit.CustomId = customId,
#          Sample.Unit.Name = name) %>%
#   unnest_wider(unitGroup) %>% 
#   rename(Sample.Unit.Group = customId) %>%
#   dplyr::select(-c(auditAttributes, id))
  
units <- read_csv("./utils/config/ReferenceLists/Units_ems_jk_2025_04_16.csv") %>% mutate(MEAS_UNIT_CD = as.character(MEAS_UNIT_CD)) %>% 
  mutate(CODE = as.character(CODE)) %>% 
  dplyr::select(-CONVERSION_FACTOR)

#Only conversions in this file are reliable
units_base <- read_excel("./utils/config/ReferenceLists/Units.xlsx", sheet = "Units") %>% dplyr::select(c(CODE, CONVERSION_FACTOR, OFFSET, Sample.Unit.Group, Sample.Unit.CustomId, Sample.Unit.Name))

units <- units %>% left_join(units_base %>%
           dplyr::select(CODE, Sample.Unit.Group, Sample.Unit.CustomId, 
                         Sample.Unit.Name, CONVERSION_FACTOR, OFFSET), 
           by = join_by("CODE")) %>%
            mutate(Sample.Unit.Group = case_when(
              SHORT_NAME == "‰" ~ "AmountOfSubstancePerVolume",
              SHORT_NAME == "mg/dscm" ~ "AirConcentration",
              SHORT_NAME == "% (Recovery)" ~ "DimensionlessRatio",
              SHORT_NAME == "N/A" ~ "None",
              .default = Sample.Unit.Group
            )) %>% 
  mutate(CONVERSION_FACTOR = if_else(SHORT_NAME == "mg/dscm", 1000, CONVERSION_FACTOR)) %>%
  dplyr::select(-CODE) %>% unique()

#we know there are duplicates in this data set
units <- units %>% 
            mutate(Sample.Unit.Name = if_else(is.na(Sample.Unit.Name), 
            DESCRIPTION, Sample.Unit.Name), 
            Sample.Unit.CustomId = if_else(is.na(Sample.Unit.CustomId),                            SHORT_NAME, Sample.Unit.CustomId)) %>% 
            dplyr::select(c(Sample.Unit.CustomId, Sample.Unit.Name, CONVERSION_FACTOR, OFFSET, Sample.Unit.Group)) %>% unique()

units_new_to_enmods <- read_excel("./utils/config/ReferenceLists/Units_new_to_enmods.xlsx", sheet = "NewUnits") %>% 
  dplyr::select(c(Sample.Unit.Group, Sample.Unit.CustomId, Sample.Unit.Name, CONVERSION_FACTOR, OFFSET))

units <- units %>% 
  bind_rows(units_new_to_enmods) %>% 
  unique()

#Sentence case in general
#Accounting for special cases
units <- units %>% 
  mutate(Sample.Unit.Name = str_to_sentence(Sample.Unit.Name)) %>%
  mutate(Sample.Unit.Name = ifelse(Sample.Unit.Name == "Ph units", 
                                   "pH units", Sample.Unit.Name))

units <- units %>% left_join(unitgroups_profiles %>% 
           dplyr::select(id, customId, supportsConversion), 
           by = join_by("Sample.Unit.Group" == "customId")) %>%
          rename(Convertible = supportsConversion)

#Elevation does not exist in the Reference Sheet
#Needs to be added manually even though it's metres
#This metres is therefore different from metre (m) in EnMoDS
units <- units %>% add_row(CONVERSION_FACTOR = 1,
                           OFFSET = 0,
                           Convertible = TRUE,
                           Sample.Unit.Group = "SYS-REQUIRED - Length", 
                           Sample.Unit.CustomId = "metre",
                           Sample.Unit.Name = "Elevation")

#fixing the units file for anomalous Count group associated with No/m2
units <- units %>% 
  mutate(Convertible = ifelse(Sample.Unit.Name == "Number per square meter", 
                              FALSE, Convertible)) %>%
  mutate(OFFSET = if_else(is.na(OFFSET), 0, OFFSET))

# #110 only; even though 117 in the units file
# get_check <- get_profiles("prod", "units")
# 
# #no such units
# units_na <- units %>% dplyr::filter(is.na(Sample.Unit.CustomId))
# 
# units_missing <- units %>% 
#                   anti_join(get_check, 
#                     by = join_by("Sample.Unit.CustomId" == "customId"))

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


# EXTENDED ATTRIBUTES ----

extendedAttributes <- read_excel("./utils/config/ReferenceLists/ExtendedAttributes.xlsx", 
                                 sheet = "ExtendedAttributes")

dropdownLists <- read_excel("./utils/config/ReferenceLists/ExtendedAttributes.xlsx", 
                            sheet = "DropdownLists")

dropdownLists <- dropdownLists %>%
  dplyr::select(EA_customId, DDL_customId) %>%
  group_by(EA_customId) %>%
  summarise(
    dropdownlist = list(
      map(DDL_customId, ~ list(customId = .x))
    ),
    .groups = "drop"
  )

extendedAttributes <- extendedAttributes %>%
  left_join(dropdownLists, by = join_by(customId == EA_customId),
            keep = FALSE)

# DETECTION CONDITIONS ----

# #Had to run this only once in a lifetime
# detectionConditions <- get_profiles("test", "detectionconditions") #%>%
#   #dplyr::select(customId)
# 
# # Save workbook
# write_xlsx(list(detectionConditions), "./utils/config/ReferenceLists/DetectionConditions.xlsx")
# 
# # Load an existing workbook
# wb <- loadWorkbook("./utils/config/ReferenceLists/DetectionConditions.xlsx")
# 
# # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
# renameWorksheet(wb, sheet = "Sheet1", newName = "detectionconditions")
# 
# # Save the workbook with the updated sheet name
# saveWorkbook(wb, "./utils/config/ReferenceLists/DetectionConditions.xlsx", overwrite = TRUE)

detectionConditions <- read_excel("./utils/config/ReferenceLists/DetectionConditions.xlsx", 
                                 sheet = "detectionconditions")

# SAVED FILTERS ----

# #Had to run this only once in a lifetime
# savedFilters <- get_profiles("test", "filters") #%>%
#   #dplyr::select(customId)
# 
# # Save workbook
# write_xlsx(list(savedFilters), "./utils/config/ReferenceLists/savedfilters.xlsx")
# 
# # Load an existing workbook
# wb <- loadWorkbook("./utils/config/ReferenceLists/savedfilters.xlsx")
# 
# # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
# renameWorksheet(wb, sheet = "Sheet1", newName = "savedfilters")
# 
# # Save the workbook with the updated sheet name
# saveWorkbook(wb, "./utils/config/ReferenceLists/savedfilters.xlsx", overwrite = TRUE)

savedFilters <- read_excel("./utils/config/ReferenceLists/savedfilters.xlsx", 
                                  sheet = "savedfilters")


# PROJECTS ----

projects_test <- get_profiles("test", "projects")

projects <- read_excel("./utils/config/ReferenceLists/projects.xlsx", 
                           sheet = "projects")

#Type needs to be in case UPPER
projects <- projects %>% 
                mutate(StartDate = as.POSIXct(StartDate),
                       EndDate = as.POSIXct(EndDate)) %>%
                mutate(Type = toupper(Type), 
                       StartDate = case_when(
                           is.na(StartDate) ~ NA,
                           .default =  format(StartDate, "%Y-%m-%dT00:00:00%z")
                       ), 
                       EndDate = case_when(
                         is.na(EndDate) ~ NA,
                         .default =  format(EndDate, "%Y-%m-%dT00:00:00%z")
                       )) 
                       
# OBSERVED PROPERTIES ----
#need to get unit group and unit IDs prior to importing OPs
unit_groups <- get_profiles("prod", "unitgroups") %>% 
  rename("unit.group.id" = "id")

#units without groups
units <- get_profiles("prod", "units") %>% 
  rename("unit.id" = "id")

#EMS exported OPs
OPs <- read_excel("./utils/config/ReferenceLists/Observed_Properties.xlsx")

#The units don't matter for non-convertable OP units. For example microbial units. So remove them from consideration.
OPs$Sample.Unit[OPs$Convertable.In.Samples == "N"] <- ""

#clean up modifers
OPs$Modifier[is.na(OPs$Modifier)] <- ""

#Add an empty Result.Type column
OPs$Result.Type <- ""

#get the unique list of OP IDs
OPs <- OPs %>% dplyr::select(c("Parm.Code", "NewNameID", "Description", 
                               "Analysis.Type", "Result.Type", "Sample.Unit.Group", 
                               "Sample.Unit", "CAS")) #%>% unique()

## OPs new to EnMoDS not in EMS
OPs_new_to_EnMoDS <- 
  read_excel("./utils/config/ReferenceLists/New_OPS_not_in_EMS.xlsx", 
             sheet = "OPs_new")

#get the unique list of OP IDs
OPs_new_to_EnMoDS <- OPs_new_to_EnMoDS %>% 
  dplyr::select(c("Parm.Code", "NewNameID", "Description", 
                  "Analysis.Type", "Result.Type", "Sample.Unit.Group", 
                  "Sample.Unit","CAS")) #%>% unique()

#fixing the missing sample group id issue in the list
OPs_new_to_EnMoDS <- OPs_new_to_EnMoDS %>% mutate(Sample.Unit.Group = 
              if_else(NewNameID == "Biological Sample Volume (vol.)", 
                      "Volume", Sample.Unit.Group))

## Taxonomic OPs
OPs_taxonomic <- read.csv("./utils/config/ReferenceLists/Taxonomic_OP.csv", stringsAsFactors = F) #1607

#get the unique list of OP IDs
OPs_taxonomic <- OPs_taxonomic %>% 
  dplyr::select(c("Parm.Code", "NewNameID", "Description", "Analysis.Type", 
                  "Result.Type", "Sample.Unit.Group", "Sample.Unit","CAS")) #%>%  unique()

#use CAS for ITIS ID? Nope not allowed eye roll
#OPs_unique$CAS <- unlist(lapply(str_split(OPs_unique$NewNameID, " - "), function(x) x[[1]][1]))

#error holder these two are missing!
#"39369 - Carex" 734
#"40371 - Agropyron cristatum" 741

## Merge all OPs and process

#get the unique list of OP IDs
OPs <- bind_rows(OPs, OPs_new_to_EnMoDS, OPs_taxonomic) %>%
        unique() %>% 
        group_by(NewNameID) %>% 
        mutate(Count = n()) %>%
        ungroup()
                                          
#the total number of unique NewNameID should be the same as the total number of unique rows
newNameID_unique <- OPs$NewNameID %>% unique()

OPs <- OPs %>% 
  mutate(Sample.Unit = ifelse(Sample.Unit == "pH Units", 
                                   "pH units", Sample.Unit))

#rename the required OPs so they show up otherwise they are in the background?
OPs <- OPs %>% 
  mutate(Sample.Unit.Group = case_when(
    NewNameID == "Biological Sex (cat.)" ~ "None",
    NewNameID == "Biological Life Stage (cat.)" ~ "None",
    .default = Sample.Unit.Group
  )) %>% 
  mutate(Analysis.Type = case_when(
    NewNameID == "Biological Sex (cat.)" ~ "BIOLOGICAL",
    NewNameID == "Biological Life Stage (cat.)" ~ "BIOLOGICAL",
    .default = Analysis.Type
  ))

#they are not! 
#current pipeline will upload the first OP into the system
#But its fine because...
#they belong to unit groups that are convertible
#Below code helps you make that check
OPs_problematic <- OPs %>% 
                    dplyr::filter(Count>1) %>%
                    left_join(unit_groups %>% 
                    dplyr::select(customId, supportsConversion), 
                      by = join_by("Sample.Unit.Group" == "customId")) %>%
                    dplyr::filter(supportsConversion == FALSE)

#add GUID to the list of OPs for unit groups
OPs <- left_join(OPs, unit_groups, 
                by = join_by('Sample.Unit.Group' == 'customId'), keep = FALSE)

#add GUID to the list of OPs for units
OPs <- left_join(OPs, units, 
                by = join_by('Sample.Unit' == 'customId'), keep = FALSE)
                      
#analysis.Type must be ALL CAPS
OPs$Analysis.Type <- toupper(OPs$Analysis.Type)
OPs$Result.Type <- toupper(OPs$Result.Type)

# Checking which entries are not getting posted
get_check <- get_profiles("prod", "observedproperties")
#not all OPs getting posted
#compare get_check for OPs with raw OPs
OPs_not_posted <- OPs %>% anti_join(get_check,
                                    by = join_by("NewNameID" == "customId"))
# 
# OPs_not_posted <- OPs %>% dplyr::filter(NewNameID == "Biological Sample Volume (vol.)")

# METHODS -----------------------------------------------------------------
Methods <- read_excel("./utils/config/ReferenceLists/Observed_Properties.xlsx")

#Method names cannot contain semicolons

#The units don't matter for non convertable OP units. For example microbial units. So remove them from consideration.
Methods$Sample.Unit[Methods$Convertable.In.Samples == "N"] <- ""

#get the new name (customId) and methods 
Methods <- Methods %>% select(c("NewNameID", "Method.Code", "Method", "Method.Description"))

OPs_ids <- get_profiles("prod", "observedproperties") %>% 
  dplyr::select("id", "customId")

#check that nothing has been dropped here!
Methods_problematic <- anti_join(Methods, OPs_ids, by = join_by("NewNameID" == "customId")) 

Methods <- left_join(Methods, OPs_ids, by = join_by("NewNameID" == "customId"))
# 
# list_methods <- unique(Methods %>% select(-c("NewNameID", "id")))
# 
# update_base_url_token("prod")
# 
# for (i in seq(1, nrow(list_methods))) {
#   
#   OPs_for_method <- Methods %>% filter(Method.Code == list_methods$Method.Code[i]) %>% select(id)
#   
#   result_list = list()
#   
#   for (j in seq(1, nrow(OPs_for_method))) {
#     result_list[[j]] = list("id" = OPs_for_method$id[j])
#   }
#   
#   
#   #Make a new method and assign a single OP
#   url <- paste0(base_url, 'v1/analysismethods')
#   data_body <- list("methodId" = list_methods$Method.Code[i],
#                     "name" = list_methods$Method[i],
#                     "description" = list_methods$Method.Description[i],
#                     "context" = "EMS Migration",
#                     "observedProperties" = result_list
#   )
#   
#   
#   x<-POST(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
#   
#   if (x$status_code != 200) {
#     print(i)
#     print(x$status_code)
#     print(list_methods$Method[i])
#   }
#   
# }

OPs_for_methods <- Methods %>%
  dplyr::select(id, Method.Code) %>%
  group_by(Method.Code) %>%
  summarise(
    OPs_list = list(
      map(id, ~ list("id" = .x))
    ),
    .groups = "drop"
  )

Methods <- unique(Methods %>% select(-c("NewNameID", "id")))

Methods <- Methods %>%
  left_join(OPs_for_methods, by = join_by(Method.Code == Method.Code),
            keep = FALSE)

# LABS --------------------------------------------------------------------
Labs <- read.csv("./utils/config/ReferenceLists/Labs.csv", stringsAsFactors = F)

Labs$Description = str_c("Created by ", Labs$WHO_CREATED, " on ", Labs$WHEN_CREATED)

# TAXONOMY LEVELS ---------------------------------------------------------
##Had to run this only once in a lifetime
# taxonomylevels <- get_profiles("test", "taxonomylevels") %>%
#   dplyr::select(customId)
# # Save workbook
# write_xlsx(list(taxonomylevels), "./utils/config/ReferenceLists/TaxonomyLevels.xlsx")
# 
# # Load an existing workbook
# wb <- loadWorkbook("./utils/config/ReferenceLists/TaxonomyLevels.xlsx")
# 
# # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
# renameWorksheet(wb, sheet = "Sheet1", newName = "taxonomylevels")
# 
# # Save the workbook with the updated sheet name
# saveWorkbook(wb, "./utils/config/ReferenceLists/TaxonomyLevels.xlsx", overwrite = TRUE)

taxonomylevels <- read_excel("./utils/config/ReferenceLists/TaxonomyLevels.xlsx", 
                             sheet = "taxonomylevels")

# LOCATION GROUP TYPES ---------------------------------------------------------
##Had to run this only once in a lifetime
# locationgrouptypes <- get_profiles("test", "samplinglocationgrouptypes") %>%
#   dplyr::select(customId)
# # Save workbook
# write_xlsx(list(locationgrouptypes), "./utils/config/ReferenceLists/LocationGroupTypes.xlsx")
# 
# # Load an existing workbook
# wb <- loadWorkbook("./utils/config/ReferenceLists/LocationGroupTypes.xlsx")
# 
# # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
# renameWorksheet(wb, sheet = "Sheet1", newName = "locationgrouptypes")
# 
# # Save the workbook with the updated sheet name
# saveWorkbook(wb, "./utils/config/ReferenceLists/LocationGroupTypes.xlsx", overwrite = TRUE)
locationgrouptypes <- read_excel("./utils/config/ReferenceLists/LocationGroupTypes.xlsx", 
                             sheet = "locationgrouptypes")

# LOCATION TYPES ---------------------------------------------------------
locationtypes <- read_excel("./utils/config/ReferenceLists/LocationTypes.xlsx", 
                                 sheet = "locationtypes")


# MEDIUMS ---------------------------------------------------------
mediums <- read_excel("./utils/config/ReferenceLists/Mediums.xlsx", 
                            sheet = "Mediums")

# RESULT GRADES ---------------------------------------------------------
##Had to run this only once in a lifetime
# resultgrades <- get_profiles("test", "resultgrades") %>%
#   dplyr::select(customId)
#
# # Save workbook
# write_xlsx(list(resultgrades), "./utils/config/ReferenceLists/ResultGrades.xlsx")
# 
# # Load an existing workbook
# wb <- loadWorkbook("./utils/config/ReferenceLists/ResultGrades.xlsx")
# 
# # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
# renameWorksheet(wb, sheet = "Sheet1", newName = "resultgrades")
# 
# # Save the workbook with the updated sheet name
# saveWorkbook(wb, "./utils/config/ReferenceLists/ResultGrades.xlsx", overwrite = TRUE)
resultgrades <- read_excel("./utils/config/ReferenceLists/ResultGrades.xlsx", 
                                 sheet = "resultgrades")


# RESULT STATUSES ---------------------------------------------------------
# #Had to run this only once in a lifetime
# resultstatuses <- get_profiles("test", "resultstatuses") %>%
#   dplyr::select(customId)
# 
# # Save workbook
# write_xlsx(list(resultstatuses), "./utils/config/ReferenceLists/ResultStatuses.xlsx")
# 
# # Load an existing workbook
# wb <- loadWorkbook("./utils/config/ReferenceLists/ResultStatuses.xlsx")
# 
# # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
# renameWorksheet(wb, sheet = "Sheet1", newName = "resultstatuses")
# 
# # Save the workbook with the updated sheet name
# saveWorkbook(wb, "./utils/config/ReferenceLists/ResultStatuses.xlsx", overwrite = TRUE)
resultstatuses <- read_excel("./utils/config/ReferenceLists/ResultStatuses.xlsx", 
                           sheet = "resultstatuses")

# FISH TAXONOMY -----------------------------------------------------------
Taxons <- read_excel("./utils/config/ReferenceLists/FishTaxonomy.xlsx", sheet = "Taxonomy")

# COLLECTION METHODS ------------------------------------------------------
collection_methods <- read_excel("./utils/config/ReferenceLists/Collection_methods.xlsx", sheet = "CollectionMethods")

#remove collection methods we no longer want
collection_methods <- collection_methods %>% filter(`New EnMoDS Short Name/ID` != "DELETE")

#select just the needed columns
collection_methods <- collection_methods %>% select(c("New EnMoDS Short Name/ID", "EMS CODE", "Definition"))

#merge ems codes into a long string
collection_methods <- collection_methods %>% 
  group_by(`New EnMoDS Short Name/ID`) %>% 
  reframe(merged_codes = paste(`EMS CODE`, collapse = ", "), Definition)

#remove duplicates
collection_methods<-distinct(collection_methods)

#replace EMS code with blanks where its NA
collection_methods$merged_codes[collection_methods$merged_codes == 'NA'] = ""

# Function based configuration development----------------------------------------------------

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
    
  } else if(data_type == "unitgroups"){
    
    url <- str_c(base_url, "v1/unitgroups")
    
  } else if(data_type == "extendedattributes"){
    
    url <- str_c(base_url, "v1/extendedattributes")
    
  } else if(data_type == "observedproperties"){
    
    url <- str_c(base_url, "v1/observedproperties")
    
  } else if(data_type == "methods"){
    
    url <- str_c(base_url, "v1/analysismethods")
    
  } else if(data_type == "labs"){
    
    url <- str_c(base_url, "v1/laboratories")
    
  } else if(data_type == "locationgrouptypes"){
    
    url <- str_c(base_url, "v1/samplinglocationgrouptypes")
    
  } else if(data_type == "locationtypes"){
    
    url <- str_c(base_url, "v1/samplinglocationtypes")
    
  } else if(data_type == "mediums"){
    
    url <- str_c(base_url, "v1/mediums")
    
  } else if(data_type == "taxonomylevels"){
    
    url <- str_c(base_url, "v1/taxonomylevels")
    
  } else if(data_type == "detectionconditions"){
    
    url <- str_c(base_url, "v1/detectionconditions")
    
  } else if(data_type == "resultgrades"){
    
    url <- str_c(base_url, "v1/resultgrades")
    
  } else if(data_type == "resultstatuses"){
    
    url <- str_c(base_url, "v1/resultstatuses")
    
  } else if(data_type == "fishtaxonomy"){
    
    url <- str_c(base_url, "v1/taxons")
    
  } else if(data_type == "collectionmethods"){
    
    url <- str_c(base_url, "v1/collectionmethods")
    
  } else if(data_type == "filters"){
    
    url <- str_c(base_url, "v1/filters")
    
  } else if(data_type == "projects"){
    
    url <- str_c(base_url, "v1/projects")
    
  }
  
  temp_profiles <- get_profiles_for_url(env, url)
  
  return(temp_profiles)
  
}

get_check <- get_profiles("prod", "resultgrades")

get_check <- get_profiles("prod", "resultstatuses")

get_check <- get_profiles("prod", "mediums")

get_check <- get_profiles("test", "projects")

get_check <- get_profiles("prod", "locationtypes")

get_check <- get_profiles("prod", "locationgrouptypes")

get_check <- get_profiles("prod", "collectionmethods")

get_check <- get_profiles("prod", "taxonomylevels")

get_check <- get_profiles("prod", "filters")

get_check <- get_profiles("prod", "fishtaxonomy")

get_check <- get_profiles("prod", "labs")

get_check <- get_profiles("prod", "methods")

get_check <- get_profiles("prod", "extendedattributes")

get_check <- get_profiles("prod", "observedproperties")

get_check <- get_profiles("prod", "units")

del_profiles <- function(env, data_type){
  
  # env <- "prod"
  # 
  # data_type <- "taxonomylevels"#labs"#"observedproperties"

  temp_profile <- get_profiles(env, data_type)
  
  #default is "test" and for prod env, use the function parameter "prod"
  url_parameters <- update_base_url_token(env)
  base_url <- url_parameters[[1]]
  token <- url_parameters[[2]]
  
  if(data_type == "unitgroups"){
    
    del_profiles(env, "units")
    
    url <- str_c(base_url, "v1/unitgroups/")
    
  } else if(data_type == "units"){
    
    del_profiles(env, "observedproperties")
    
    url <- str_c(base_url, "v1/units/")
    
  } else if(data_type == "extendedattributes"){
    
    url <- str_c(base_url, "v1/extendedattributes/")
    
  } else if(data_type == "observedproperties"){
    
    url <- str_c(base_url, "v1/observedproperties/")
    
  } else if(data_type == "methods"){
    
    url <- str_c(base_url, "v1/analysismethods/")
    
  } else if(data_type == "labs"){
    
    url <- str_c(base_url, "v1/laboratories/")
    
  } else if(data_type == "fishtaxonomy"){
    
    url <- str_c(base_url, "v1/taxons/")
    
  } else if(data_type == "collectionmethods"){
    
    url <- str_c(base_url, "v1/collectionmethods/")
    
  } else if(data_type == "filters"){
    
    url <- str_c(base_url, "v1/filters/")
    
  } else if(data_type == "projects"){
    
    url <- str_c(base_url, "v1/projects/")
    
  } else if(data_type == "locationgrouptypes"){
    
    put_profiles("prod", "locationgrouptypes", tibble(customId = character()))
    
    return()
    
  } else if(data_type == "locationtypes"){
    
    put_profiles("prod", "locationtypes", tibble(customId = character()))
    
    return()
    
  } else if(data_type == "mediums"){
    
    mediums_required <- get_profiles("prod", "mediums") %>%
      dplyr::filter(!is.na(systemCode)) %>% 
      dplyr::select(customId)
    
    put_profiles("prod", "mediums", mediums_required)
    
    return()
    
  } else if(data_type == "taxonomylevels"){
    
    put_profiles("prod", "taxonomylevels", tibble(customId = character()))
    
    return()
    
  } else if(data_type == "detectionconditions"){
    
    url <- str_c(base_url, "v1/detectionconditions/")
    
  } else if(data_type == "resultgrades"){
    
    put_profiles("prod", "resultgrades", tibble(customId = character()))
    
    return()
    
  } else if(data_type == "resultstatuses"){
    
    put_profiles("prod", "resultstatuses", tibble(customId = character()))
    
    return()
    
  }
  
  del_ids <- temp_profile$id
  
  #response <- character(length = length(del_ids))
  
  i = 1
  
  for(id in del_ids){
    
    #id <- del_ids[1]
    
    data_body <- list()
    
    url_id <- str_c(url, id)
    
    #Make the unit group
    x<-DELETE(url_id, config = c(add_headers(.headers = c('Authorization' = token))), 
              body = data_body, encode = 'json')
    
    print(i)
    
    #response_check <- fromJSON(rawToChar(x$content))
    
    #response[i] <- if ("message" %in% names(response_check)) response_check$message else ""
    
    i = i + 1
    
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
  
  #return(response)
  return()
  
}

del_check <- del_profiles("prod", "resultgrades")

del_check <- del_profiles("prod", "resultstatuses")

del_check <- del_profiles("prod", "mediums")

del_check <- del_profiles("prod", "projects")

del_check <- del_profiles("prod", "locationtypes")

del_check <- del_profiles("prod", "locationgrouptypes")

del_check <- del_profiles("prod", "fishtaxonomy")

del_check <- del_profiles("prod", "taxonomylevels")

del_check <- del_profiles("prod", "taxonomylevels")

del_check <- del_profiles("prod", "detectionconditions")

del_check <- del_profiles("prod", "filters")

del_check <- del_profiles("prod", "collectionmethods")

del_check <- del_profiles("prod", "labs")

del_check <- del_profiles("prod", "methods")

#OPs use units so have to be deleted first
del_check <- del_profiles("prod", "observedproperties")

del_check <- del_profiles("prod", "units")

del_check <- del_profiles("prod", "unitgroups")

del_check <- del_profiles("prod", "extendedattributes")

put_profiles <- function(env, data_type, profile){
  
  # env <- "prod"
  # 
  # data_type <- "resultgrades"
  # 
  # profile <- resultgrades

  #default is "test" and for prod env, use the function parameter "prod"
  url_parameters <- update_base_url_token(env)
  base_url <- url_parameters[[1]]
  token <- url_parameters[[2]]
  
  if(data_type == "taxonomylevels"){
    
    #update url to include data_type
    url <- str_c(base_url, "v1/taxonomylevels")
    
  } else if(data_type == "locationgrouptypes"){
    
    #update url to include data_type
    url <- str_c(base_url, "v1/samplinglocationgrouptypes")
    
  } else if(data_type == "locationtypes"){
    
    #update url to include data_type
    url <- str_c(base_url, "v1/samplinglocationtypes")
  
  } else if(data_type == "mediums"){
    
    #update url to include data_type
    url <- str_c(base_url, "v1/mediums")
    
  } else if(data_type == "resultgrades"){
    
    #update url to include data_type
    url <- str_c(base_url, "v1/resultgrades")
    
  } else if(data_type == "resultstatuses"){
    
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

#Error code 500 which suggests something is wrong at AQS end; informed Jeremy
#Doing it manually
put_check <- put_profiles("prod", "resultgrades", resultgrades)

#Error code 500 which suggests something is wrong at AQS end; informed Jeremy
put_check <- put_profiles("prod", "resultstatuses", resultstatuses)

put_check <- put_profiles("prod", "taxonomylevels", taxonomylevels)

put_check <- put_profiles("prod", "locationgrouptypes", locationgrouptypes)

put_check <- put_profiles("prod", "locationtypes", locationtypes)

put_check <- put_profiles("prod", "mediums", mediums)

post_profiles <- function(env, data_type, profile){

  # env = "prod"
  # 
  # data_type = "projects"
  # 
  # profile <- projects
  # 
  # profile <- profile %>%
  #               dplyr::filter(ID == "BCLMN")
  # #   dplyr::filter(NewNameID == "Biological Sex (cat.)")
  # # #  dplyr::filter(Sample.Unit.CustomId == "mL")
  # # #   dplyr::filter(NewNameID == "pH (acidity)")

  #Clean the old stuff out of the environment before posting new stuff
  if(!is.null(dim(get_profiles(env, data_type))[1])){
    
      del_profiles(env, data_type)
  
    }

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
    
    rel_var <- c("CONVERSION_FACTOR", "OFFSET", "Convertible",
                 "Sample.Unit.Group", "Sample.Unit.CustomId",
                 "Sample.Unit.Name", "Sample.Unit.GroupID")
    
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
    
  } else if(data_type == "extendedattributes"){
    
    url <- paste0(base_url, "v1/extendedattributes")
    
    rel_var <- c("customId", "dataType",
                 "appliesToType", "description", "dropdownlist")
    
  } else if(data_type == "observedproperties"){
    
    url <- paste0(base_url, "v1/observedproperties")
    
    rel_var <- c("Parm.Code", "NewNameID", "Description", "Analysis.Type",
                 "unit.group.id", "unit.id", "CAS")
    
  } else if(data_type == "methods"){
    
    url <- paste0(base_url, "v1/analysismethods")
    
    rel_var <- c("Method.Code", "Method", "Method.Description", "OPs_list")
    
  } else if(data_type == "labs"){
    
    url <- str_c(base_url, "v1/laboratories")
    
    rel_var <- c("ID", "Name", "Description", "Address", "Point.Of.Contact", 
                 "Email", "Phone.Number")
    
  } else if(data_type == "fishtaxonomy"){
    
    url <- str_c(base_url, "v1/taxons")
    
    taxonomylevels_profiles <- get_profiles("prod", "taxonomylevels")
    
    profile <- profile %>%
      left_join(taxonomylevels_profiles %>% 
                  dplyr::select(id, customId) %>%
                  rename(Taxonomy.Level.ID = id), 
                by = join_by(Level == customId), 
                keep = FALSE)
    
    rel_var <- c("Taxonomy.Level.ID", "Scientific Name", "Common Name", "Source", 
                 "Comments", "ITIS TSN")
    
  } else if(data_type == "collectionmethods"){
    
    url <- str_c(base_url, "v1/collectionmethods")
    
    rel_var <- c("New EnMoDS Short Name/ID", "merged_codes", "Definition")
    
  } else if(data_type == "detectionconditions"){
    
    url <- str_c(base_url, "v1/detectionconditions")
    
    rel_var <- c("customId", "name", "description", "systemCode")
    
  } else if(data_type == "filters"){
    
    url <- str_c(base_url, "v1/filters")
    
    rel_var <- c("customId")
    
  } else if(data_type == "projects"){
    
    url <- str_c(base_url, "v1/projects")
    
    rel_var <- c("ID", "Name", "Type", "StartDate", "EndDate", 
                 "Comments", "Scope")
    
  }
  
  messages <- list()
  
  for(j in 1:dim(profile)[1]){
    
    #j <- 1
    
    temp_profile <- profile %>% 
      keep(names(.) %in% rel_var) %>%
      slice(j) #%>%
      #as.list()
    
    if(data_type == "unitgroups"){
      
      data_body <- list(
        "customId" = temp_profile$Sample.Unit.Group,
        "supportsConversion" = temp_profile$Convertible)
      
    } else if(data_type == "units"){
      
      #loop to put all the units in the group
      
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
      
    } else if(data_type == "extendedattributes"){
      
      data_body <- list(
          "customId" = temp_profile$customId,
          "dataType" = temp_profile$dataType,
          "appliesToType" = temp_profile$appliesToType,
          "description" = temp_profile$description
      )
      
      if(temp_profile$dataType == 'DROP_DOWN_LIST'){
        
        data_body$dropDownListItems <- temp_profile$dropdownlist[[1]]
   
      }
      
    } else if(data_type == "observedproperties"){
      
      data_body <- list(
          "customId" = temp_profile$NewNameID,
          "name" = temp_profile$Parm.Code,
          "description" = temp_profile$Description,
          "resultType" = "NUMERIC",
          "analysisType" = temp_profile$Analysis.Type,
          "unitGroup" = list("id" = temp_profile$unit.group.id),
          "defaultUnit" = list("id" = temp_profile$unit.id),
          "casNumber" = temp_profile$CAS
      )
      
    } else if(data_type == "methods"){
      
      data_body <- list("methodId" = temp_profile$Method.Code,
                        "name" = temp_profile$Method,
                        "description" = temp_profile$Method.Description,
                        "context" = "EMS Migration",
                        "observedProperties" = temp_profile$OPs_list[[1]]
      )
      
    } else if(data_type == "labs"){
      
      data_body <- list("customId" = temp_profile$ID,
                        "name" = temp_profile$Name,
                        "description" = temp_profile$Description,
                        "address" = temp_profile$Address,
                        "pointOfContact" = temp_profile$Point.Of.Contact,
                        "emailAddress" = temp_profile$Email,
                        "phoneNumber" = temp_profile$Phone.Number)
      
    } else if(data_type == "fishtaxonomy"){
      
      data_body <- list("scientificName" = temp_profile$`Scientific Name`,
                        "commonName" = temp_profile$`Common Name`,
                        "TaxonomyLevel" = list("id" = temp_profile$Taxonomy.Level.ID),
                        "source" = temp_profile$Source,
                        "comment" = temp_profile$Comments,
                        "itisTsn" = temp_profile$`ITIS TSN`,
                        "itisURL" = "www.google.ca")
      
    } else if(data_type == "collectionmethods"){
      
      data_body <- list("customId" = temp_profile$`New EnMoDS Short Name/ID`,
                          "identifierOrganization" = temp_profile$merged_codes,
                          "name" = temp_profile$Definition)
      
    } else if(data_type == "detectionconditions"){
      
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
      
    }
    
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
  
  return(messages)
  
}

post_profiles("prod", "collectionmethods", collection_methods)

post_profiles("prod", "fishtaxonomy", Taxons)

post_profiles("prod", "unitgroups", unit_groups)

post_profiles("prod", "units", units)

post_check <- post_profiles("prod", "observedproperties", OPs)

post_check <- post_profiles("prod", "extendedattributes", extendedAttributes)

post_check <- post_profiles("prod", "detectionconditions", detectionConditions)

post_check <- post_profiles("prod", "filters", savedFilters)

post_check <- post_profiles("prod", "projects", projects)

post_check <- post_profiles("prod", "methods", Methods)

post_check <- post_profiles("prod", "labs", Labs)

# Categorical values issue ------------------------------------------------

#Issue with categorical values in OPs
# get_categorical_values_sample <- function(env, data_type){
#   
#   #env <- "test"
#   
#   #default is "test" and for prod env, use the function parameter "prod"
#   url_parameters <- update_base_url_token(env)
#   base_url <- url_parameters[[1]]
#   token <- url_parameters[[2]]
#   
#   #data_type <- "unitgroups"
#   
#   temp_ids <- get_profiles(env, data_type)$id %>% unlist()
#   
#   profiles <- tibble()
#   
#   for(id in temp_ids){
#     
#     if(data_type == "observedproperties"){
#       
#       url <- str_c(base_url, "v1/observedproperties/", id, "/categoricalvalues")
#       
#     }
#     
#     temp_profiles <- get_profiles_for_url(env, url)
#     
#     profiles <- bind_rows(profiles, temp_profiles)
#     
#   }
#   
#   return(profiles)
#   
# }
# 
# categoricalIds <- get_categorical_values_sample("prod", "observedproperties")
# 
# env <- "prod"
# 
# data_type <- "categoricalvalues"
# 
# #default is "test" and for prod env, use the function parameter "prod"
# url_parameters <- update_base_url_token("prod")
# base_url <- url_parameters[[1]]
# token <- url_parameters[[2]]
# 
# data_body <- list()
# 
# if(data_type == "categoricalvalues"){
#   
#   url <- str_c(base_url, "v1/observedproperties/", get_check$id[1], "/categoricalvalues/")#, categoricalIds$id[1]
#   
# } 
# 
# #Make the unit group
# x<-DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), 
#           body = data_body, encode = 'json')
# 
# response_check <- fromJSON(rawToChar(x$content))$message
# 
# 
# del_categorical_values_sample <- function(env, data_type){
#   
#   #env <- "test"
#   
#   #default is "test" and for prod env, use the function parameter "prod"
#   url_parameters <- update_base_url_token(env)
#   base_url <- url_parameters[[1]]
#   token <- url_parameters[[2]]
#   
#   #data_type <- "unitgroups"
#   
#   temp_data <- get_profiles(env, data_type)$id[1] %>% unlist()
#   
#   if(data_type == "observedproperties"){
#     
#     url <- str_c(base_url, "v1/observedproperties/", temp_data, "/categoricalvalues")
#     
#   }
#   
#   temp_profiles <- get_profiles_for_url(env, url)
#   
#   return(temp_profiles)
#   
# }


