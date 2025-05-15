library(httr)
library(jsonlite)
library(tidyverse)
library(purrr)
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
library(hunspell)

#get the API tokens from your environment file
readRenviron(paste0(getwd(), "./.Renviron"))
testToken <- Sys.getenv("TEST_TOKEN")
prodToken <- Sys.getenv("PROD_TOKEN")
testURL <- Sys.getenv("TEST_URL")
prodURL <- Sys.getenv("PROD_URL")

#function to update URL/token based on chosen environment
update_baseURL_token <- function(env){
  
  if(env == "prod"){
    
    baseURL <- prodURL
    token <- prodToken
    
  } else {
    
    baseURL <- testURL
    token <- testToken
    
  }
  
  urlParameters <- list(baseURL, token)
  
  return(urlParameters)
  
}

#by default global variables always reference "test" environment
urlParameters <- update_baseURL_token("test")
baseURL <- urlParameters[[1]]
token <- urlParameters[[2]]

### PREPROCESSING REFERENCE SHEETS ### ----

# [SB OK] UNITS and UNIT GROUPS ----
# [SB OK] PREPROCESSING TO CONSOLIDATE OLDER UNITS FILES--------
# units <- read_csv("./utils/config/ReferenceLists/Units_ems_jk_2025_04_16.csv") %>% mutate(MEAS_UNIT_CD = as.character(MEAS_UNIT_CD)) %>%
#   mutate(CODE = as.character(CODE)) %>%
#   dplyr::select(-CONVERSION_FACTOR) %>%
#   mutate(CODE = str_replace(CODE, "^0+", ""))
# #Only conversions in this file are reliable
# units_base <- read_excel("./utils/config/ReferenceLists/Units.xlsx", sheet = "Units") %>% mutate(CODE = str_replace(CODE, "^0+", ""))
# #%>% dplyr::select(c(CODE, CONVERSION_FACTOR, OFFSET, Sample.Unit.Group, Sample.Unit.CustomId, Sample.Unit.Name))
# 
# # Identify the new columns from units_base
# new_cols <- setdiff(names(units_base), names(units))
# 
# # Create a tibble of just those new columns
# new_data <- units_base %>%
#   dplyr::select(all_of(new_cols)) %>%
#   mutate(across(everything(), ~ NA)) %>%
#   unique() %>% slice(rep(1, nrow(units)))
# 
# units <- units %>% bind_cols(new_data)
# 
# units <- units %>% bind_rows(units_base %>% mutate(Results = NA)) %>% unique()
# 
# # Columns to move
# cols_to_move <- c("CODE", "Sample.Unit.CustomId", "Sample.Unit.Name",
#                   "Sample.Unit.Short.Name", "Sample.Unit.Group",
#                   "Sample.Unit.Modifier", "Results", "Base Unit",
#                   "OFFSET", "Convertible")
# 
# # Reorder with selected columns at the end
# units <- units %>%
#   dplyr::select(all_of(cols_to_move), everything()) %>%
#   group_by(CODE) %>%
#   summarize(across(c(Sample.Unit.CustomId, Sample.Unit.Name:CONVERSION_FACTOR), ~ (if (all(is.na(.x))) NA else .x[!is.na(.x)][1])), .groups = "drop") %>%
#   mutate(Sample.Unit.Group = case_when(
#     SHORT_NAME == "‰" ~ "DimensionlessRatio",
#     SHORT_NAME == "mg/dscm" ~ "AirConcentration",
#     SHORT_NAME == "% (Recovery)" ~ "DimensionlessRatio",
#     SHORT_NAME == "N/A" ~ "None",
#     .default = Sample.Unit.Group
#   )) %>% mutate(Sample.Unit.CustomId = case_when(
#     SHORT_NAME == "N/A" ~ "Unknown",
#     SHORT_NAME == "‰" ~ "‰",
#     SHORT_NAME == "mg/dscm" ~ "mg/dscm",
#     .default = Sample.Unit.CustomId
#   )) %>% mutate(Sample.Unit.Name = case_when(
#     SHORT_NAME == "N/A" ~ "Unknown",
#     SHORT_NAME == "‰" ~ "Per mille (0/00 VSMOW, isotope composition)",
#     SHORT_NAME == "mg/dscm" ~ "Milligrams per dry standard cubic metre",
#     .default = Sample.Unit.Name
#   )) %>%
#   mutate(CONVERSION_FACTOR = if_else(SHORT_NAME == "mg/dscm", 1000, CONVERSION_FACTOR))
#
# units_new_to_enmods <- read_excel("./utils/config/ReferenceLists/Units_new_to_enmods.xlsx", sheet = "NewUnits")
#
# # Identify the new columns from units_base
# new_cols <- setdiff(names(units), names(units_new_to_enmods))
# 
# # Create a tibble of just those new columns
# new_data <- units %>%
#   dplyr::select(all_of(new_cols)) %>%
#   mutate(across(everything(), ~ NA)) %>%
#   unique() %>% slice(rep(1, nrow(units_new_to_enmods)))
# 
# units_new_to_enmods <- units_new_to_enmods %>%
#   bind_cols(new_data) %>%
#   mutate(Sample.Unit.Modifier = as.character(Sample.Unit.Modifier))
# 
# units <- units %>%
#   bind_rows(units_new_to_enmods) %>%
#   unique()
# #
# # Replacement dictionary as a named vector
# replacements <- c("Ph units" = "pH units",
#                   "hg" = "Hg",
#                   "hectar" = "hectare",
#                   "Count" = "Counts",
#                   "Us gallons" = "US gallons",
#                   "Tons" = "US tons",
#                   "Kilopascal" = "Kilopascals",
#                   "Micro grams per kilogram" = "Micrograms per kilogram",
#                   "Microequivelents" = "Microequivalents",
#                   "Milliequivalent" = "Milliequivalents",
#                   "meter" = "metre",
#                   "Day" = "Days",
#                   "Micromole per gram" = "Micromoles per gram",
#                   "Cenitmetre" = "Centimetres",
#                   "Milisiemens per centimetre" = "Millisiemens per centimetre")
# 
# #Sentence case in general for sample unit names
# #Accounting for special cases
# units <- units %>%
#   mutate(Sample.Unit.Name = str_to_sentence(Sample.Unit.Name)) %>%
#   mutate(Sample.Unit.Name =
#            str_replace_all(Sample.Unit.Name, replacements)) %>%
#   mutate(Sample.Unit.Name = case_when(
#     Sample.Unit.Name == "Centimetre" ~ "Centimetres",
#     Sample.Unit.Name == "Micrometre" ~ "Micrometres",
#     .default = Sample.Unit.Name)) %>%
#   mutate(DESCRIPTION = str_replace_all(DESCRIPTION, "Tons", "US tons"))
# 
# #need to insert unit groups now
# unitGroups <- units %>%
#   dplyr::select(Sample.Unit.Group, Convertible) %>%
#   #group_by(across(everything())) %>%
#   #summarize(Count = n()) %>%
#   #ungroup() %>%
#   mutate(Sample.Unit.Group = case_when(
#     Sample.Unit.Group == "Length" ~ "SYS-REQUIRED - Length",
#     .default = Sample.Unit.Group
#   )) %>% dplyr::filter(!is.na(Convertible)) %>%
#   unique()
# 
# post_check <- post_profiles("prod", "unitgroups", unitGroups)
# 
# unitGroupsProfiles <- get_profiles("prod", "unitgroups")
# 
# units <- units %>% left_join(unitGroupsProfiles %>%
#            dplyr::select(id, customId, supportsConversion),
#            by = join_by("Sample.Unit.Group" == "customId")) %>%
#           dplyr::select(-Convertible) %>%
#           rename(Convertible = supportsConversion,
#                  Sample.Unit.GroupID = id)
# 
# #fixing the units file for anomalous Count group associated with No/m2
# units <- units %>%
#   #mutate(Convertible = ifelse(Sample.Unit.Name == "Number per square metre",
#   #                            FALSE, Convertible)) %>%
#   mutate(OFFSET = if_else(is.na(OFFSET), 0, OFFSET)) %>%
#   mutate(Sample.Unit.Group = case_when(
#     Sample.Unit.Group == "Length" ~ "SYS-REQUIRED - Length",
#     Sample.Unit.Group == "Apperance" ~ "Appearance",
#     .default = Sample.Unit.Group
#   ))
# #
# units <- units %>% mutate(Convertible = if_else(is.na(Convertible), FALSE, Convertible)) %>% unique() %>% mutate(Convertible = if_else(Sample.Unit.Group == "SYS-REQUIRED - Length", TRUE, Convertible))
# 
# #Spell check things before writing
# units_spellcheck <- units %>%
#   mutate(
#     words = str_extract_all(DESCRIPTION, "[A-Z][a-z]+"),  # splits CamelCase into words
#     # words = strsplit(Sample.Unit.Name, "\\s+"),  # split text into words
#     misspelled = map(words, hunspell)
#   )
# # # print(units_spellcheck %>% select(misspelled) %>% unnest(misspelled) %>% unlist() %>% unique(), n = 126)
# # #
# 
# write_xlsx(units, "./utils/config/ReferenceLists/Consolidated_units.xlsx")
# # Load an existing workbook
# wb <- loadWorkbook("./utils/config/ReferenceLists/Consolidated_units.xlsx")
# # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
# renameWorksheet(wb, sheet = "Sheet1", newName = "Units")
# # Save the workbook with the updated sheet name
# saveWorkbook(wb, "./utils/config/ReferenceLists/Consolidated_units.xlsx", overwrite = TRUE)

# [SB OK] PREPROCESSING UNITS FOR NEW DATA ---------------------------------------------

#Only conversions in this file are reliable
units_base <- read_excel("./utils/config/ReferenceLists/consolidatedUnits.xlsx", sheet = "Units") %>% 
  mutate(CODE = str_replace(CODE, "^0+", ""))

#ADD NEW FILE HERE; IF NO NEW FILE, CONSOLIDATED BASE UNITS FILE WILL BE USED
if(file.exists("./utils/config/ReferenceLists/Units_ems_jk_2025_04_16.csv")){

      units <- read_csv("./utils/config/ReferenceLists/Units_ems_jk_2025_04_16.csv") %>% 
      mutate(MEAS_UNIT_CD = as.character(MEAS_UNIT_CD)) %>%
      mutate(CODE = as.character(CODE)) %>%
      dplyr::select(-CONVERSION_FACTOR) %>%
      mutate(CODE = str_replace(CODE, "^0+", ""))
      
      # Identify the new columns from units_base
      new_cols <- setdiff(names(units_base), names(units))
      
      # Create a tibble of just those new columns
      new_data <- units_base %>%
        dplyr::select(all_of(new_cols)) %>%
        mutate(across(everything(), ~ NA)) %>%
        unique() %>% slice(rep(1, nrow(units)))
      
      units <- units %>% bind_cols(new_data)
      
      units <- units %>% bind_rows(units_base %>% mutate(Results = NA)) %>% unique()
      
} else {
      
      units <- units_base

}

# Columns to move
cols_to_move <- c("Sample.Unit.CustomId", "CODE", "Sample.Unit.Name",
                  "Sample.Unit.Short.Name", "Sample.Unit.Group",
                  "Sample.Unit.Modifier", "Results", "Base Unit",
                  "OFFSET", "Convertible")

# Replacement dictionary as a named vector
replacements <- c("Ph units" = "pH units",
                  "hg" = "Hg",
                  #"hectar" = "hectare",
                  #"Count" = "Counts",
                  "Us gallons" = "US gallons",
                  #"Tons" = "US tons",
                  #"Kilopascal" = "Kilopascals",
                  "Micro grams per kilogram" = "Micrograms per kilogram",
                  "Microequivelents" = "Microequivalents",
                  #"Milliequivalent" = "Milliequivalents",
                  "meter" = "metre",
                  #"Day" = "Days",
                  "Micromole per gram" = "Micromoles per gram",
                  "Cenitmetre" = "Centimetres",
                  "Milisiemens per centimetre" = "Millisiemens per centimetre")

# Count the number of units with NA in code; these are units not in EMS
units_missing_code <- units %>% 
                        dplyr::filter(is.na(CODE)) %>%
                        count() %>% unlist()

#allocating random CODE values to those rows so they do not get removed
set.seed(123)  # optional, for reproducibility
random_code_num <- sample(-1:-20, units_missing_code)

units$CODE[which(is.na(units$CODE))] <- random_code_num

# Reorder with selected columns at the end
units <- units %>%
  dplyr::select(all_of(cols_to_move), everything()) %>%
  group_by(CODE) %>%
  summarize(across(c(Sample.Unit.CustomId, Sample.Unit.Name:Sample.Unit.GroupID), ~ (if (all(is.na(.x))) NA else .x[!is.na(.x)][1])), 
            .groups = "drop") %>% 
  mutate(Sample.Unit.Modifier = as.character(Sample.Unit.Modifier)) %>% 
  mutate(Convertible = if_else(is.na(Convertible), FALSE, Convertible)) %>%
  mutate(OFFSET = if_else(is.na(OFFSET), 0, OFFSET)) %>%
  mutate(Sample.Unit.Name = str_to_sentence(Sample.Unit.Name)) %>%
  mutate(Sample.Unit.Name = 
                      str_replace_all(Sample.Unit.Name, replacements)) %>%
  dplyr::filter(!is.na(Sample.Unit.Name)) %>%
  group_by(Sample.Unit.Name) %>%
  mutate(Results = sum(Results, na.rm = TRUE)) %>% 
  ungroup() %>%
  unique()

write_xlsx(units, "./utils/config/ReferenceLists/consolidatedUnits.xlsx")

# Load an existing workbook
wb <- loadWorkbook("./utils/config/ReferenceLists/consolidatedUnits.xlsx")

# Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
renameWorksheet(wb, sheet = "Sheet1", newName = "Units")

# Save the workbook with the updated sheet name
saveWorkbook(wb, "./utils/config/ReferenceLists/consolidatedUnits.xlsx", overwrite = TRUE)

units <- units %>% 
  dplyr::select(CONVERSION_FACTOR, Sample.Unit.Name, Sample.Unit.CustomId,
                Convertible, CONVERSION_FACTOR, OFFSET, Sample.Unit.Group) %>%
  unique()

# [SB OK] QA/QC for UNITS -------------------------------------------

#first post files
post_check <- post_profiles("prod", "units", units)

# # #109 only; even though 126 in the units file - this is OK because many units
#were merged from EMS for example % ww and % weight and % vv all become %
get_check <- get_profiles("prod", "units")
# #
# # #no such units
#units_na <- units %>% dplyr::filter(is.na(Sample.Unit.CustomId))
# #
units_missing <- units %>%
  anti_join(get_check,
            by = join_by("Sample.Unit.CustomId" == "customId"))

# [SB OK] PREPROCESSING OF UNIT GROUPS --------------------------------------------

#list of unit groups
#sometimes this list may have one less or more unit groups than in ENV
#This is because AQS automatically creates Length if no unit groups present
#This added Unit Group may be called "SYS-REQUIRED - Length"
unitGroups <- units %>% 
  dplyr::select(Sample.Unit.Group, Convertible) %>% 
  group_by(across(everything())) %>%
  summarize(Count = n()) %>% 
  ungroup() %>%
  mutate(Sample.Unit.Group = case_when(
          Sample.Unit.Group == "Length" ~ "SYS-REQUIRED - Length",
          .default = Sample.Unit.Group
  ))

write_xlsx(unitGroups, "./utils/config/ReferenceLists/consolidatedUnitGroups.xlsx")

# Load an existing workbook
wb <- loadWorkbook("./utils/config/ReferenceLists/consolidatedUnitGroups.xlsx")

# Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
renameWorksheet(wb, sheet = "Sheet1", newName = "UnitGroups")

# Save the workbook with the updated sheet name
saveWorkbook(wb, "./utils/config/ReferenceLists/consolidatedUnitGroups.xlsx", overwrite = TRUE)


# [SB OK] EXTENDED ATTRIBUTES ----
# [SB OK] PREPROCESSING EXTENDED ATTRIBUTES FOR NEW DATA --------------------------

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





# [SB OK] DETECTION CONDITIONS ----
# [SB OK] PREPROCESSING TO GENERATE OLDER DETECTION CONDITION FILES ---------------
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

# [SB OK] PREPROCESSING DETECTION CONDITIONS FOR NEW DATA --------------------

detectionConditions <- read_excel("./utils/config/ReferenceLists/DetectionConditions.xlsx", 
                                 sheet = "detectionconditions")





# [SB OK] SAVED FILTERS ----
# [SB OK] PREPROCESSING TO GENERATE OLDER SAVED FILTERS FILES ---------------

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

# [SB OK] PREPROCESSING SAVED FILTERS FOR NEW DATA ---------------------------

savedFilters <- read_excel("./utils/config/ReferenceLists/savedfilters.xlsx", 
                                  sheet = "savedfilters")




# [SB OK] PROJECTS ----
# [SB OK] PREPROCESSING PROJECTS FOR NEW DATA ---------------------

##had to run only once
#projects_test <- get_profiles("test", "projects")

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
                       




# [SB OK] SAMPLING LOCATION GROUPS AND LOCATIONS-------------------------------------------
# [SB OK] PREPROCESSING SAMPLING LOCATIONS AND GROUPS FOR NEW DATA --------------------
non_zero_post_2006 <- read_excel("./utils/config/ReferenceLists/samplingLocations/March5_2025NonzeroSamplesAfter2006Export.xlsx")
zero_pre_2006_auth <- read_excel("./utils/config/ReferenceLists/samplingLocations/March5_2025_ZeroSamplesBefore2006ActiveSuspended.xlsx")
zero_2006_2024_can <- read_excel("./utils/config/ReferenceLists/samplingLocations/March5_2025_Between2006And2024ZeroSamples.xlsx")

#Check for identical column names
if(identical(names(zero_pre_2006_auth), names(non_zero_post_2006))){
  
  if(identical(names(zero_2006_2024_can), names(non_zero_post_2006))){
    
    print("The column names are SAME across the location files")
    
  } else {
    
    print("The column names are DIFFERENT across the location files")
    
  }
    
} else {
  
  print("The column names are DIFFERENT across the location files")
  
}

#join together the ones with actual data that needs to be included
locations <- rbind(non_zero_post_2006, zero_pre_2006_auth)

#remove those locations that were cancelled and never sampled
locations <- locations %>% 
                anti_join(zero_2006_2024_can, by = join_by("Location ID"))

#remove do not migrate
locations <- locations %>% dplyr::filter(Type != "DO NOT MIGRATE")

#remove all NA from data frame!
locations <- locations %>% 
  mutate(across(where(is.character), ~ replace(., is.na(.), "")),
         across(where(is.numeric), ~ replace(., is.na(.), "")),
         across(where(is.logical), ~ replace(., is.na(.), "")),)

#Get location groups
#read the AMS data
ams_url <- "https://www2.gov.bc.ca/assets/gov/environment/waste-management/waste-discharge-authorization/datamart/all_ams_authorizations.xlsx"
download.file(ams_url, "all_ams_authorizations.xlsx", mode = "wb")

amsPermits <- read_excel("all_ams_authorizations.xlsx")

#storing relevant records of AMS Permits
amsPermits <- amsPermits %>% 
  dplyr::select(`Authorization Number`, Company, `Facility Type - Description`, `Facility Address`)

#remove duplicates in AMS
amsPermits <- unique(amsPermits)

#Get the list of permits from locations
emsPermits <- unique(locations$`Location Groups`)
emsPermits <- emsPermits[emsPermits != ""]

emsPermits <- data.frame("Permit ID" = unlist(strsplit(emsPermits, ";"))) %>% unique()
colnames(emsPermits) <- "Permit ID"
emsPermits$`Permit ID` <- as.numeric(emsPermits$`Permit ID`)
emsPermits <- emsPermits %>% unique()

#join EMS with AMS dropping records that are only in the AMS side
locationGroups <- left_join(emsPermits, amsPermits, 
                            join_by(`Permit ID` == `Authorization Number`))

locationGroups$Description = paste0("", locationGroups$Company)

locationGroups$Type = "Authorization"

#make the groups
locationGroups <- locationGroups %>% dplyr::select(`Permit ID`, Type, Description)

locationGroupTypes <- get_profiles("prod", "locationgrouptypes")

#joing location group guid to location groups table
locationGroups <- inner_join(locationGroups, locationGroupTypes, 
                             by = join_by(Type == customId)) %>%
                             rename(locationgrouptypeID = id)

# could not figure out locations completely
# generating split files that have to be loaded manually
# make the files smaller
locations <- locations %>% 
  mutate(`Elevation Unit` = 
           case_when(#is.na(`Elevation Unit`) ~ "metre", 
             `Elevation Unit` == "metre" ~ "m",
             .default = `Elevation Unit`))


#locations have to be pushed manually, 10000 at a time using AQS Import
locations_1 <- locations[seq(1,10000),]
write.csv(locations_1, file = "./utils/config/ReferenceLists/samplingLocations/1_Locations_Extract_May6_2025.csv", row.names = F)

locations_2 <- locations[seq(10001,20000),]
write.csv(locations_2, file = "./utils/config/ReferenceLists/samplingLocations/2_Locations_Extract_May6_2025.csv", row.names = F)

locations_3 <- locations[seq(20001,30000),]
write.csv(locations_3, file = "./utils/config/ReferenceLists/samplingLocations/3_Locations_Extract_May6_2025.csv", row.names = F)

locations_4 <- locations[seq(30001, nrow(locations)),]
write.csv(locations_4, file = "./utils/config/ReferenceLists/samplingLocations/4_Locations_Extract_May6_2025.csv", row.names = F)


# [SB OK] OBSERVED PROPERTIES ----
# [SB OK] PREPROCESSING TO CONSOLIDATE OLDER OP FILES -----------------------------
# #EMS exported observedProperties
# observedProperties <- read_excel("./utils/config/ReferenceLists/Observed_Properties.xlsx")
# 
# #The units don't matter for non-convertable OP units. For example microbial units. So remove them from consideration.
# observedProperties$Sample.Unit[observedProperties$Convertable.In.Samples == "N"] <- ""
# 
# #clean up modifers
# observedProperties$Modifier[is.na(observedProperties$Modifier)] <- ""
# 
# #Add an empty Result.Type column
# observedProperties$Result.Type <- ""
# 
# #Keep the core columns at the front
# cols_to_move <- c("NewNameID", "Parm.Code", "Description",
#                   "Analysis.Type", "Result.Type", "Sample.Unit.Group",
#                   "Sample.Unit","CAS")
# 
# #get the unique list of OP IDs
# observedProperties <- observedProperties %>% unique()
# 
# 
# ## observedProperties new to EnMoDS not in EMS
# OPs_new_to_EnMoDS <-
#   read_excel("./utils/config/ReferenceLists/New_OPS_not_in_EMS.xlsx",
#              sheet = "OPs_new")
# 
# #get the unique list of OP IDs
# OPs_new_to_EnMoDS <- OPs_new_to_EnMoDS %>% unique()
# 
# #fixing the missing sample group id issue in the list
# OPs_new_to_EnMoDS <- OPs_new_to_EnMoDS %>% mutate(Sample.Unit.Group =
#               if_else(NewNameID == "Biological Sample Volume (vol.)",
#                       "Volume", Sample.Unit.Group))
# 
# #Identify the new columns compared to observedProperties
# new_cols <- setdiff(names(observedProperties), names(OPs_new_to_EnMoDS))
# 
# # Create a tibble of just those new columns
# new_data <- observedProperties %>%
#   dplyr::select(all_of(new_cols)) %>%
#   mutate(across(everything(), ~ NA)) %>%
#   unique() %>% slice(rep(1, nrow(OPs_new_to_EnMoDS)))
# 
# OPs_new_to_EnMoDS <- OPs_new_to_EnMoDS %>% bind_cols(new_data)
# 
# observedProperties <- observedProperties %>% bind_rows(OPs_new_to_EnMoDS %>% mutate(Results = NA)) %>% unique()
# 
# ## Taxonomic observedProperties
# OPs_taxonomic <- read.csv("./utils/config/ReferenceLists/Taxonomic_OP.csv", stringsAsFactors = F) #1607
# 
# #get the unique list of OP IDs
# OPs_taxonomic <- OPs_taxonomic %>% unique()
# 
# #Identify the new columns compared to observedProperties
# new_cols <- setdiff(names(observedProperties), names(OPs_taxonomic))
# 
# # Create a tibble of just those new columns
# new_data <- observedProperties %>%
#   dplyr::select(all_of(new_cols)) %>%
#   mutate(across(everything(), ~ NA)) %>%
#   unique() %>% slice(rep(1, nrow(OPs_taxonomic)))
# 
# OPs_taxonomic <- OPs_taxonomic %>% bind_cols(new_data)
# 
# observedProperties <- observedProperties %>% bind_rows(OPs_taxonomic %>% mutate(Results = NA)) %>% unique()
# 
# ## Merge all observedProperties and process
# 
# observedProperties <- observedProperties %>%
# #bind_rows(observedProperties, OPs_new_to_EnMoDS, OPs_taxonomic) %>%
# #        unique() %>%
#         group_by(NewNameID) %>%
#         mutate(Count = n()) %>%
#         ungroup()
# 
# #the total number of unique NewNameID should be the same as the total number of unique rows
# newNameIDUnique <- observedProperties$NewNameID %>% unique()
# 
# observedProperties <- observedProperties %>%
#   mutate(Sample.Unit = case_when(
#           Sample.Unit == "pH Units" ~ "pH units",
#           #Sample.Unit == NA ~ Unit,
#           #Sample.Unit == "" ~ NA,
#           .default = Sample.Unit),
#          )
# 
# #rename the required observedProperties so they show up otherwise they are in the background?
# observedProperties <- observedProperties %>%
#   mutate(Sample.Unit.Group = case_when(
#     NewNameID == "Biological Sex (cat.)" ~ "None",
#     NewNameID == "Biological Life Stage (cat.)" ~ "None",
#     Sample.Unit.Group == "Length" ~ "SYS-REQUIRED - Length",
#     Sample.Unit.Group == "Apperance"~ "Appearance",
#     .default = Sample.Unit.Group
#   )) %>%
#   mutate(Analysis.Type = case_when(
#     NewNameID == "Biological Sex (cat.)" ~ "BIOLOGICAL",
#     NewNameID == "Biological Life Stage (cat.)" ~ "BIOLOGICAL",
#     .default = Analysis.Type
#   ))
# 
# #need to get unit group and unit IDs prior to importing observedProperties
# unitGroups <- get_profiles("prod", "unitgroups") %>%
#   dplyr::select(id, customId, supportsConversion) %>%
#   rename("Unit.Group.Id" = "id")
# 
# #they are not!
# #current pipeline will upload the first OP into the system
# #But its fine because...
# #they belong to unit groups that are convertible
# #Below code helps you make that check
# OPs_convertible <- observedProperties %>%
#                     dplyr::filter(Count>1) %>%
#                     left_join(unitGroups %>%
#                     dplyr::select(customId, Unit.Group.Id, supportsConversion),
#                       by = join_by("Sample.Unit.Group" == "customId")) %>%
#                     dplyr::filter(supportsConversion == FALSE)
# 
# # OPs_missing_unitgroup <- observedProperties %>%
# #   dplyr::filter(is.na(Unit.Group.Id))
# 
# #add GUID to the list of observedProperties for unit groups
# observedProperties <- left_join(observedProperties, unitGroups,
#                 by = join_by('Sample.Unit.Group' == 'customId'), keep = FALSE)
# 
# #units without groups
# units <- get_profiles("prod", "units") %>%
#    dplyr::select(id, customId) %>%
#    rename("Unit.Id" = "id")
# 
# #add GUID to the list of observedProperties for units
# observedProperties <- left_join(observedProperties, units,
#                 by = join_by('Sample.Unit' == 'customId'), keep = FALSE)
# 
# #analysis.Type must be ALL CAPS
# observedProperties$Analysis.Type <- toupper(observedProperties$Analysis.Type)
# observedProperties$Result.Type <- toupper(observedProperties$Result.Type)
# 
# write_xlsx(observedProperties, "./utils/config/ReferenceLists/consolidatedObservedProperties.xlsx")
# 
# # Load an existing workbook
# wb <- loadWorkbook("./utils/config/ReferenceLists/consolidatedObservedProperties.xlsx")
# 
# # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
# renameWorksheet(wb, sheet = "Sheet1", newName = "ObservedProperties")
# 
# # Save the workbook with the updated sheet name
# saveWorkbook(wb, "./utils/config/ReferenceLists/consolidatedObservedProperties.xlsx", overwrite = TRUE)
# 
# observedProperties <- observedProperties %>% dplyr::select(c("Parm.Code", "NewNameID", "Description", "Analysis.Type", "Result.Type", "Sample.Unit.Group", "Sample.Unit","CAS")) %>% unique()

# [SB OK] PREPROCESSING OP FOR NEW DATA --------------------------------------

observedProperties_new <- read_excel("./utils/config/ReferenceLists/Observed_Properties_jk_2025-04-22.xlsx") 

#The units don't matter for non-convertable OP units. For example microbial units. So remove them from consideration.
observedProperties_new$Sample.Unit[observedProperties_new$Convertable.In.Samples == "N"] <- ""

#clean up modifers
observedProperties_new$Modifier[is.na(observedProperties_new$Modifier)] <- ""

#Add an empty Result.Type column
observedProperties_new$Result.Type <- ""

observedProperties_base <- read_excel("./utils/config/ReferenceLists/consolidatedObservedProperties.xlsx")

#The units don't matter for non-convertable OP units. For example microbial units. So remove them from consideration.
observedProperties_base$Sample.Unit[observedProperties_base$Convertable.In.Samples == "N"] <- ""

#clean up modifers
observedProperties_base$Modifier[is.na(observedProperties_base$Modifier)] <- ""

#Add an empty Result.Type column
observedProperties_base$Result.Type <- ""

#Unit and unit groups may have been updated; remove their IDs
observedProperties_base <- observedProperties_base %>%
                              dplyr::select(-c(Unit.Id, Unit.Group.Id))

#Identify the new columns compared to observedProperties
new_cols <- setdiff(names(observedProperties_base), names(observedProperties_new))

# Create a tibble of just those new columns
new_data <- observedProperties_base %>%
  dplyr::select(all_of(new_cols)) %>%
  mutate(across(everything(), ~ NA)) %>%
  unique() %>% slice(rep(1, nrow(observedProperties_new)))

observedProperties_new <- observedProperties_new %>% bind_cols(new_data)

observedProperties <- observedProperties_base %>% 
  bind_rows(observedProperties_new %>% mutate(Results = NA)) %>% unique()

## Merge all observedProperties
observedProperties <- observedProperties %>%
  #bind_rows(observedProperties, OPs_new_to_EnMoDS, OPs_taxonomic) %>%
  #        unique() %>% 
  group_by(NewNameID) %>% 
  mutate(Count = n()) %>%
  ungroup()

#the total number of unique NewNameID should be the same as the total number of unique rows
newNameIDUnique <- observedProperties$NewNameID %>% unique()

observedProperties <- observedProperties %>% 
  mutate(Sample.Unit = case_when(
    Sample.Unit == "pH Units" ~ "pH units",
    #Sample.Unit == NA ~ Unit,
    #Sample.Unit == "" ~ NA,
    .default = Sample.Unit),
  )

#rename the required observedProperties so they show up otherwise they are in the background?
observedProperties <- observedProperties %>% 
  mutate(Sample.Unit.Group = case_when(
    NewNameID == "Biological Sex (cat.)" ~ "None",
    NewNameID == "Biological Life Stage (cat.)" ~ "None",
    Sample.Unit.Group == "Length" ~ "SYS-REQUIRED - Length",
    Sample.Unit.Group == "Apperance"~ "Appearance",
    .default = Sample.Unit.Group
  )) %>% 
  mutate(Analysis.Type = case_when(
    NewNameID == "Biological Sex (cat.)" ~ "BIOLOGICAL",
    NewNameID == "Biological Life Stage (cat.)" ~ "BIOLOGICAL",
    .default = Analysis.Type
  ))

#need to get unit group and unit IDs prior to importing observedProperties
unitGroups <- get_profiles("prod", "unitgroups") %>% 
  dplyr::select(id, customId, supportsConversion) %>%
  rename("Unit.Group.Id" = "id")

#they are not! 
#current pipeline will upload the first OP into the system
#But its fine because...
#they belong to unit groups that are convertible
#Below code helps you make that check
OPs_convertible <- observedProperties %>% 
  dplyr::filter(Count>1) %>%
  dplyr::select(-supportsConversion) %>%
  left_join(unitGroups %>% 
              dplyr::select(customId, Unit.Group.Id, supportsConversion), 
            by = join_by("Sample.Unit.Group" == "customId")) %>%
  dplyr::filter(supportsConversion == FALSE)

# OPs_missing_unitgroup <- observedProperties %>% 
#   dplyr::filter(is.na(Unit.Group.Id))

#add GUID to the list of observedProperties for unit groups
observedProperties <- left_join(observedProperties, unitGroups, 
                                by = join_by('Sample.Unit.Group' == 'customId'), keep = FALSE)

#units without groups
units <- get_profiles("prod", "units") %>%
  dplyr::select(id, customId) %>%
  rename("Unit.Id" = "id")

#add GUID to the list of observedProperties for units
observedProperties <- observedProperties %>% 
                        left_join(units, 
                                  by = join_by('Sample.Unit' == 'customId'), 
                                  keep = FALSE)

#analysis.Type must be ALL CAPS
observedProperties$Analysis.Type <- toupper(observedProperties$Analysis.Type)
observedProperties$Result.Type <- toupper(observedProperties$Result.Type)

write_xlsx(observedProperties, "./utils/config/ReferenceLists/consolidatedObservedProperties.xlsx")

# Load an existing workbook
wb <- loadWorkbook("./utils/config/ReferenceLists/consolidatedObservedProperties.xlsx")

# Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
renameWorksheet(wb, sheet = "Sheet1", newName = "ObservedProperties")

# Save the workbook with the updated sheet name
saveWorkbook(wb, "./utils/config/ReferenceLists/consolidatedObservedProperties.xlsx", overwrite = TRUE)

#read observed properties
observedProperties <- read_excel("./utils/config/ReferenceLists/consolidatedObservedProperties.xlsx",
                                 sheet = "ObservedProperties")

observedProperties <- observedProperties %>% dplyr::select(c("Parm.Code", "NewNameID", "Description", "Analysis.Type", "Sample.Unit.Group", "Sample.Unit","CAS")) %>% unique()

# [SB OK] QA/QC for OPs -----------------------------------------------------------

# Checking which entries are not getting posted
get_check <- get_profiles("prod", "observedproperties")
#not all observedProperties getting posted
#compare get_check for observedProperties with raw observedProperties - 0 but 4172 in AQS 4285 in data frame
OPs_not_posted <- observedProperties %>% anti_join(get_check,
                                    by = join_by("NewNameID" == "customId"))



# [SB OK] METHODS -----------------------------------------------------------------
# [SB OK] PREPROCESSING METHODS FOR NEW DATA --------------------------------------

methods <- read_excel("./utils/config/ReferenceLists/consolidatedObservedProperties.xlsx")

#The units don't matter for non convertable OP units. For example microbial units. So remove them from consideration.
methods$Sample.Unit[methods$Convertable.In.Samples == "N"] <- ""

#get the new name (customId) and methods 
methods <- methods %>% 
              dplyr::select(c("NewNameID", "Method.Code", "Method", 
                              "Method.Description"))

OPs.ids <- get_profiles("prod", "observedproperties") %>% 
  dplyr::select("id", "customId")

#check that nothing has been dropped here!
methodsProblematic <- anti_join(methods, OPs.ids, by = join_by("NewNameID" == "customId")) 

methods <- left_join(methods, OPs.ids, by = join_by("NewNameID" == "customId"))

OPs.for.methods <- methods %>%
  dplyr::select(id, Method.Code) %>%
  group_by(Method.Code) %>%
  summarise(
    OPs.list = list(
      map(id, ~ list("id" = .x))
    ),
    .groups = "drop"
  )

methods <- unique(methods %>% select(-c("NewNameID", "id")))

methods <- methods %>%
  left_join(OPs.for.methods, by = join_by(Method.Code == Method.Code),
            keep = FALSE)

#Method names cannot contain semicolons
#removing semicolons in names and replacing them with colons
methods <- methods %>% 
  mutate(Method = str_replace_all(Method, ";", ":"))

# [SB OK] QA/QC for METHODS -------------------------------------------

#first post files
post_check <- post_profiles("prod", "methods", methods)

get_check <- get_profiles("prod", "methods")

methods.missing <- methods %>%
  anti_join(get_check,
            by = join_by("Method.Code" == "methodId"))



# [SB OK] LABS --------------------------------------------------------------------
# [SB OK] PREPROCESSING LABS FOR NEW DATA --------------------------------------

labs <- read.csv("./utils/config/ReferenceLists/Labs.csv", stringsAsFactors = F)

labs$Description = str_c("Created by ", labs$WHO_CREATED, " on ", labs$WHEN_CREATED)




# [SB OK] TAXONOMY LEVELS ---------------------------------------------------------
# [SB OK] PREPROCESSING TO GENERATE OLDER TAXONOMY LEVELS FILES ----------------

##Had to run this only once in a lifetime
# taxonomyLevels <- get_profiles("test", "taxonomylevels") %>%
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

# [SB OK] PREPROCESSING TAXONOMY LEVELS FOR NEW DATA ------------------------------

taxonomyLevels <- read_excel("./utils/config/ReferenceLists/TaxonomyLevels.xlsx", 
                             sheet = "taxonomylevels")





# [SB OK] LOCATION GROUP TYPES -----------------------------------------
# [SB OK] PREPROCESSING TO GENERATE OLDER LOCATION GROUP TYPES FILES ------------
##Had to run this only once in a lifetime
# locationgrouptypes <- get_profiles("test", "locationgrouptypes") %>%
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

# [SB OK] PREPROCESSING LOCATION GROUP TYPES FOR NEW DATA -------------------------

locationgrouptypes <- read_excel("./utils/config/ReferenceLists/LocationGroupTypes.xlsx", 
                             sheet = "locationgrouptypes")



# [SB OK] LOCATION TYPES ---------------------------------------------------------
locationTypes <- read_excel("./utils/config/ReferenceLists/LocationTypes.xlsx", 
                                 sheet = "locationtypes")

locationTypes <- locationTypes %>% 
                    mutate(customId = case_when(
                      customId == "Land - Fram" ~ "Land - Farm",
                      .default = customId
                    ))



# [SB OK] MEDIUMS ---------------------------------------------------------
mediums <- read_excel("./utils/config/ReferenceLists/Mediums.xlsx", 
                            sheet = "Mediums")


# Doing it manually; getting error on AQS's end

# [SB OK] RESULT GRADES ---------------------------------------------------------
# [SB OK] PREPROCESSING TO GENERATE OLDER RESULT GRADES FILES --------------

##Had to run this only once in a lifetime
# resultGrades <- get_profiles("test", "resultgrades") %>%
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

# [SB OK] PREPROCESSING RESULT GRADES FOR NEW DATA --------------------------------

resultGrades <- read_excel("./utils/config/ReferenceLists/ResultGrades.xlsx", 
                                 sheet = "resultgrades")



# Doing it manually; getting error on AQS's end


# [SB OK] RESULT STATUSES ---------------------------------------------------------
# [SB OK] PREPROCESSING TO GENERATE OLDER RESULT STATUSES FILES ---------------------

# #Had to run this only once in a lifetime
# resultStatuses <- get_profiles("test", "resultstatuses") %>%
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

# [SB OK] PREPROCESSING RESULT STATUSES FOR NEW DATA ------------------------------

resultStatuses <- read_excel("./utils/config/ReferenceLists/ResultStatuses.xlsx", 
                           sheet = "resultstatuses")




# [SB OK] FISH TAXONOMY -----------------------------------------------------------
# [SB OK] PREPROCESSING FISH TAXONS FOR NEW DATA ----------------------------------

taxons <- read_excel("./utils/config/ReferenceLists/FishTaxonomy.xlsx", sheet = "Taxonomy")




# [SB OK] COLLECTION METHODS ------------------------------------------------------
# [SB OK] PREPROCESSING COLLECTION METHODS FOR NEW DATA ---------------------------

collectionMethods <- read_excel("./utils/config/ReferenceLists/Collection_methods.xlsx", 
                                 sheet = "CollectionMethods")

#remove collection methods we no longer want
collectionMethods <- collectionMethods %>% filter(`New EnMoDS Short Name/ID` != "DELETE")

#select just the needed columns
collectionMethods <- collectionMethods %>% select(c("New EnMoDS Short Name/ID", "EMS CODE", "Definition"))

#merge ems codes into a long string
collectionMethods <- collectionMethods %>% 
  group_by(`New EnMoDS Short Name/ID`) %>% 
  reframe(merged_codes = paste(`EMS CODE`, collapse = ", "), Definition)

#remove duplicates
collectionMethods<-distinct(collectionMethods)

#replace EMS code with blanks where its NA
collectionMethods$merged_codes[collectionMethods$merged_codes == 'NA'] = ""





# Function based configuration development----------------------------------------------------

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

get_check <- get_profiles("prod", "resultgrades")

get_check <- get_profiles("prod", "resultstatuses")

get_check <- get_profiles("prod", "mediums")

get_check <- get_profiles("test", "projects")

get_check <- get_profiles("prod", "locationtypes")

get_check <- get_profiles("prod", "locations")

get_check <- get_profiles("prod", "locationgroups")

get_check <- get_profiles("prod", "locationgrouptypes")

get_check <- get_profiles("prod", "collectionmethods")

get_check <- get_profiles("prod", "taxonomylevels")

get_check <- get_profiles("prod", "filters")

get_check <- get_profiles("prod", "fishtaxonomy")

get_check <- get_profiles("prod", "labs")

get_check <- get_profiles("prod", "methods")

get_check <- get_profiles("prod", "extendedattributes")

get_check <- get_profiles("prod", "observedproperties")

get_check <- get_profiles("prod", "unitgroups")

get_check <- get_profiles("prod", "units")

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

#not working
del_check <- del_profiles("prod", "resultgrades")

#not working
del_check <- del_profiles("prod", "resultstatuses")

#not working because they are required
del_check <- del_profiles("prod", "mediums")

del_check <- del_profiles("prod", "projects")

del_check <- del_profiles("prod", "locations")

del_check <- del_profiles("prod", "locationgroups")

del_check <- del_profiles("prod", "locationtypes")

del_check <- del_profiles("prod", "locationgrouptypes")

del_check <- del_profiles("prod", "fishtaxonomy")

del_check <- del_profiles("prod", "taxonomylevels")

#not working
del_check <- del_profiles("prod", "detectionconditions")

del_check <- del_profiles("prod", "filters")

del_check <- del_profiles("prod", "collectionmethods")

del_check <- del_profiles("prod", "labs")

del_check <- del_profiles("prod", "methods")

#observedProperties use units so have to be deleted first
del_check <- del_profiles("prod", "observedproperties")

del_check <- del_profiles("prod", "units")

del_check <- del_profiles("prod", "unitgroups")

del_check <- del_profiles("prod", "extendedattributes")

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

#Error code 500 which suggests something is wrong at AQS end; informed Jeremy
#Now getting a new error as well as follows:
#Error occurred at repository: PSQLException: ERROR: cannot update system code or delete result grade with system code\n  Where: PL/pgSQL function raise_exception_for_result_grade_systemcode_update() line 3 at RAISE
#Doing it manually
put_check <- put_profiles("prod", "resultgrades", resultGrades)

#Error code 500 which suggests something is wrong at AQS end; informed Jeremy
#Also getting a new error
#"org.postgresql.util.PSQLException: ERROR: cannot update system code or delete mediums with system code\n  Where: PL/pgSQL function raise_exception_for_systemcode_update() line 3 at RAISE"
put_check <- put_profiles("prod", "resultstatuses", resultStatuses)

put_check <- put_profiles("prod", "taxonomylevels", taxonomyLevels)

put_check <- put_profiles("prod", "locationgrouptypes", locationGroupTypes)

put_check <- put_profiles("prod", "locationtypes", locationTypes)

put_check <- put_profiles("prod", "mediums", mediums)

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

post_check <- post_profiles("prod", "collectionmethods", collectionMethods)

post_check <- post_profiles("prod", "fishtaxonomy", taxons) 

post_check <- post_profiles("prod", "unitgroups", unitGroups)

post_check <- post_profiles("prod", "units", units)

post_check <- post_profiles("prod", "observedproperties", observedProperties)

post_check <- post_profiles("prod", "extendedattributes", extendedAttributes)

post_check <- post_profiles("prod", "detectionconditions", detectionConditions)

post_check <- post_profiles("prod", "filters", savedFilters)

post_check <- post_profiles("prod", "projects", projects)

post_check <- post_profiles("prod", "locationgroups", locationGroups)

post_check <- post_profiles("prod", "methods", methods)

post_check <- post_profiles("prod", "labs", labs)
