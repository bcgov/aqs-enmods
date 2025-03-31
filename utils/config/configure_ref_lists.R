#Created August 23 2024
#
#When configuring a new environment in AQS it must be done in the correct order
#
# 1) Populate Lists 
# 2) Units
# 3)
#Update on March 31 2025 by J-Krogh

library(httr)
library(jsonlite)
library(dplyr)
library(writexl)
library(readxl)

#Load tokens and urls
readRenviron(paste0(getwd(), "./.Renviron"))
token <- Sys.getenv("api_test_token")
base_url = Sys.getenv("url_test")

### LISTS ### ----

#Configuration of 'Lists' under settings
#Because lists do not have a POST API we have to add them manually but we in
#in theory edit them but I can't get it to work.

#Extract locaiton types with guid from the current environment
url <- paste0(base_url, "v1/samplinglocationtypes")
data_body <- list()
x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
location_types_extract = fromJSON(rawToChar(x$content))$domainObjects %>% select(c("id", "customId"))

#Update Location Types
update_location_types <- read_excel("./utils/config/ReferenceLists/Location_Types.xlsx")

url <- "https://bcenv-training.aqsamples.com/api/v1/samplinglocationtypes"
data_body <- list(id = "c89b98b5-ca42-4a68-a1d8-cadb03cd1202",
                  customId = "Test Type")

PUT(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#Try mediums
#Extract mediums
url <- paste0(base_url, "v1/mediums")
data_body <- list()
x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
mediums = fromJSON(rawToChar(x$content))$domainObjects %>% select(c("id", "customId", "systemCode"))
#write_xlsx(mediums, "./Lists/Extract_Mediums.xlsx")

#Update Mediums
mediums <- read_excel("./Lists/EMS_LOCN_STATE_DESCRIPTOR_EXPORT_MAP_July24.xlsx", sheet = 1) %>% 
  select('EnMoDS_Medium') %>% unique()
mediums <- mediums %>% filter(EnMoDS_Medium != "DO NOT MIGRATE")

data_body <- list("id" = "9c763e27-cf60-49af-bbf4-0b02145d2cf9",
                  "customId" = "test name")

#hmmm this isn't working
PUT(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

###

### UNITS ### ----

units <- read_excel("./utils/config/ReferenceLists/Units.xlsx", sheet = 3)

#we know there are duplicates in this data set
units <- units %>% select(c(CONVERSION_FACTOR,Convertible,
                            Sample.Unit.Group, Samples.Unit.CustomId,
                            Samples.Unit.Name))
units <- distinct(units)

#list of unit groups
unit_groups <- unique(units$Sample.Unit.Group)

#loop to make all the unit groups
for (i in seq(1, length(unit_groups))) {
  
  #get all the units for the i-th group
  unit_group_units <- units %>% filter(Sample.Unit.Group == unit_groups[i])

  url <- paste0(base_url, "v1/unitgroups")
  data_body <- list("customId" = unit_groups[i],
                    "supportsConversion" = unit_group_units$Convertible[1])

  #Make the unit group
  x<-POST(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  
  #get the unit group's id
  unit_group_id <- fromJSON(rawToChar(x$content))$id
 
  #if the unit group already exist get the id
  if (is.null(unit_group_id)) {
    
    url = paste0(base_url, 'v1/unitgroups?customId=', unit_groups[i])
    data_body <- list()
    x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
    unit_group_id <- fromJSON(rawToChar(x$content))$domainObjects$id
  }
  
  #loop to put all the units in the group
  for (j in seq(1,nrow(unit_group_units))) {
    
    #If the unit group supports conversion provide conversion factors
    if (unit_group_units$Convertible[1] == TRUE) {
      url <- paste0(base_url, "v1/units")
      data_body <- list("customId" = unit_group_units$Samples.Unit.CustomId[j],
                  "name" = unit_group_units$Samples.Unit.Name[j],
                  "baseMultiplier" = 1/unit_group_units$CONVERSION_FACTOR[j],
                  "baseOffset" = 0,
                  "unitGroup" = list("id" = unit_group_id))
      POST(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
    } #else if the group does not support converion do not provide conversion factors
      else {
      url <- paste0(base_url, "v1/units")
      data_body <- list("customId" = unit_group_units$Samples.Unit.CustomId[j],
                        "name" = unit_group_units$Samples.Unit.Name[j],
                        "unitGroup" = list("id" = unit_group_id))
      POST(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
    }
  }
  
}



#Get EA from test

url <- paste0(base_url, 'v1/extendedattributes')
data_body <- list()
x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

EA <- fromJSON(rawToChar(x$content))$domainObjects

#get drop down values
EA_drop_down <- EA %>% filter(dataType == "DROP_DOWN_LIST")


url <- paste0(base_url,'v1/extendedattributes/',EA_drop_down$id[i],'/dropdownlistitems')
x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

drop_down_list <- fromJSON(rawToChar(x$content))$domainObjects

#Carmen figured this out in Python


#Observed Properties------------------------------------------------------------
OPs <- read_excel("./utils/config/ReferenceLists/Observed_Properties.xlsx")

#The units don't matter for non-convertable OP units. For example microbial units. So remove them from consideration.
OPs$Sample.Unit[OPs$Convertable.In.Samples == "N"] <- ""

#clean up modifers
OPs$Modifier[is.na(OPs$Modifier)] <- ""

#test merging the EMS parm code with unit group to make 'new codes'that can be safely used for upload
OPs$NewCode <- paste0(OPs$Parm.Code, "-", OPs$Sample.Unit.Group, OPs$Modifier)

#get the unique list of OP IDs
OPs_unique <- OPs %>% select(c("Parm.Code", "NewNameID", "Description", "Analysis.Type", "Sample.Unit.Group", "Sample.Unit","CAS", "NewCode")) 
OPs_unique <- distinct(OPs_unique) #the total number of unique NewNameID should be the same as the total number of unique rows. 2433 Nov 18 adding new code good



#find the ones that are duplicated
  #unique_ids <- unique(OPs$NewNameID)
  #x<-OPs_unique %>% group_by(NewNameID) %>% summarize(count = n())

  #Some OPs have multiple units based on their previous methods. But as long as the units are the same
  #group and are convertable this should not cause any problems. Only the first one will load into the system and thats fine.


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

#add GUID to the list of OPs for unit groups
OPs_unique <- merge(OPs_unique, unit_grps, by.x = 'Sample.Unit.Group', 'customId')
OPs_unique <- OPs_unique %>% rename("unit.group.id" = "id")

#add GUID to the list of OPs for units
OPs_unique <- merge(OPs_unique, units, by.x = 'Sample.Unit', 'customId', all.x = T)
OPs_unique <- OPs_unique %>% rename("unit.id" = "id")

#analysType must be ALL CAPS
OPs_unique$Analysis.Type <- toupper(OPs_unique$Analysis.Type)

#
url <- paste0(base_url, "v1/observedproperties")


for (j in seq(1, nrow(OPs_unique))) {
  data_body <- list("customId" = OPs_unique$NewNameID[j],
                    "name" = OPs_unique$Parm.Code[j],
                    "description" = OPs_unique$Description[j],
                    "resultType" = "NUMERIC",
                    "analysisType" = OPs_unique$Analysis.Type[j],
                    "unitGroup" = list("id" = OPs_unique$unit.group.id[j]),
                    "defaultUnit" = list("id" = OPs_unique$unit.id[j]),
                    "casNumber" = OPs_unique$CAS[j])
  
  x<-POST(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  
  #print(j)
  
  if (x$status_code != 200) {
    print(x$status_code)
    print(OPs_unique$NewNameID[j])
  }
  
}

#Check why they aren't all there
#so what happened was that some parameters have a single unit group but multiple default units.
#none of the ones this happened to matter so I think it's fine. like mercury mg/L and ng/L which are convertible.

#NEW OPs not in EMS -------------
new_OP_not_in_EMS <- read_excel("./utils/config/ReferenceLists/New_OPS_not_in_EMS.xlsx") #11

#get the unique list of OP IDs
OPs_unique <- new_OP_not_in_EMS %>% select(c("Parm.Code", "NewNameID", "Description", "Analysis.Type", "Result.Type", "Sample.Unit.Group", "Sample.Unit","CAS"))
OPs_unique <- distinct(OPs_unique)

#Unit group is null when the OP is cat but this causes issues with the merge
#OPs_unique$Sample.Unit.Group[is.na(OPs_unique$Sample.Unit.Group)] <- "" 

#need to get unit IDs prior to importing OPs
url = paste0(base_url, 'v1/unitgroups')
data_body <- list()
x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
unit_grps <- fromJSON(rawToChar(x$content))$domainObjects %>% select("id", "customId")

#units without groups
url = paste0(base_url,'v1/units')
data_body <- list()
x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
units <- fromJSON(rawToChar(x$content))$domainObjects %>% select("id", "customId")

#add GUID to the list of OPs for unit groups
OPs_unique <- merge(OPs_unique, unit_grps, by.x = 'Sample.Unit.Group', 'customId', all.x = T)
OPs_unique <- OPs_unique %>% rename("unit.group.id" = "id")

#add GUID to the list of OPs for units
OPs_unique <- merge(OPs_unique, units, by.x = 'Sample.Unit', 'customId', all.x = T)
OPs_unique <- OPs_unique %>% rename("unit.id" = "id")

#analysType must be ALL CAPS
OPs_unique$Analysis.Type <- toupper(OPs_unique$Analysis.Type)
OPs_unique$Result.Type <- toupper(OPs_unique$Result.Type)

#
url <- paste0(base_url, "v1/observedproperties")

for (j in seq(1, nrow(OPs_unique))) {
  data_body <- list("customId" = OPs_unique$NewNameID[j],
                    "name" = OPs_unique$Parm.Code[j],
                    "description" = OPs_unique$Description[j],
                    "resultType" = OPs_unique$Result.Type[j],
                    "analysisType" = OPs_unique$Analysis.Type[j],
                    "unitGroup" = list("id" = OPs_unique$unit.group.id[j]),
                    "defaultUnit" = list("id" = OPs_unique$unit.id[j]))
  
  x<-POST(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  
  print(OPs_unique$NewNameID[j])
  print(x$status_code)
  print(fromJSON(rawToChar(x$content))$message)
  
}

#for cat variables will need to add the values wanted
cat_values <- read_excel("./utils/config/ReferenceLists/New_OPS_not_in_EMS.xlsx", sheet = 2)

url <- paste0(base_url, "v1/observedproperties")

#list of OPs for which we need to assing categorical fixed values
OPs_to_update <- unique(cat_values$customId)

for (j in seq(1, length(OPs_to_update))) {
  
  #get the ID for each OP that we want to assign values for
  url <- paste0(base_url, "v1/observedproperties?customId=", unique(cat_values$customId)[j]) 
  data_body = list()
  x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  OP_ID <- fromJSON(rawToChar(x$content))$domainObjects$id

  #now assign value for this OP
  values <- cat_values$value[cat_values$customId == OPs_to_update[j]]
  
  for (i in seq(1, length(values))) {
    url = paste0(base_url, "v1/observedproperties/", OP_ID, "/categoricalvalues")
    
    data_body <- list(
                    list("customId" = values[i])
                    )
    
    x<-POST(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
    
    print(values[i])
    print(x$status_code)
    print(fromJSON(rawToChar(x$content))$message)
  }

}

#Taxonomic OPs------------------------------------------------------------------
OPs <- read.csv("./utils/config/ReferenceLists/Taxonomic_OP.csv", stringsAsFactors = F) #1607

#get the unique list of OP IDs
OPs_unique <- OPs %>% select(c("Parm.Code", "NewNameID", "Description", "Analysis.Type", "Sample.Unit.Group", "Sample.Unit","CAS"))
OPs_unique <- distinct(OPs_unique)

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

#add GUID to the list of OPs for unit groups
OPs_unique <- merge(OPs_unique, unit_grps, by.x = 'Sample.Unit.Group', 'customId')
OPs_unique <- OPs_unique %>% rename("unit.group.id" = "id")

#None of the taxonomic OPs have default units

#analysType must be ALL CAPS
OPs_unique$Analysis.Type <- toupper(OPs_unique$Analysis.Type)

#
url <- paste0(base_url, "v1/observedproperties")


#error holder these two are missing!
#"39369 - Carex" 734
#"40371 - Agropyron cristatum" 741

#use CAS for ITIS ID? Nope not allowed eye roll
#OPs_unique$CAS <- unlist(lapply(str_split(OPs_unique$NewNameID, " - "), function(x) x[[1]][1]))

for (j in seq(1, nrow(OPs_unique))) {
  data_body <- list("customId" = OPs_unique$NewNameID[j],
                    "name" = OPs_unique$Parm.Code[j],
                    "description" = OPs_unique$Description[j],
                    "resultType" = "NUMERIC",
                    "analysisType" = OPs_unique$Analysis.Type[j],
                    "unitGroup" = list("id" = OPs_unique$unit.group.id[j]))
  
  x<-POST(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  
  if (x$status_code != 200) {
    print(x$status_code)
    print(OPs_unique$NewNameID[j])
  }
  
}

#Methods------
OPs <- read_excel("./utils/config/ReferenceLists/Observed_Properties.xlsx")

#Method names cannot contain semicolons

#The units don't matter for non convertable OP units. For example microbial units. So remove them from consideration.
OPs$Sample.Unit[OPs$Convertable.In.Samples == "N"] <- ""

#get the new name (customId) and methods 
Methods <- OPs %>% select(c("NewNameID", "Method.Code", "Method", "Method.Description"))

#get the GUID for every NewNameID
url <- paste0(base_url, "v1/observedproperties")
data_body <- list()
x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
OP_ids <- fromJSON(rawToChar(x$content))$domainObjects %>% select("customId", "id")


Methods <- merge(Methods, OP_ids, by.x = "NewNameID", by.y = "customId") #check that nonthing has been dropped here!

list_methods <- unique(Methods %>% select(-c("NewNameID", "id")))

for (i in seq(1, nrow(list_methods))) {
  
  OPs_for_method <- Methods %>% filter(Method.Code == list_methods$Method.Code[i]) %>% select(id)
  
  result_list = list()
  
  for (j in seq(1, nrow(OPs_for_method))) {
    result_list[[j]] = list("id" = OPs_for_method$id[j])
  }
  
  #Make a new method and assign a single OP
  url <- paste0(base_url, 'v1/analysismethods')
  data_body <- list("methodId" = list_methods$Method.Code[i],
                    "name" = list_methods$Method[i],
                    "description" = list_methods$Method.Description[i],
                    "context" = "EMS Migration",
                    "observedProperties" = result_list
                    )
  
  x<-POST(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  
  if (x$status_code != 200) {
    print(i)
    print(x$status_code)
    print(list_methods$Method[i])
  }

}

#Labs-----
labs <- read.csv("./utils/config/ReferenceLists/Labs.csv", stringsAsFactors = F)

labs$Description = paste0("Created by ", labs$WHO_CREATED, " on ", labs$WHEN_CREATED)
  
url <- paste0(base_url, "v1/laboratories")

for (i in seq(1, nrow(labs))) {
  
  data_body <- list("customId" = labs$ID[i],
                    "name" = labs$Name[i],
                    "description" = labs$Description[i],
                    "address" = labs$Address[i],
                    "pointOfContact" = labs$Point.Of.Contact[i],
                    "emailAddress" = labs$Email[i],
                    "phoneNumber" = labs$Phone.Number[i]
                    )
  
  x <- POST(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  
  if(x$status_code != "200") {
    print(i)
    print(x$status_code)
  }
}

#Taxonomy--------
taxa <- read_excel("./utils/config/ReferenceLists/FishTaxonomy.xlsx", sheet = 1)

#get levels
url <- paste0(base_url, "v1/taxonomylevels")
data_body = list()

x <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

taxa_levels <- fromJSON(rawToChar(x$content))$domainObjects %>% select(id, customId)

#merge the level id
taxa <- merge(taxa, taxa_levels, by.x = 'Level', by.y = 'customId')

url <- paste0(base_url, "v1/taxons")

for (i in seq(1, nrow(taxa))) {
  
  data_body <- list("scientificName" = taxa$`Scientific Name`[i],
                    "commonName" = taxa$`Common Name`[i],
                    "TaxonomyLevel" = list("id" = taxa$id[i]),
                    "source" = taxa$Source[i],
                    "comment" = taxa$Comments[i],
                    "itisTsn" = taxa$`ITIS TSN`[i],
                    "itisURL" = "www.google.ca"
  )
  
  x <- POST(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  
  if(x$status_code != "200") {
    print(i)
    print(x$status_code)
  }
}


#Collection Methods----
collection_methods <- read_excel("./utils/config/ReferenceLists/Collection_methods.xlsx", sheet = 1)

#remove collection methods we no longer want
collection_methods <- collection_methods %>% filter(`New EnMoDS Short Name/ID` != "DELETE")

#select just the needed columms
collection_methods <- collection_methods %>% select(c("New EnMoDS Short Name/ID", "EMS CODE", "Definition"))

#merge ems codes into a long sting
collection_methods<-collection_methods %>% group_by(`New EnMoDS Short Name/ID`) %>% summarise(merged_codes = paste(`EMS CODE`, collapse = ", "), Definition)

#remove duplicates
collection_methods<-distinct(collection_methods)

#filter out where the ems code in NA
collection_methods$merged_codes[collection_methods$merged_codes == 'NA'] = ""

url <- paste0(base_url, "v1/collectionmethods")

for (i in seq(1, nrow(collection_methods))) {

  data_body <- list("customId" = collection_methods$`New EnMoDS Short Name/ID`[i],
                    "identifierOrganization" = collection_methods$merged_codes[i],
                    "name" = collection_methods$Definition[i])

  x <- POST(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  
  if(x$status_code != "200") {
    print(i)
    print(x$status_code)
    print(collection_methods$`New EnMoDS Short Name/ID`[i])
  }
}

#Locations

#Location Groups

#Saved Filter

#Standards

#Analytical Groups ----
OPs <- read_excel("./utils/config/ReferenceLists/Observed_Propertiesxlsx")

#Extract just the OP group and new name ID that is all that is needed
Analytical_Groups <- OPs %>% select(NewNameID, OP_Group) %>% unique()

#remove trailing semicolon
Analytical_Groups$OP_Group <- gsub(";", Analytical_Groups$OP_Group, replacement = "")

#check that every new name has 1 and only 1 group
check <- Analytical_Groups %>% group_by(NewNameID) %>% summarize(count = n()) #pass

#add GUIDS for each OP
url <- paste0(base_url, "v1/observedproperties")
data_body <- list()
x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

OPS_in_system <- fromJSON(rawToChar(x$content))$domainObjects %>% select(id, customId)

#merge GUIDS into the Analytical groups
Analytical_Groups <- merge(Analytical_Groups, OPS_in_system, by.x = "NewNameID", by.y = "customId") #check the count is the same and nothing is lost!


#Unique list of each analytical group
groups <- unique(Analytical_Groups$OP_Group)

for (j in seq(1, length(groups))) {
  
  A_group <- Analytical_Groups %>% filter(OP_Group == groups[j])
  
  #make a list of observed properties where each one has a list of it's id
  op <- lapply(A_group$id, function(x) list(id = x))
  analyticalGroupItems <- lapply(op, function(x) list(observedProperty = x))
  
  url <- paste0(base_url, "v1/analyticalgroups")
  data_body <- list("name" = A_group$OP_Group[1],
                    "description" = "group description here.",
                    "type" = "UNKNOWN",
                    "analyticalGroupItems" = analyticalGroupItems
  )
 
  
  x<-POST(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

}

#get analytical groups out (for editing)
url <- ""
data_body <- list()


x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
AG <- fromJSON(rawToChar(x$content))$domainObjects %>% select(id, name, description, analyticalGroupItems)

OP <- unlist(AG$analyticalGroupItems)

#get sampling context tags from USA environment

enmods_url <- ''

url <- paste0(enmods_url, "tags")
x<-GET(url, config = c(add_headers(.headers = c('Authorization' = enmods_token))), body = data_body, encode = 'json')

tags <- fromJSON(rawToChar(x$content))$domainObjects %>% select(name, description)

write.csv(tags, "samplingContextTags.csv", row.names = F)

### Upload sampling context tags
tags <- read.csv("./AsBuiltTraining/samplingContextTags.csv", stringsAsFactors = F)

url <- paste0(base_url, "v1/tags")

for (i in seq(1, nrow(tags))) {
  
  data_body <- list("name" = tags$name[i],
                    "description" = tags$description[i]
                    
  )
  
  x <- POST(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  
  if(x$status_code != "200") {
    print(i)
    print(x$status_code)
  }
}
