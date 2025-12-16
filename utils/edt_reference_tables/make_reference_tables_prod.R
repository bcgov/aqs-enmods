library(httr)
library(jsonlite)
#library(readxl)
library(dplyr)
library(stringr)


readRenviron(paste0(getwd(), "./.Renviron"))
#testToken <- Sys.getenv("TEST_TOKEN")
prodToken <- Sys.getenv("PROD_READ_ONLY_TOKEN")
#testURL <- Sys.getenv("TEST_URL")
prodURL <- Sys.getenv("PROD_URL")

#---Observed Properties Table---
OPs <- GET(paste0(prodURL, "v1/observedproperties/"), config = c(add_headers(.headers = c('Authorization' = prodToken ))), body = list(), encode = 'json')
OPs <-fromJSON(rawToChar(OPs$content))$domainObjects

#select and rename to make it look nice
OPs <- unnest(OPs, cols = c(unitGroup, defaultUnit), names_repair = "universal")

OPs <- OPs %>% select('Observed Property ID' = customId...2,
                    'Unit Group' = customId...9,
                    'Default Unit' = name...15,
                    'Supports Unit Conversion' = supportsConversion,
                    'EMS Code' = name...4,
                    'Description' = description,
                    'Result Type' = resultType,
                    'Analysis Type' = analysisType,
                    'CAS Number' = casNumber)

OPs <- OPs[order(OPs$`Observed Property ID`),]

write.csv(OPs, "./utils/edt_reference_tables/Observed_Properties.csv", row.names = F)

#---Analysis Methods Table---
AMs <- GET(paste0(prodURL, "v1/analysismethods"), config = c(add_headers(.headers = c('Authorization' = prodToken ))), body = list(), encode = 'json')
AMs <-fromJSON(rawToChar(AMs$content))$domainObjects

AMs <- unnest(AMs, cols = c(observedProperties), names_repair = "universal")

AMs <- AMs %>% select('Method ID' = methodId,
                      'Method Name' = name...4,
                      'Method Description' = description...6,
                      'Method Source' = context,
                      'Observed Property ID' = customId)

#write methods table to excel or make one bigger table with both methods and OPs
write.csv(AMs, "./utils/edt_reference_tables/Analytical_Methods.csv", row.names = F)

AMs <- unnest(AMs, cols = c(unitGroup, defaultUnit), names_repair = "universal")

AM_OP <- AMs %>% select('Observed Property ID' = customId...8,
                      'Observed Property Descritpion' = description...11,
                      'CAS Number' = casNumber,
                      'Unit Group' = customId...15,
                      'Default Unit' = customId...19,
                      'Supports Unit Conversion' = supportsConversion,
                      'EMS Code'= name...10,
                      'Analysis Type' = analysisType,
                      'Result Type' = resultType,
                      'Method ID' = methodId,
                      'Method Name' = name...4,
                      'Method Description' = description...6,
                      'Method Source' = context)

AM_OP <- AM_OP[order(AM_OP$`Observed Property ID`),]
#Table of both AM and OP
write.csv(AM_OP, "./utils/edt_reference_tables/Observed_Properties_and_Analytical_Methods.csv", row.names = F)

#---Units---
aqs_units <- GET(paste0(prodURL, "v1/units"), config = c(add_headers(.headers = c('Authorization' = prodToken ))), body = list(), encode = 'json')
aqs_units <-fromJSON(rawToChar(aqs_units$content))$domainObjects

aqs_units <- unnest(aqs_units, cols = c(unitGroup), names_repair = "universal")

#split the name for upload by ' - '
aqs_units <- aqs_units %>% separate(name, into = c("Code", "Name"), sep = " - ")

aqs_units <- aqs_units %>% select(Code,
                                  Name,
                                  'Supports Unit Conversion' = supportsConversion,
                                  'Unit Group' = customId...7)

aqs_units <- aqs_units[order(aqs_units$`Unit Group`),]

write.csv(aqs_units, "./utils/edt_reference_tables/Units.csv", row.names = F)

#--- Mediums ---
mediums <- GET(paste0(prodURL, "v1/mediums"), config = c(add_headers(.headers = c('Authorization' = prodToken ))), body = list(), encode = 'json')
mediums <-fromJSON(rawToChar(mediums$content))$domainObjects

mediums <- mediums %>% select("Medium" = customId)

write.csv(mediums, "./utils/edt_reference_tables/Mediums.csv", row.names = F)

#--- collection Methods ---
CM <- GET(paste0(prodURL, "v1/collectionmethods"), config = c(add_headers(.headers = c('Authorization' = prodToken ))), body = list(), encode = 'json')
CM <-fromJSON(rawToChar(CM$content))$domainObjects

CM <- CM %>% select("Name" = customId,
                    "Description" = name,
                    "EMS Collection Method Code" = identifierOrganization) %>%
  filter(Name != "SYS-REQUIRED")

write.csv(CM, "./utils/edt_reference_tables/Collection_Methods.csv", row.names = F)

#---Sampling Agencies 7b333754-1382-4ce1-abab-e2e9f0a8b89b
SA <- GET(paste0(prodURL, "v1/extendedattributes/7b333754-1382-4ce1-abab-e2e9f0a8b89b/dropdownlistitems"), config = c(add_headers(.headers = c('Authorization' = prodToken ))), body = list(), encode = 'json')
SA <-fromJSON(rawToChar(SA$content))$domainObjects

SA <- SA %>% select("Samply Agency" = customId)

write.csv(SA, "./utils/edt_reference_tables/Sampling_Agencies.csv", row.names = F)

#---Labs
labs <- GET(paste0(prodURL, "v1/laboratories"), config = c(add_headers(.headers = c('Authorization' = prodToken ))), body = list(), encode = 'json')
labs <-fromJSON(rawToChar(labs$content))$domainObjects

labs <- labs %>% select("Code" = customId,
                        "Name" = name) 

write.csv(labs, "./utils/edt_reference_tables/Labs.csv", row.names = F)

#---Tissue Types c2b7c1fe-92bd-4509-a5ff-b75340895664
TT <- GET(paste0(prodURL, "v1/extendedattributes/c2b7c1fe-92bd-4509-a5ff-b75340895664/dropdownlistitems"), config = c(add_headers(.headers = c('Authorization' = prodToken ))), body = list(), encode = 'json')
TT <-fromJSON(rawToChar(TT$content))$domainObjects

TT <- TT %>% select("Tissue Type" = customId) 

write.csv(TT, "./utils/edt_reference_tables/Tissue_Types.csv", row.names = F)

#---Biological Life Stage
BLS <- GET(paste0(prodURL, "v1/extendedattributes/9efc47d5-339b-48bf-a55d-e3de991f7f87/dropdownlistitems"), config = c(add_headers(.headers = c('Authorization' = prodToken ))), body = list(), encode = 'json')
BLS <-fromJSON(rawToChar(BLS$content))$domainObjects

BLS <- BLS %>% select("Biological Life Stage" = customId) 

write.csv(BLS, "./utils/edt_reference_tables/Biological_Life_Stage.csv", row.names = F)

#---Detection Conditions
DC <- GET(paste0(prodURL, "v1/detectionconditions"), config = c(add_headers(.headers = c('Authorization' = prodToken ))), body = list(), encode = 'json')
DC <-fromJSON(rawToChar(DC$content))$domainObjects

DC <- DC %>% select("Code" = customId,
                        "Name" = name,
                        "Description" = description) 

write.csv(DC, "./utils/edt_reference_tables/Detection_Conditions.csv", row.names = F)

#---Fish Species
TAX <- GET(paste0(prodURL, "v1/taxons"), config = c(add_headers(.headers = c('Authorization' = prodToken ))), body = list(), encode = 'json')
TAX <-fromJSON(rawToChar(TAX$content))$domainObjects

TAX <- TAX %>% select("Scientific Name" = scientificName,
                    "Common Name" = commonName,
                    "ITIS TSN" = itisTsn) 
write.csv(TAX, "./utils/edt_reference_tables/Fish_Taxons.csv", row.names = F)

#set up account access for BC box
Sys.setenv("AWS_ACCESS_KEY_ID" =  Sys.getenv("AWS_ACCESS_KEY"),
           "AWS_SECRET_ACCESS_KEY" =  Sys.getenv("AWS_SECRET_ACCESS_KEY"),
           "AWS_S3_ENDPOINT" = "nrs.objectstore.gov.bc.ca",
           "AWS_DEFAULT_REGION" = "")


#post to object store
put_object(file = "./utils/edt_reference_tables/Observed_Properties.csv", 
           object = "Reference_Lists/Observed_Properties.csv",
           bucket = "enmods",
           region = "",
           acl = "public-read")

put_object(file = "./utils/edt_reference_tables/Analytical_Methods.csv", 
           object = "Reference_Lists/Analytical_Methods.csv",
           bucket = "enmods",
           region = "",
           acl = "public-read")

put_object(file = "./utils/edt_reference_tables/Observed_Properties_and_Analytical_Methods.csv", 
           object = "Reference_Lists/Observed_Properties_and_Analytical_Methods.csv",
           bucket = "enmods",
           region = "",
           acl = "public-read")

put_object(file = "./utils/edt_reference_tables/Units.csv", 
           object = "Reference_Lists/Units.csv",
           bucket = "enmods",
           region = "",
           acl = "public-read")

put_object(file = "./utils/edt_reference_tables/Mediums.csv", 
           object = "Reference_Lists/Mediums.csv",
           bucket = "enmods",
           region = "",
           acl = "public-read")

put_object(file = "./utils/edt_reference_tables/Collection_Methods.csv", 
           object = "Reference_Lists/Collection_Methods.csv",
           bucket = "enmods",
           region = "",
           acl = "public-read")

put_object(file = "./utils/edt_reference_tables/Sampling_Agencies.csv", 
           object = "Reference_Lists/Sampling_Agencies.csv",
           bucket = "enmods",
           region = "",
           acl = "public-read")

put_object(file = "./utils/edt_reference_tables/Labs.csv", 
           object = "Reference_Lists/Labs.csv",
           bucket = "enmods",
           region = "",
           acl = "public-read")

put_object(file = "./utils/edt_reference_tables/Detection_Conditions.csv", 
           object = "Reference_Lists/Detection_Conditions.csv",
           bucket = "enmods",
           region = "",
           acl = "public-read")

put_object(file = "./utils/edt_reference_tables/Tissue_Types.csv", 
           object = "Reference_Lists/Tissue_Types.csv",
           bucket = "enmods",
           region = "",
           acl = "public-read")

put_object(file = "./utils/edt_reference_tables/Biological_Life_Stage.csv", 
           object = "Reference_Lists/Biological_Life_Stage.csv",
           bucket = "enmods",
           region = "",
           acl = "public-read")


put_object(file = "./utils/edt_reference_tables/Fish_Taxons.csv", 
           object = "Reference_Lists/Fish_Taxons.csv",
           bucket = "enmods",
           region = "",
           acl = "public-read")
