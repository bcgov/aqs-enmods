#This script runs on demand as a github action used to maintain the json files
#that control the drop down selections in the CoC CHEFS forms.

#By J-Krogh

#May 2 2025

library(aws.s3)
library(readxl)

#get the API token from your environment file
readRenviron(paste0(getwd(), "./.Renviron"))
testToken <- Sys.getenv("TEST_READ_ONLY_TOKEN")
prodToken <- Sys.getenv("PROD_READ_ONLY_TOKEN")
testURL <- Sys.getenv("TEST_URL")
prodURL <- Sys.getenv("PROD_URL")

#set up account access for BC box
Sys.setenv("AWS_ACCESS_KEY_ID" =  Sys.getenv("AWS_ACCESS_KEY"),
           "AWS_SECRET_ACCESS_KEY" =  Sys.getenv("AWS_SECRET_ACCESS_KEY"),
           "AWS_S3_ENDPOINT" = "nrs.objectstore.gov.bc.ca")

source("./utils/config/ref_functions_json_proc.R")



# #example code using the reference functions -------------------------------
# MEDIUMS -----------------------------------------------------------------

#reading in EnMoDS config
mediums <- get_profiles("prod", "mediums") %>% 
  keep(names(.) %in% gen_list_rel_var("mediums"))

#extracting medium names from the imported mediums file
jsonMediumsRaw <- mediums %>% dplyr::select(customId)

#storing medium names in JSON format
jsonMediumsProc <- toJSON(list(items = jsonMediumsRaw), pretty = TRUE)

#writing the created JSON file to upload to BC Box
write(jsonMediumsProc, file = "enmods_mediums_data.json")

#post to object store
put_object(file = jsonMediumsProc, 
           object = "CoC_Tables/mediums_PROD.json",
           bucket = "enmods",
           region = "",
           acl = "public-read")


# LOCATIONS ---------------------------------------------------------------
if (FALSE) {
# #reading in EnMoDS config
 locations <- get_profiles("test", "locations") %>% 
   keep(names(.) %in% gen_list_rel_var("samplinglocations"))

#locations$emsDateModified <- lapply(locations$extendedAttributes,)
# 
# #selecting columns and renaming them as required
 jsonLocationsRaw <- locations %>%
   dplyr::select(customId, name) %>%
   rename(disp_name = name) %>%
   mutate(disp_name = str_c(customId, " - ", disp_name)) %>%
   dplyr::select(disp_name)
# 
# #processing data into JSON format
# #added the items keyword to store variables in an array
 jsonLocationsProc <- toJSON(list(items = jsonLocationsRaw), pretty = TRUE)
# 
# #writing the created JSON file
write(jsonLocationsProc, file = "enmods_locations_data.json")

put_object(file = "full_enmods_locations_data.json", 
           object = "CoC_Tables/enmods_locations_data.json",
           bucket = "enmods",
           region = "",
           acl = "public-read")
}
# LABS --------------------------------------------------------------------

#reading in EnMoDS config
labs <- get_profiles("prod", "labs") %>% 
  keep(names(.) %in% gen_list_rel_var("labs"))

#processing data into JSON format
jsonLabsProc <- toJSON(list(items = labs), pretty = TRUE)

#writing the created JSON file
write(jsonLabsProc, file = "enmods_labs_data.json")

#Post to object store
put_object(file = "enmods_labs_data.json", 
           object = "CoC_Tables/labs_PROD.json",
           bucket = "enmods",
           region = "",
           acl = "public-read")

# SAMPLING AGENCY ---------------------------------------------------------

# #reading in EnMoDS config
samplingAgency <- dropdownlist_extended_attributes("prod", "Sampling Agency")

samplingAgency <- samplingAgency$domainObjects
# #processing data into JSON format
 jsonSamplingAgencyProc <- toJSON(list(items = samplingAgency), pretty = TRUE)
# 
# #writing the created JSON file
 write(jsonSamplingAgencyProc, file = "enmods_samplingagency_data.json")
 
 #Post to object store
 put_object(file = "enmods_samplingagency_data.json", 
            object = "CoC_Tables/sampling_agency_PROD.json",
            bucket = "enmods",
            region = "",
            acl = "public-read")

# COLLECTION METHODS ------------------------------------------------------

#reading in EnMoDS config
collectionMethods <- get_profiles("prod", "collectionmethods") %>% 
  keep(names(.) %in% gen_list_rel_var("collectionmethods"))

#processing data into JSON format
jsonCollectionMethodsProc <- toJSON(list(items = collectionMethods), pretty = TRUE)

#writing the created JSON file
write(jsonCollectionMethodsProc, file = "enmods_collectionmethods_data.json")

#Post to object store
put_object(file = "enmods_collectionmethods_data.json", 
           object = "CoC_Tables/collection_methods_PROD.json",
           bucket = "enmods",
           region = "",
           acl = "public-read")


# PROJECTS ----------------------------------------------------------------

#reading in EnMoDS config
projects <- get_profiles("prod", "projects") %>%
  keep(names(.) %in% gen_list_rel_var("projects")) 

#further selecting relevant columns
jsonProjectsRaw <- projects %>%
  dplyr::select(customId, name, description, type) %>%
  dplyr::filter(type!="ROUTINE_MONITORING")

#processing data into JSON format
jsonProjectsProc <- toJSON(list(items = jsonProjectsRaw), pretty = TRUE)

#writing the created JSON file
write(jsonProjectsProc, file = "enmods_projects_data.json")

#Post to object store
put_object(file = "enmods_projects_data.json", 
           object = "CoC_Tables/projects_PROD.json",
           bucket = "enmods",
           region = "",
           acl = "public-read")


#lab analysis packages, run on demand as needed
if (FALSE) {
json_analysisPackagesCoC_data <- read_excel("./utils/coc/analysisPackagesCoC - May-7-2025.xlsx") 

#### water and soil
water_soil <- json_analysisPackagesCoC_data %>% filter(matrix %in% c("water", "soil"))

json_water_soil <- water_soil %>%
  dplyr::select(-c(preservative.name)) %>%
  rename(bottle = items.bottle, 
         filter = items.field.filter,
         preservative = items.preservative,
         analysisGroup = items.analysisGroup
  ) %>%
  group_by(matrix, analysisGroup, bottle, filter, preservative, comments) %>%
  #rowwise() %>%
  mutate(observedProperties = list(tibble(label = test.label, 
                                          analysisGroup = analysisGroup))) %>%
  ungroup() %>%
  dplyr::select(-c(test.label)) %>% unique()

json_water_soil <- toJSON(list(items = json_water_soil), pretty = TRUE)

#writing the created JSON file
write(json_water_soil, file = "water_soil_analysisPackages_data.json")

### air
air <- json_analysisPackagesCoC_data %>% filter(matrix %in% c("air"))

json_air <- air %>%
  dplyr::select(-c(preservative.name)) %>%
  rename(bottle = items.bottle, 
         filter = items.field.filter,
         preservative = items.preservative,
         analysisGroup = items.analysisGroup
  ) %>%
  group_by(matrix, analysisGroup, bottle, filter, preservative, comments) %>%
  #rowwise() %>%
  mutate(observedProperties = list(tibble(label = test.label, 
                                          analysisGroup = analysisGroup))) %>%
  ungroup() %>%
  dplyr::select(-c(test.label)) %>% unique()

json_air <- toJSON(list(items = json_air), pretty = TRUE)

#writing the created JSON file
write(json_air, file = "air_analysisPackages_data.json")

### Biological
bio <- json_analysisPackagesCoC_data %>% filter(matrix %in% c("biological"))

json_bio <- bio %>%
  dplyr::select(-c(preservative.name)) %>%
  rename(bottle = items.bottle, 
         filter = items.field.filter,
         preservative = items.preservative,
         analysisGroup = items.analysisGroup
  ) %>%
  group_by(matrix, analysisGroup, bottle, filter, preservative, comments) %>%
  #rowwise() %>%
  mutate(observedProperties = list(tibble(label = test.label, 
                                          analysisGroup = analysisGroup))) %>%
  ungroup() %>%
  dplyr::select(-c(test.label)) %>% unique()

json_bio <- toJSON(list(items = json_bio), pretty = TRUE)

#writing the created JSON file
write(json_bio, file = "bio_analysisPackages_data.json")

### Upload to BC Box

#Post to object store
put_object(file = "water_soil_analysisPackages_data.json", 
           object = "CoC_Tables/PROD_water_soil_AnalyticalPackages.json",
           bucket = "enmods",
           region = "",
           acl = "public-read")

#Post to object store
put_object(file = "air_analysisPackages_data.json", 
           object = "CoC_Tables/PROD_air_AnalyticalPackages.json",
           bucket = "enmods",
           region = "",
           acl = "public-read")

#Post to object store
put_object(file = "bio_analysisPackages_data.json", 
           object = "CoC_Tables/PROD_bio_AnalyticalPackages.json",
           bucket = "enmods",
           region = "",
           acl = "public-read")


}