# FILE TO PREPROCESS OTHER VARIABLES THAT MIGHT BE USED BY SAMPLING LOCATIONS
# REQUIRES THAT OPs ARE ALREADY IN THE SYSTEM

# EXTENDED ATTRIBUTES ----
# PREPROCESSING EXTENDED ATTRIBUTES FOR NEW DATA --------------------------

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
# PREPROCESSING TO GENERATE OLDER DETECTION CONDITION FILES ---------------
run_init <- FALSE

if(run_init){
  #Had to run this only once in a lifetime
  detectionConditions <- get_profiles("test", "detectionconditions") #%>%
  #dplyr::select(customId)
  
  # Save workbook
  write_xlsx(list(detectionConditions), "./utils/config/ReferenceLists/DetectionConditions.xlsx")
  
  # Load an existing workbook
  wb <- loadWorkbook("./utils/config/ReferenceLists/DetectionConditions.xlsx")
  
  # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
  renameWorksheet(wb, sheet = "Sheet1", newName = "detectionconditions")
  
  # Save the workbook with the updated sheet name
  saveWorkbook(wb, "./utils/config/ReferenceLists/DetectionConditions.xlsx", overwrite = TRUE)
}

# PREPROCESSING DETECTION CONDITIONS FOR NEW DATA --------------------

detectionConditions <- read_excel("./utils/config/ReferenceLists/DetectionConditions.xlsx", 
                                  sheet = "detectionconditions")

# SAVED FILTERS ----
# PREPROCESSING TO GENERATE OLDER SAVED FILTERS FILES ---------------

run_init = FALSE
if(run_init){
  #Had to run this only once in a lifetime
  savedFilters <- get_profiles("test", "filters") #%>%
  #dplyr::select(customId)
  
  # Save workbook
  write_xlsx(list(savedFilters), "./utils/config/ReferenceLists/savedfilters.xlsx")
  
  # Load an existing workbook
  wb <- loadWorkbook("./utils/config/ReferenceLists/savedfilters.xlsx")
  
  # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
  renameWorksheet(wb, sheet = "Sheet1", newName = "savedfilters")
  
  # Save the workbook with the updated sheet name
  saveWorkbook(wb, "./utils/config/ReferenceLists/savedfilters.xlsx", overwrite = TRUE)
}

# PREPROCESSING SAVED FILTERS FOR NEW DATA ---------------------------

savedFilters <- read_excel("./utils/config/ReferenceLists/savedfilters.xlsx", 
                           sheet = "savedfilters")

# PROJECTS ----
# PREPROCESSING PROJECTS FOR NEW DATA ---------------------

run_init = FALSE
if(run_init){
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
}

# METHODS -----------------------------------------------------------------
# PREPROCESSING METHODS FOR NEW DATA --------------------------------------

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

# # QA/QC for METHODS -------------------------------------------
# 
# #first post files
# post_check <- post_profiles("prod", "methods", methods)
# 
# get_check <- get_profiles("prod", "methods")
# 
# methods.missing <- methods %>%
#   anti_join(get_check,
#             by = join_by("Method.Code" == "methodId"))
# 
# 
# 
# LABS --------------------------------------------------------------------
# PREPROCESSING LABS FOR NEW DATA --------------------------------------

labs <- read.csv("./utils/config/ReferenceLists/Labs.csv", stringsAsFactors = F)

labs$Description = str_c("Created by ", labs$WHO_CREATED, " on ", labs$WHEN_CREATED)




# TAXONOMY LEVELS ---------------------------------------------------------
# PREPROCESSING TO GENERATE OLDER TAXONOMY LEVELS FILES ----------------
run_init = FALSE

if(run_init){
  #Had to run this only once in a lifetime
  taxonomyLevels <- get_profiles("test", "taxonomylevels") %>%
    dplyr::select(customId)
  # Save workbook
  write_xlsx(list(taxonomylevels), "./utils/config/ReferenceLists/TaxonomyLevels.xlsx")
  
  # Load an existing workbook
  wb <- loadWorkbook("./utils/config/ReferenceLists/TaxonomyLevels.xlsx")
  
  # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
  renameWorksheet(wb, sheet = "Sheet1", newName = "taxonomylevels")
  
  # Save the workbook with the updated sheet name
  saveWorkbook(wb, "./utils/config/ReferenceLists/TaxonomyLevels.xlsx", overwrite = TRUE)
}

# PREPROCESSING TAXONOMY LEVELS FOR NEW DATA ------------------------------

taxonomyLevels <- read_excel("./utils/config/ReferenceLists/TaxonomyLevels.xlsx", 
                             sheet = "taxonomylevels")





# MEDIUMS ---------------------------------------------------------
mediums <- read_excel("./utils/config/ReferenceLists/Mediums.xlsx", 
                      sheet = "Mediums")


# Doing it manually; getting error on AQS's end


# RESULT GRADES ---------------------------------------------------------
# PREPROCESSING TO GENERATE OLDER RESULT GRADES FILES --------------
run_init = FALSE

if(run_init){
  #Had to run this only once in a lifetime
  resultGrades <- get_profiles("test", "resultgrades") %>%
    dplyr::select(customId)
  
  # Save workbook
  write_xlsx(list(resultgrades), "./utils/config/ReferenceLists/ResultGrades.xlsx")
  
  # Load an existing workbook
  wb <- loadWorkbook("./utils/config/ReferenceLists/ResultGrades.xlsx")
  
  # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
  renameWorksheet(wb, sheet = "Sheet1", newName = "resultgrades")
  
  # Save the workbook with the updated sheet name
  saveWorkbook(wb, "./utils/config/ReferenceLists/ResultGrades.xlsx", overwrite = TRUE)
}

# PREPROCESSING RESULT GRADES FOR NEW DATA --------------------------------

resultGrades <- read_excel("./utils/config/ReferenceLists/ResultGrades.xlsx", 
                           sheet = "resultgrades")



# Doing it manually; getting error on AQS's end


# RESULT STATUSES ---------------------------------------------------------
# PREPROCESSING TO GENERATE OLDER RESULT STATUSES FILES ---------------------
run_init = FALSE

if(run_init){
  #Had to run this only once in a lifetime
  resultStatuses <- get_profiles("test", "resultstatuses") %>%
    dplyr::select(customId)
  
  # Save workbook
  write_xlsx(list(resultstatuses), "./utils/config/ReferenceLists/ResultStatuses.xlsx")
  
  # Load an existing workbook
  wb <- loadWorkbook("./utils/config/ReferenceLists/ResultStatuses.xlsx")
  
  # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
  renameWorksheet(wb, sheet = "Sheet1", newName = "resultstatuses")
  
  # Save the workbook with the updated sheet name
  saveWorkbook(wb, "./utils/config/ReferenceLists/ResultStatuses.xlsx", overwrite = TRUE)
}

# PREPROCESSING RESULT STATUSES FOR NEW DATA ------------------------------

resultStatuses <- read_excel("./utils/config/ReferenceLists/ResultStatuses.xlsx", 
                             sheet = "resultstatuses")




# FISH TAXONOMY -----------------------------------------------------------
# PREPROCESSING FISH TAXONS FOR NEW DATA ----------------------------------

taxons <- read_excel("./utils/config/ReferenceLists/FishTaxonomy.xlsx", 
                     sheet = "Taxonomy")




# COLLECTION METHODS ------------------------------------------------------
# PREPROCESSING COLLECTION METHODS FOR NEW DATA ---------------------------

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





