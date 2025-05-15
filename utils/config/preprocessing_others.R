# FILE TO PREPROCESS OTHER VARIABLES THAT MIGHT BE USED BY SAMPLING LOCATIONS
# REQUIRES THAT observed_properties ARE ALREADY IN THE SYSTEM

# EXTENDED ATTRIBUTES ----
# PREPROCESSING EXTENDED ATTRIBUTES FOR NEW DATA --------------------------

extended_attributes <- read_excel("./utils/config/ReferenceLists/Extended_Attributes.xlsx", 
                                 sheet = "Extended_Attributes") %>% 
  rename_with(tolower) %>%
  rename_with(~ gsub("\\.", "_", .)) %>%
  rename_with(~ gsub(" ", "_", .)) %>%
  rename(data_type = datatype, 
         applies_to_type = appliestotype)

dropdownlists <- read_excel("./utils/config/ReferenceLists/Extended_Attributes.xlsx", 
                            sheet = "Dropdownlists") %>% 
  rename_with(tolower) %>%
  rename_with(~ gsub("\\.", "_", .))

dropdownlists <- dropdownlists %>%
  dplyr::select(ea_customid, ddl_customid) %>%
  mutate(ddl_customid = as.character(ddl_customid)) %>%
  group_by(ea_customid) %>%
  summarise(
    dropdownlist = list(
      map(ddl_customid, ~ list(customId = .x))
    ),
    .groups = "drop"
  )

extended_attributes <- extended_attributes %>%
  left_join(dropdownlists, by = join_by(customid == ea_customid),
            keep = FALSE)

# DETECTION CONDITIONS ----
# PREPROCESSING TO GENERATE OLDER DETECTION CONDITION FILES ---------------
run_init <- FALSE

if(run_init){
  #Had to run this only once in a lifetime
  detection_conditions <- get_profiles("test", "detection_conditions") #%>%
  #dplyr::select(customId)
  
  # Save workbook
  write_xlsx(list(detection_conditions), "./utils/config/ReferenceLists/Detection_Conditions.xlsx")
  
  # Load an existing workbook
  wb <- loadWorkbook("./utils/config/ReferenceLists/Detection_Conditions.xlsx")
  
  # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
  renameWorksheet(wb, sheet = "Sheet1", newName = "Detection_Conditions")
  
  # Save the workbook with the updated sheet name
  saveWorkbook(wb, "./utils/config/ReferenceLists/Detection_Conditions.xlsx", 
               overwrite = TRUE)
}

# PREPROCESSING DETECTION CONDITIONS FOR NEW DATA --------------------

detection_conditions <- read_excel("./utils/config/ReferenceLists/Detection_Conditions.xlsx", 
                                  sheet = "Detection_Conditions") %>% 
  rename(customid = customId, system_code = systemCode)
  

# PROJECTS ----
# PREPROCESSING PROJECTS FOR NEW DATA ---------------------

run_init = FALSE
if(run_init){
  #had to run only once
  projects <- get_profiles("test", "projects")
  
  # Save workbook
  write_xlsx(projects, "./utils/config/ReferenceLists/Projects.xlsx")
  
  # Load an existing workbook
  wb <- loadWorkbook("./utils/config/ReferenceLists/Projects.xlsx")
  
  # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
  renameWorksheet(wb, sheet = "Sheet1", newName = "Projects")
  
  # Save the workbook with the updated sheet name
  saveWorkbook(wb, "./utils/config/ReferenceLists/Projects.xlsx", overwrite = TRUE)
  
}

  projects <- read_excel("./utils/config/ReferenceLists/Projects.xlsx", 
                         sheet = "Projects")
  
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

# METHODS -----------------------------------------------------------------
# PREPROCESSING METHODS FOR NEW DATA --------------------------------------

methods <- read_excel("./utils/config/ReferenceLists/Consolidated_Observed_Properties.xlsx") %>% 
    rename_with(tolower) %>% 
    rename_with(~ gsub("\\.", "_", .))

#The units don't matter for non convertable OP units. For example microbial units. So remove them from consideration.
methods$sample_unit[methods$convertable_in_samples == "N"] <- ""

#get the new name (customId) and methods 
methods <- methods %>% 
  dplyr::select(tolower(c("NewNameID", "Method_Code", "Method", 
                  "Method_Description")))

observed_properties.ids <- get_profiles(env, "observed_properties") %>% 
  dplyr::select("id", "customId")

#check that nothing has been dropped here!
methods_problematic <- anti_join(methods, observed_properties.ids, 
                                 by = join_by("newnameid" == "customId")) 

methods <- left_join(methods, observed_properties.ids, 
                     by = join_by("newnameid" == "customId"))

observed_properties_for_methods <- methods %>%
  dplyr::select(id, method_code) %>%
  group_by(method_code) %>%
  summarise(
    observed_properties_list = list(
      map(id, ~ list("id" = .x))
    ),
    .groups = "drop"
  )

methods <- unique(methods %>% select(-c("newnameid", "id")))

methods <- methods %>%
  left_join(observed_properties_for_methods, 
            by = join_by(method_code == method_code),
            keep = FALSE)

#Method names cannot contain semicolons
#removing semicolons in names and replacing them with colons
methods <- methods %>% 
  mutate(method = str_replace_all(method, ";", ":"))

# # QA/QC for METHODS -------------------------------------------
# 
# #first post files
# post_check <- post_profiles(env, "methods", methods)
# 
# get_check <- get_profiles(env, "methods")
# 
# methods.missing <- methods %>%
#   anti_join(get_check,
#             by = join_by("method_code" == "methodId"))
# 
# 
# 
# LABS --------------------------------------------------------------------
# PREPROCESSING LABS FOR NEW DATA --------------------------------------

labs <- read.csv("./utils/config/ReferenceLists/Labs.csv", stringsAsFactors = F) %>% 
  rename_with(tolower) %>%
  rename_with(~ gsub("\\.", "_", .))

labs$description = str_c("Created by ", labs$who_created, " on ", labs$when_created)

# TAXONOMY LEVELS ---------------------------------------------------------
# PREPROCESSING TO GENERATE OLDER TAXONOMY LEVELS FILES ----------------
run_init = FALSE

if(run_init){
  #Had to run this only once in a lifetime
  taxonomy_levels <- get_profiles("test", "taxonomy_levels") %>%
    dplyr::select(customId)
  # Save workbook
  write_xlsx(list(taxonomy_levels), "./utils/config/ReferenceLists/Taxonomy_Levels.xlsx")
  
  # Load an existing workbook
  wb <- loadWorkbook("./utils/config/ReferenceLists/Taxonomy_Levels.xlsx")
  
  # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
  renameWorksheet(wb, sheet = "Sheet1", newName = "Taxonomy_Levels")
  
  # Save the workbook with the updated sheet name
  saveWorkbook(wb, "./utils/config/ReferenceLists/Taxonomy_Levels.xlsx", 
               overwrite = TRUE)
}

# PREPROCESSING TAXONOMY LEVELS FOR NEW DATA ------------------------------

taxonomy_levels <- read_excel("./utils/config/ReferenceLists/Taxonomy_Levels.xlsx", 
                             sheet = "Taxonomy_Levels")

# MEDIUMS ---------------------------------------------------------
mediums <- read_excel("./utils/config/ReferenceLists/Mediums.xlsx", 
                      sheet = "Mediums")

# Doing it manually; getting error on AQS's end

# RESULT GRADES ---------------------------------------------------------
# PREPROCESSING TO GENERATE OLDER RESULT GRADES FILES --------------
run_init = FALSE

if(run_init){
  #Had to run this only once in a lifetime
  result_grades <- get_profiles("test", "result_grades") %>%
    dplyr::select(customId)
  
  # Save workbook
  write_xlsx(list(result_grades), "./utils/config/ReferenceLists/Result_Grades.xlsx")
  
  # Load an existing workbook
  wb <- loadWorkbook("./utils/config/ReferenceLists/Result_Grades.xlsx")
  
  # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
  renameWorksheet(wb, sheet = "Sheet1", newName = "Result_Grades")
  
  # Save the workbook with the updated sheet name
  saveWorkbook(wb, "./utils/config/ReferenceLists/Result_Grades.xlsx", 
               overwrite = TRUE)
}

# PREPROCESSING RESULT GRADES FOR NEW DATA --------------------------------

result_grades <- read_excel("./utils/config/ReferenceLists/Result_Grades.xlsx", 
                           sheet = "Result_Grades")

# Doing it manually; getting error on AQS's end

# RESULT STATUSES ---------------------------------------------------------
# PREPROCESSING TO GENERATE OLDER RESULT STATUSES FILES ---------------------
run_init = FALSE

if(run_init){
  #Had to run this only once in a lifetime
  result_statuses <- get_profiles("test", "result_statuses") %>%
    dplyr::select(customId)
  
  # Save workbook
  write_xlsx(list(result_statuses), "./utils/config/ReferenceLists/Result_Statuses.xlsx")
  
  # Load an existing workbook
  wb <- loadWorkbook("./utils/config/ReferenceLists/Result_Statuses.xlsx")
  
  # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
  renameWorksheet(wb, sheet = "Sheet1", newName = "Result_Statuses")
  
  # Save the workbook with the updated sheet name
  saveWorkbook(wb, "./utils/config/ReferenceLists/Result_Statuses.xlsx", overwrite = TRUE)
}

# PREPROCESSING RESULT STATUSES FOR NEW DATA ------------------------------

result_statuses <- read_excel("./utils/config/ReferenceLists/Result_Statuses.xlsx", 
                             sheet = "Result_Statuses")

# FISH TAXONOMY -----------------------------------------------------------
# PREPROCESSING FISH TAXONS FOR NEW DATA ----------------------------------

taxons <- read_excel("./utils/config/ReferenceLists/Fish_Taxonomy.xlsx", 
                     sheet = "Taxonomy") %>% 
  rename_with(tolower) %>%
  rename_with(~ gsub("\\.", "_", .)) %>%
  rename_with(~ gsub(" ", "_", .))

# COLLECTION METHODS ------------------------------------------------------
# PREPROCESSING COLLECTION METHODS FOR NEW DATA ---------------------------

collection_methods <- read_excel("./utils/config/ReferenceLists/Collection_Methods.xlsx", 
                                sheet = "Collection_Methods") %>% 
  rename_with(tolower) %>%
  rename_with(~ gsub("\\.", "_", .)) %>%
  rename_with(~ gsub(" ", "_", .))
  

#remove collection methods we no longer want
collection_methods <- collection_methods %>% 
  filter(`new_enmods_short_name/id` != "DELETE")

#select just the needed columns
collection_methods <- collection_methods %>% 
  select(c("new_enmods_short_name/id", "ems_code", "definition"))

#merge ems codes into a long string
collection_methods <- collection_methods %>% 
  group_by(`new_enmods_short_name/id`) %>% 
  reframe(merged_codes = paste(`ems_code`, collapse = ", "), definition)

#remove duplicates
collection_methods<-distinct(collection_methods)

#replace EMS code with blanks where its NA
collection_methods$merged_codes[collection_methods$merged_codes == 'NA'] = ""






# LOCATION GROUP TYPES ----------------------------------------------------
# PREPROCESSING TO GENERATE OLDER LOCATION GROUP TYPES FILES ------------

run_init <- FALSE
if(run_init){
  #Had to run this only once in a lifetime
  location_group_types <- get_profiles("test", "location_group_types") %>%
    dplyr::select(customId)
  
  # Save workbook
  write_xlsx(list(location_group_types), "./utils/config/ReferenceLists/Location_Group_Types.xlsx")
  
  # Load an existing workbook
  wb <- loadWorkbook("./utils/config/ReferenceLists/Location_Group_Types.xlsx")
  
  # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
  renameWorksheet(wb, sheet = "Sheet1", newName = "Location_Group_Types")
  
  # Save the workbook with the updated sheet name
  saveWorkbook(wb, "./utils/config/ReferenceLists/Location_Group_Types.xlsx", 
               overwrite = TRUE)
}

# PREPROCESSING LOCATION GROUP TYPES FOR NEW DATA -------------------------

location_group_types <- read_excel("./utils/config/ReferenceLists/Location_Group_Types.xlsx", 
                                  sheet = "Location_Group_Types")
  # %>% 
  # rename_with(tolower) %>%
  # rename_with(~ gsub("\\.", "_", .)) %>%
  # rename_with(~ gsub(" ", "_", .))

# LOCATION TYPES ---------------------------------------------------------
location_types <- read_excel("./utils/config/ReferenceLists/Location_Types.xlsx", 
                             sheet = "Location_Types")

location_types <- location_types %>% 
  mutate(customId = case_when(
    customId == "Land - Fram" ~ "Land - Farm",
    .default = customId
  ))


