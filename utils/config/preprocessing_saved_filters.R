# SAVED FILTERS ----
# PREPROCESSING TO GENERATE OLDER SAVED FILTERS FILES ---------------

run_init = FALSE
if(run_init){
  
  # Have to run a renaming code coz file name is too long to import otherwise
  # List files starting with the old prefix
  files_to_rename <- list.files(path = "./utils/config/ReferenceLists/", pattern = paste0("^", "EMS_Monitoring_Groups_"), full.names = TRUE)
  
  # Generate new file names
  new_file_names <- file.path(path = "./utils/config/ReferenceLists", "Saved_Filters.xlsx")
  
  # Rename the files
  file.rename(from = files_to_rename, to = new_file_names)
  
  #Had to run this only once in a lifetime
  saved_filters <- read_csv("./utils/config/ReferenceLists/Saved_Filters.xlsx") %>% 
    rename_with(tolower) %>% 
    rename_with(~ gsub("\\.", "_", .)) %>% 
    rename_with(~ gsub("\\-", "_", .)) %>%
    rename_with(~ gsub(" ", "_", .))
  
  #saved_filters <- get_profiles("test", "filters") %>% 
  #dplyr::select(customId)
  
  # Save workbook
  write_xlsx(saved_filters, "./utils/config/ReferenceLists/Saved_Filters.xlsx")
  
  # Load an existing workbook
  wb <- loadWorkbook("./utils/config/ReferenceLists/Saved_Filters.xlsx")
  
  # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
  renameWorksheet(wb, sheet = "Sheet1", newName = "Saved_Filters")
  
  # Save the workbook with the updated sheet name
  saveWorkbook(wb, "./utils/config/ReferenceLists/Saved_Filters.xlsx", overwrite = TRUE)
  
}

# PREPROCESSING SAVED FILTERS FOR NEW DATA ---------------------------

locations <- get_profiles(env, "locations")

saved_filters <- read_excel("./utils/config/ReferenceLists/Saved_Filters.xlsx", 
                            sheet = "Saved_Filters") %>% rename_with(tolower) %>% 
  rename_with(tolower) %>% 
  rename_with(~ gsub("\\.", "_", .)) %>% 
  rename_with(~ gsub("\\-", "_", .)) %>%
  rename_with(~ gsub(" ", "_", .))

saved_filters <- saved_filters %>% 
  inner_join(locations %>% dplyr::select(id, customId, name) %>%
               rename(location_guid = id, location_name = name),
             by = join_by("location_id" == "customId",
                          "location_name")) %>% 
  dplyr::select(c(id, name, location_guid, comments)) %>% 
  unique()

saved_filters <- saved_filters %>%
  dplyr::select(name, comments, location_guid) %>%
  mutate(location_guid = as.character(location_guid)) %>%
  group_by(name, comments) %>%
  #summarise(sampling_locations = list(id = location_guid), .groups = "drop") %>% 
  summarise(
    sampling_locations = list(
      map(location_guid, ~ list(id = .x))
    ),
    .groups = "drop"
  ) %>%
  ungroup() %>% 
  unique()