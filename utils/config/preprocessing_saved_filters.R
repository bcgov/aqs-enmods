preprocessing_saved_filters <- function(env){

  #' Title: preprocessing_saved_filters
  #' @description
  #'  Pre-processes starter files for saved filters
  #' @param env AQS environment. Takes values "prod" or "test"
  #' @returns NULL
  #' @import readxl writexl openxlsx dplyr stringr readr
  #' @importFrom magrittr %>%
  #' @export
  #' @examples


# SAVED FILTERS ----
# PREPROCESSING TO GENERATE OLDER SAVED FILTERS FILES ---------------

run_init = TRUE
if(run_init){

  # Have to run a renaming code coz file name is too long to import otherwise
  # List files starting with the old prefix
  files_to_rename <- base::list.files(path = "./inst/extdata/Reference_Lists/", pattern = paste0("^", "EMS_Monitoring_Groups_"), full.names = TRUE)

  # Generate new file names
  new_file_names <- base::file.path(path = "./inst/extdata/Reference_Lists", "Saved_Filters.xlsx")

  # Rename the files
  base::file.rename(from = files_to_rename, to = new_file_names)

  #Had to run this only once in a lifetime
  saved_filters <- readr::read_csv("./inst/extdata/Reference_Lists/Saved_Filters.xlsx") %>%
    dplyr::rename_with(tolower) %>%
    dplyr::rename_with(~ gsub("\\.", "_", .)) %>%
    dplyr::rename_with(~ gsub("\\-", "_", .)) %>%
    dplyr::rename_with(~ gsub(" ", "_", .))

  #saved_filters <- get_profiles("test", "filters") %>%
  #dplyr::select(customId)

  # Save workbook
  writexl::write_xlsx(saved_filters, "./inst/extdata/Reference_Lists/Saved_Filters.xlsx")

  # Load an existing workbook
  wb <- openxlsx::loadWorkbook("./inst/extdata/Reference_Lists/Saved_Filters.xlsx")

  # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
  openxlsx::renameWorksheet(wb, sheet = "Sheet1", newName = "Saved_Filters")

  # Save the workbook with the updated sheet name
  openxlsx::saveWorkbook(wb, "./inst/extdata/Reference_Lists/Saved_Filters.xlsx", overwrite = TRUE)

}

# # PREPROCESSING SAVED FILTERS FOR NEW DATA ---------------------------
#
# locations <- get_profiles(env, "locations")
#
# saved_filters <- readxl::read_excel("./inst/extdata/Reference_Lists/Saved_Filters.xlsx",
#                             sheet = "Saved_Filters") %>% dplyr::rename_with(tolower) %>%
#   dplyr::rename_with(tolower) %>%
#   dplyr::rename_with(~ gsub("\\.", "_", .)) %>%
#   dplyr::rename_with(~ gsub("\\-", "_", .)) %>%
#   dplyr::rename_with(~ gsub(" ", "_", .))
#
# saved_filters <- saved_filters %>%
#   dplyr::inner_join(locations %>% dplyr::select(id, customId, name) %>%
#                dplyr::rename(location_guid = id, location_name = name),
#              by = dplyr::join_by("location_id" == "customId",
#                           "location_name")) %>%
#   dplyr::select(c(id, name, location_guid, comments)) %>%
#   unique()
#
# saved_filters <- saved_filters %>%
#   dplyr::select(name, comments, location_guid) %>%
#   dplyr::mutate(location_guid = as.character(location_guid)) %>%
#   dplyr::group_by(name, comments) %>%
#   #summarise(sampling_locations = list(id = location_guid), .groups = "drop") %>%
#   dplyr::summarise(
#     sampling_locations = list(
#       map(location_guid, ~ list(id = .x))
#     ),
#     .groups = "drop"
#   ) %>%
#   dplyr::ungroup() %>%
#   unique()

}
