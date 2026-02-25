preprocessing_others <- function(env){

  #' Title: preprocessing_others
  #' @description
  #'  Pre-processes starter files for other parameters
  #' @param env AQS environment. Takes values "prod" or "test"
  #' @returns NULL
  #' @import readxl writexl openxlsx dplyr stringr readr
  #' @importFrom magrittr %>%
  #' @export
  #' @examples

# FILE TO PREPROCESS OTHER VARIABLES THAT MIGHT BE USED BY SAMPLING LOCATIONS
# REQUIRES THAT observed_properties ARE ALREADY IN THE SYSTEM

# EXTENDED ATTRIBUTES ----
# PREPROCESSING EXTENDED ATTRIBUTES FOR NEW DATA --------------------------

extended_attributes <- readxl::read_excel("./inst/extdata/Reference_Lists/Extended_Attributes.xlsx",
                                 sheet = "Extended_Attributes") %>%
  dplyr::rename_with(tolower) %>%
  dplyr::rename_with(~ gsub("\\.", "_", .)) %>%
  dplyr::rename_with(~ gsub(" ", "_", .)) %>%
  dplyr::rename(data_type = datatype,
         applies_to_type = appliestotype)

dropdownlists <- readxl::read_excel("./inst/extdata/Reference_Lists/Extended_Attributes.xlsx",
                            sheet = "Dropdownlists") %>%
  dplyr::rename_with(tolower) %>%
  dplyr::rename_with(~ gsub("\\.", "_", .))

dropdownlists <- dropdownlists %>%
  dplyr::select(ea_customid, ddl_customid) %>%
 dplyr::mutate(ddl_customid = as.character(ddl_customid)) %>%
 dplyr::group_by(ea_customid) %>%
  dplyr::summarise(
    dropdownlist = list(
      map(ddl_customid, ~ list(customId = .x))
    ),
    .groups = "drop"
  )

extended_attributes <- extended_attributes %>%
  dplyr::left_join(dropdownlists, by = dplyr::join_by(customid == ea_customid),
            keep = FALSE)

# DETECTION CONDITIONS ----
# PREPROCESSING TO GENERATE OLDER DETECTION CONDITION FILES ---------------
run_init <- FALSE

if(run_init){
  #Had to run this only once in a lifetime
  detection_conditions <- get_profiles("test", "detection_conditions") #%>%
  #dplyr::select(customId)

  # Save workbook
  writexl::write_xlsx(list(detection_conditions), "./inst/extdata/Reference_Lists/Detection_Conditions.xlsx")

  # Load an existing workbook
  wb <- openxlsx::loadWorkbook("./inst/extdata/Reference_Lists/Detection_Conditions.xlsx")

  # dplyr::rename a worksheet (e.g., change "OldSheet" to "NewSheet")
  openxlsx::renameWorksheet(wb, sheet = "Sheet1", newName = "Detection_Conditions")

  # Save the workbook with the updated sheet name
  openxlsx::saveWorkbook(wb, "./inst/extdata/Reference_Lists/Detection_Conditions.xlsx",
               overwrite = TRUE)
}

# PREPROCESSING DETECTION CONDITIONS FOR NEW DATA --------------------

detection_conditions <- readxl::read_excel("./inst/extdata/Reference_Lists/Detection_Conditions.xlsx",
                                  sheet = "Detection_Conditions") %>%
  dplyr::rename(customid = customId, system_code = systemCode)


# PROJECTS ----
# PREPROCESSING TO GENERATE THE TEST PROJECT LIST -----------------------------
run_init = TRUE
if(run_init){
  #had to run only once
  projects <- get_profiles("test", "projects")

  # Save workbook
  writexl::write_xlsx(projects, "./inst/extdata/Reference_Lists/Projects.xlsx")

  # Load an existing workbook
  wb <- openxlsx::loadWorkbook("./inst/extdata/Reference_Lists/Projects.xlsx")

  # dplyr::rename a worksheet (e.g., change "OldSheet" to "NewSheet")
  openxlsx::renameWorksheet(wb, sheet = "Sheet1", newName = "Projects")

  # Save the workbook with the updated sheet name
  openxlsx::saveWorkbook(wb, "./inst/extdata/Reference_Lists/Projects.xlsx", overwrite = TRUE)

}

# PREPROCESSING PROJECTS FOR NEW DATA ---------------------
  projects <- readxl::read_excel("./inst/extdata/Reference_Lists/Projects.xlsx",
                         sheet = "Projects")

  projects<- projects %>% dplyr::filter(type == "STUDY"|name == "Unknown")

  projects <- projects %>%
    dplyr::mutate(startTime = as.POSIXct(startTime),
                  endTime = as.POSIXct(endTime))

# #read the AMS data
# ams_url <- "https://www2.gov.bc.ca/assets/gov/environment/waste-management/waste-discharge-authorization/datamart/all_ams_authorizations.xlsx"
# download.file(ams_url, "all_ams_authorizations.xlsx", mode = "wb")

amsPermits <- read_excel("./inst/extdata/Reference_Lists/all_ams_authorizations_20260106.xlsx")

#storing relevant records of AMS Permits
amsPermits <- amsPermits %>%
  dplyr::select(`Authorization Number`, Company, `Issue Date`) %>%
  mutate(`Authorization Number` = as.character(`Authorization Number`))

#remove duplicates in AMS
amsPermits <- unique(amsPermits)

amsPermits <- amsPermits %>% mutate(Name = str_c(`Authorization Number`, Company, sep = ", "))

amsPermits <- amsPermits %>%
  mutate(Type = "ROUTINE_MONITORING") %>%
  rename(customId = `Authorization Number`,
         name = Name,
         type = Type,
         startTime = `Issue Date`) %>%
  dplyr::mutate(startTime = as.POSIXct(startTime))

amsPermits <- amsPermits %>% dplyr::select(-Company)

#Identify the new columns compared to observed_properties
new_cols <- setdiff(names(projects), names(amsPermits))

# Create a tibble of just those new columns
new_data <- projects %>%
    dplyr::select(all_of(new_cols)) %>%
    dplyr::mutate(across(everything(), ~ NA)) %>%
    unique() %>% dplyr::slice(rep(1, nrow(amsPermits)))

amsPermits <- amsPermits %>%
    dplyr::bind_cols(new_data)

projects <- projects %>%
    dplyr::bind_rows(amsPermits)

#Type needs to be in case UPPER
projects <- projects %>%
  dplyr::mutate(type = toupper(type),
                startTime = case_when(
                  is.na(startTime) ~ NA,
                  .default =  format(startTime, "%Y-%m-%dT00:00:00%z")
                ),
                endTime = case_when(
                  is.na(endTime) ~ NA,
                  .default =  format(endTime, "%Y-%m-%dT00:00:00%z")
                ))

# ANALYSIS METHODS -----------------------------------------------------------------
# PREPROCESSING ANALYSIS METHODS FOR NEW DATA --------------------------------------

analysis_methods <- readxl::read_excel("./inst/extdata/Reference_Lists/Consolidated_Observed_Properties.xlsx") %>%
    dplyr::rename_with(tolower) %>%
    dplyr::rename_with(~ gsub("\\.", "_", .))

#The units don't matter for non convertable OP units. For example microbial units. So remove them from consideration.
analysis_methods$sample_unit[analysis_methods$convertable_in_samples == "N"] <- ""

#get the new name (customId) and methods
analysis_methods <- analysis_methods %>%
  dplyr::select(tolower(c("NewNameID", "Method_Code", "Method",
                  "Method_Description")))

observed_properties.ids <- get_profiles(env, "observed_properties") %>%
  dplyr::select("id", "customId")

#check that nothing has been dropped here!
analysis_methods_problematic <- dplyr::anti_join(analysis_methods, observed_properties.ids,
                                 by = dplyr::join_by("newnameid" == "customId"))

analysis_methods <- dplyr::left_join(analysis_methods, observed_properties.ids,
                     by = dplyr::join_by("newnameid" == "customId"))

observed_properties_for_methods <- analysis_methods %>%
  dplyr::select(id, method_code) %>%
  dplyr::group_by(method_code) %>%
  dplyr::summarise(
    observed_properties_list = list(
      map(id, ~ list("id" = .x))
    ),
    .groups = "drop"
  )

analysis_methods <- unique(analysis_methods %>% select(-c("newnameid", "id")))

analysis_methods <- analysis_methods %>%
  dplyr::left_join(observed_properties_for_methods,
            by = dplyr::join_by(method_code == method_code),
            keep = FALSE)

#Method names cannot contain semicolons
#removing semicolons in names and replacing them with colons
analysis_methods <- analysis_methods %>%
  dplyr::mutate(method = stringr::str_replace_all(method, ";", ":")) %>%
  unique()

# analysis_methods_check <- analysis_methods %>%
#   dplyr::group_by(method_code, method) %>%
#   dplyr::mutate(counter = n()) %>%
#   dplyr::ungroup() %>%
#   dplyr::filter(counter>1) %>%
#   dplyr::arrange(method_code)

# analysis_methods_check <- analysis_methods %>%
#   dplyr::anti_join(analysis_methods_get,
#             by = dplyr::join_by(method_code == methodId, method == name))

# LABS --------------------------------------------------------------------
# PREPROCESSING TO GENERATE PARENT LAB FILES ------------------------------
run_new <- FALSE

run_new <- length(base::list.files(path = "./inst/extdata/Reference_Lists/",
                             pattern = "^EMS_Labs")) > 0

if(run_new){

  #if file matching pattern exists in this folder, it would be in this list
  file_exists <- base::list.files(path = "./inst/extdata/Reference_Lists/",
                            pattern = "^EMS_Labs", full.names = TRUE)

  latest_file <- file_exists[which.max(file.info(file_exists)$mtime)]
  message("Latest file found: ", latest_file)

  labs <- readr::read_csv(latest_file) %>%
    dplyr::rename_with(tolower) %>%
    dplyr::rename_with(~ gsub("\\.", "_", .)) %>%
    dplyr::rename_with(~ gsub("\\-", "_", .)) %>%
    dplyr::rename_with(~ gsub(" ", "_", .)) %>%
    dplyr::rename_with(
      ~ case_when(
        (.x == "address_1" & !any(str_detect(names(labs), "address"))) ~ "address",
        .x == "short_name" ~ "id",
        .x == "e_mail_addr" ~ "email",
        .default = .x
      )
    )

  labs$description = stringr::str_c("Created by ", labs$who_created, " on ", labs$when_created)

  # Save workbook
  writexl::write_xlsx(labs, "./inst/extdata/Reference_Lists/Labs.xlsx")

  # Load an existing workbook
  wb <- openxlsx::loadWorkbook("./inst/extdata/Reference_Lists/Labs.xlsx")

  # dplyr::rename a worksheet (e.g., change "OldSheet" to "NewSheet")
  openxlsx::renameWorksheet(wb, sheet = "Sheet1", newName = "Labs")

  # Save the workbook with the updated sheet name
  openxlsx::saveWorkbook(wb, "./inst/extdata/Reference_Lists/Labs.xlsx",
               overwrite = TRUE)

  # Define destination path
  destination_file <- file.path("./inst/extdata/Reference_Lists/Archived_Data",
                                basename(latest_file))

  #move the new file to archived_data
  base::file.rename(from = latest_file, to = destination_file)
}

# LOADING PARENT LAB FILES --------------------------------------

labs <- readxl::read_excel("./inst/extdata/Reference_Lists/Labs.xlsx",
                   sheet = "Labs")

# TAXONOMY LEVELS ---------------------------------------------------------
# PREPROCESSING TO GENERATE OLDER TAXONOMY LEVELS FILES ----------------
run_init = FALSE

if(run_init){
  #Had to run this only once in a lifetime
  taxonomy_levels <- get_profiles("test", "taxonomy_levels") %>%
    dplyr::select(customId)
  # Save workbook
  writexl::write_xlsx(list(taxonomy_levels), "./inst/extdata/Reference_Lists/Taxonomy_Levels.xlsx")

  # Load an existing workbook
  wb <- openxlsx::loadWorkbook("./inst/extdata/Reference_Lists/Taxonomy_Levels.xlsx")

  # dplyr::rename a worksheet (e.g., change "OldSheet" to "NewSheet")
  openxlsx::renameWorksheet(wb, sheet = "Sheet1", newName = "Taxonomy_Levels")

  # Save the workbook with the updated sheet name
  openxlsx::saveWorkbook(wb, "./inst/extdata/Reference_Lists/Taxonomy_Levels.xlsx",
               overwrite = TRUE)
}

# PREPROCESSING TAXONOMY LEVELS FOR NEW DATA ------------------------------

taxonomy_levels <- readxl::read_excel("./inst/extdata/Reference_Lists/Taxonomy_Levels.xlsx",
                             sheet = "Taxonomy_Levels")

# FISH TAXONOMY -----------------------------------------------------------
# PREPROCESSING FISH TAXONS FOR NEW DATA ----------------------------------

taxons <- readxl::read_excel("./inst/extdata/Reference_Lists/Fish_Taxonomy.xlsx",
                     sheet = "Taxonomy") %>%
  dplyr::rename_with(tolower) %>%
  dplyr::rename_with(~ gsub("\\.", "_", .)) %>%
  dplyr::rename_with(~ gsub(" ", "_", .))


# MEDIUMS ---------------------------------------------------------
mediums <- readxl::read_excel("./inst/extdata/Reference_Lists/Mediums.xlsx",
                      sheet = "Mediums")

# RESULT GRADES ---------------------------------------------------------
# PREPROCESSING TO GENERATE OLDER RESULT GRADES FILES --------------
run_init = FALSE

if(run_init){
  #Had to run this only once in a lifetime
  result_grades <- get_profiles("test", "result_grades") %>%
    dplyr::select(customId)

  # Save workbook
  writexl::write_xlsx(list(result_grades), "./inst/extdata/Reference_Lists/Result_Grades.xlsx")

  # Load an existing workbook
  wb <- openxlsx::loadWorkbook("./inst/extdata/Reference_Lists/Result_Grades.xlsx")

  # dplyr::rename a worksheet (e.g., change "OldSheet" to "NewSheet")
  openxlsx::renameWorksheet(wb, sheet = "Sheet1", newName = "Result_Grades")

  # Save the workbook with the updated sheet name
  openxlsx::saveWorkbook(wb, "./inst/extdata/Reference_Lists/Result_Grades.xlsx",
               overwrite = TRUE)
}

# PREPROCESSING RESULT GRADES FOR NEW DATA --------------------------------

result_grades <- readxl::read_excel("./inst/extdata/Reference_Lists/Result_Grades.xlsx",
                           sheet = "Result_Grades") #%>%
  #dplyr::mutate(systemCode = stringr::str_replace_all(systemCode, "NA", ""))



# Doing it manually; getting error on AQS's end

# RESULT STATUSES ---------------------------------------------------------
# PREPROCESSING TO GENERATE OLDER RESULT STATUSES FILES ---------------------
run_init = FALSE

if(run_init){
  #Had to run this only once in a lifetime
  result_statuses <- get_profiles("test", "result_statuses") %>%
    dplyr::select(customId)

  # Save workbook
  writexl::write_xlsx(list(result_statuses), "./inst/extdata/Reference_Lists/Result_Statuses.xlsx")

  # Load an existing workbook
  wb <- openxlsx::loadWorkbook("./inst/extdata/Reference_Lists/Result_Statuses.xlsx")

  # dplyr::rename a worksheet (e.g., change "OldSheet" to "NewSheet")
  openxlsx::renameWorksheet(wb, sheet = "Sheet1", newName = "Result_Statuses")

  # Save the workbook with the updated sheet name
  openxlsx::saveWorkbook(wb, "./inst/extdata/Reference_Lists/Result_Statuses.xlsx", overwrite = TRUE)
}

# PREPROCESSING RESULT STATUSES FOR NEW DATA ------------------------------

result_statuses <- readxl::read_excel("./inst/extdata/Reference_Lists/Result_Statuses.xlsx",
                             sheet = "Result_Statuses")

# COLLECTION METHODS ------------------------------------------------------
# PREPROCESSING COLLECTION METHODS FOR NEW DATA ---------------------------

collection_methods <- readxl::read_excel("./inst/extdata/Reference_Lists/Collection_Methods.xlsx",
                                sheet = "Collection_Methods") %>%
  dplyr::rename_with(tolower) %>%
  dplyr::rename_with(~ gsub("\\.", "_", .)) %>%
  dplyr::rename_with(~ gsub(" ", "_", .))

#remove collection methods we no longer want
collection_methods <- collection_methods %>%
  dplyr::filter(`new_enmods_short_name/id` != "DELETE")

collection_methods <- collection_methods %>%
  bind_rows(tibble(
    `new_enmods_short_name/id` = "GRAB - Sys Default",
    ems_code = "USEPA",
    definition = "Water Grab Sampling - no gear"
  ))

#merge ems codes into a long string
collection_methods <- collection_methods %>%
  dplyr::group_by(`new_enmods_short_name/id`, ems_code) %>%
  dplyr::reframe(merged_codes = paste(`ems_code`, collapse = ", "), definition)

# #select just the needed columns
# collection_methods <- collection_methods %>%
#   dplyr::select(c("new_enmods_short_name/id", "ems_code", "merged_codes", "definition"))

#replace EMS code with blanks where its NA
collection_methods$merged_codes[collection_methods$merged_codes == 'NA'] = ""

#remove duplicates
collection_methods <- dplyr::distinct(collection_methods)

# Save workbook
writexl::write_xlsx(collection_methods, "./inst/extdata/Reference_Lists/Collection_Methods.xlsx")

# Load an existing workbook
wb <- openxlsx::loadWorkbook("./inst/extdata/Reference_Lists/Collection_Methods.xlsx")

# dplyr::rename a worksheet (e.g., change "OldSheet" to "NewSheet")
openxlsx::renameWorksheet(wb, sheet = "Sheet1", newName = "Collection_Methods")

# Save the workbook with the updated sheet name
openxlsx::saveWorkbook(wb, "./inst/extdata/Reference_Lists/Collection_Methods.xlsx", overwrite = TRUE)

# ANALYTICAL GROUPS -------------------------------------------------------
# PREPROCESSING GROUPS FOR NEW DATA --------------------------------------

analytical_groups <- readxl::read_excel("./inst/extdata/Reference_Lists/Consolidated_Observed_Properties.xlsx") %>%
  dplyr::rename_with(tolower) %>%
  dplyr::rename_with(~ gsub("\\.", "_", .)) %>%
  dplyr::select(newnameid, op_group) %>% unique() %>%
  dplyr::filter(!is.na(op_group))

# LOCATION GROUP TYPES ----------------------------------------------------
# PREPROCESSING TO GENERATE OLDER LOCATION GROUP TYPES FILES ------------

run_init <- FALSE
if(run_init){
  #Had to run this only once in a lifetime
  location_group_types <- get_profiles("test", "location_group_types") %>%
    dplyr::select(customId)

  # Save workbook
  writexl::write_xlsx(list(location_group_types), "./inst/extdata/Reference_Lists/Location_Group_Types.xlsx")

  # Load an existing workbook
  wb <- openxlsx::loadWorkbook("./inst/extdata/Reference_Lists/Location_Group_Types.xlsx")

  # dplyr::rename a worksheet (e.g., change "OldSheet" to "NewSheet")
  openxlsx::renameWorksheet(wb, sheet = "Sheet1", newName = "Location_Group_Types")

  # Save the workbook with the updated sheet name
  openxlsx::saveWorkbook(wb, "./inst/extdata/Reference_Lists/Location_Group_Types.xlsx",
               overwrite = TRUE)
}

# PREPROCESSING LOCATION GROUP TYPES FOR NEW DATA -------------------------

location_group_types <- readxl::read_excel("./inst/extdata/Reference_Lists/Location_Group_Types.xlsx",
                                  sheet = "Location_Group_Types")
  # %>%
  # dplyr::rename_with(tolower) %>%
  # dplyr::rename_with(~ gsub("\\.", "_", .)) %>%
  # dplyr::rename_with(~ gsub(" ", "_", .))

# LOCATION TYPES ---------------------------------------------------------
location_types <- readxl::read_excel("./inst/extdata/Reference_Lists/Location_Types.xlsx",
                             sheet = "Location_Types")

location_types <- location_types %>%
  dplyr::mutate(customId = case_when(
    customId == "Land - Fram" ~ "Land - Farm",
    .default = customId
  ))






}
