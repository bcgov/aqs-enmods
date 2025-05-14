# FILE TO PREPROCESS UNITS and UNIT GROUPS
source("./utils/config/api_functions.R")

# PREPROCESSING TO CONSOLIDATE OLDER UNITS FILES--------
run_init <- FALSE
if (run_init) {
  
  # initialization code
  units <- read_csv("./utils/config/ReferenceLists/Units_ems_jk_2025_04_16.csv") %>% 
    rename_with(tolower) %>%
    rename_with(~ gsub("\\.", "_", .)) %>%
    mutate(meas_unit_cd = as.character(meas_unit_cd)) %>%
    mutate(code = as.character(code)) %>%
    dplyr::select(-conversion_factor) %>%
    mutate(code = str_replace(code, "^0+", ""))
  #Only conversions in this file are reliable
  units_base <- read_excel("./utils/config/ReferenceLists/Units.xlsx", sheet = "Units") %>% mutate(code = str_replace(code, "^0+", ""))
  #%>% dplyr::select(c(code, conversion_factor, offset, sample_unit_group, sample_unit_customId, sample_unit_name))
  
  # Identify the new columns from units_base
  new_cols <- setdiff(names(units_base), names(units))
  
  # Create a tibble of just those new columns
  new_data <- units_base %>%
    dplyr::select(all_of(new_cols)) %>%
    mutate(across(everything(), ~ NA)) %>%
    unique() %>% slice(rep(1, nrow(units)))
  
  units <- units %>% bind_cols(new_data)
  
  units <- units %>% bind_rows(units_base %>% mutate(results = NA)) %>% unique()
  
  # Columns to move
  cols_to_move <- c("code", "sample_unit_customId", "sample_unit_name",
                    "sample_unit_short_name", "sample_unit_group",
                    "sample_unit_modifier", "results", "base unit",
                    "offset", "convertible")
  
  # Reorder with selected columns at the end
  units <- units %>%
    dplyr::select(all_of(cols_to_move), everything()) %>%
    group_by(code) %>%
    summarize(across(c(sample_unit_customId, sample_unit_name:conversion_factor), 
                     ~ (if (all(is.na(.x))) NA else .x[!is.na(.x)][1])), 
              .groups = "drop") %>%
    mutate(sample_unit_group = case_when(
      short_name == "‰" ~ "DimensionlessRatio",
      short_name == "mg/dscm" ~ "AirConcentration",
      short_name == "% (Recovery)" ~ "DimensionlessRatio",
      short_name == "N/A" ~ "None",
      .default = sample_unit_group
    )) %>% mutate(sample_unit_customId = case_when(
      short_name == "N/A" ~ "Unknown",
      short_name == "‰" ~ "‰",
      short_name == "mg/dscm" ~ "mg/dscm",
      .default = sample_unit_customId
    )) %>% mutate(sample_unit_name = case_when(
      short_name == "N/A" ~ "Unknown",
      short_name == "‰" ~ "Per mille (0/00 VSMOW, isotope composition)",
      short_name == "mg/dscm" ~ "Milligrams per dry standard cubic metre",
      .default = sample_unit_name
    )) %>%
    mutate(conversion_factor = if_else(short_name == "mg/dscm", 1000, conversion_factor))
  
  units_new_to_enmods <- read_excel("./utils/config/ReferenceLists/Units_new_to_enmods.xlsx", sheet = "NewUnits")
  
  # Identify the new columns from units_base
  new_cols <- setdiff(names(units), names(units_new_to_enmods))
  
  # Create a tibble of just those new columns
  new_data <- units %>%
    dplyr::select(all_of(new_cols)) %>%
    mutate(across(everything(), ~ NA)) %>%
    unique() %>% slice(rep(1, nrow(units_new_to_enmods)))
  
  units_new_to_enmods <- units_new_to_enmods %>%
    bind_cols(new_data) %>%
    mutate(sample_unit_modifier = as.character(sample_unit_modifier))
  
  units <- units %>%
    bind_rows(units_new_to_enmods) %>%
    unique()
  #
  # Replacement dictionary as a named vector
  replacements <- c("Ph units" = "pH units",
                    "hg" = "Hg",
                    "hectar" = "hectare",
                    "Count" = "Counts",
                    "Us gallons" = "US gallons",
                    "Tons" = "US tons",
                    "Kilopascal" = "Kilopascals",
                    "Micro grams per kilogram" = "Micrograms per kilogram",
                    "Microequivelents" = "Microequivalents",
                    "Milliequivalent" = "Milliequivalents",
                    "meter" = "metre",
                    "Day" = "Days",
                    "Micromole per gram" = "Micromoles per gram",
                    "Cenitmetre" = "Centimetres",
                    "Milisiemens per centimetre" = "Millisiemens per centimetre")
  
  #Sentence case in general for sample unit names
  #Accounting for special cases
  units <- units %>%
    mutate(sample_unit_name = str_to_sentence(sample_unit_name)) %>%
    mutate(sample_unit_name =
             str_replace_all(sample_unit_name, replacements)) %>%
    mutate(sample_unit_name = case_when(
      sample_unit_name == "Centimetre" ~ "Centimetres",
      sample_unit_name == "Micrometre" ~ "Micrometres",
      .default = sample_unit_name)) %>%
    mutate(description = str_replace_all(description, "Tons", "US tons"))
  
  #need to insert unit groups now
  unit_groups <- units %>%
    dplyr::select(sample_unit_group, convertible) %>%
    #group_by(across(everything())) %>%
    #summarize(Count = n()) %>%
    #ungroup() %>%
    mutate(sample_unit_group = case_when(
      sample_unit_group == "Length" ~ "SYS-REQUIRED - Length",
      .default = sample_unit_group
    )) %>% dplyr::filter(!is.na(convertible)) %>%
    unique()
  
  post_check <- post_profiles("test", "unit_groups", unit_groups)
  
  unit_groups_profiles <- get_profiles("test", "unit_groups")
  
  units <- units %>% left_join(unit_groups_profiles %>%
                                 dplyr::select(id, customId, supportsConversion),
                               by = join_by("sample_unit_group" == "customId")) %>%
    dplyr::select(-convertible) %>%
    rename(convertible = supportsConversion,
           sample_unit_groupID = id)
  
  #fixing the units file for anomalous Count group associated with No/m2
  units <- units %>%
    #mutate(convertible = ifelse(sample_unit_name == "Number per square metre",
    #                            FALSE, convertible)) %>%
    mutate(offset = if_else(is.na(offset), 0, offset)) %>%
    mutate(sample_unit_group = case_when(
      sample_unit_group == "Length" ~ "SYS-REQUIRED - Length",
      sample_unit_group == "Apperance" ~ "Appearance",
      .default = sample_unit_group
    ))
  #
  units <- units %>% mutate(convertible = if_else(is.na(convertible), FALSE, convertible)) %>% unique() %>% mutate(convertible = if_else(sample_unit_group == "SYS-REQUIRED - Length", TRUE, convertible))
  
  #Spell check things before writing
  units_spellcheck <- units %>%
    mutate(
      words = str_extract_all(description, "[A-Z][a-z]+"),  # splits CamelCase into words
      # words = strsplit(sample_unit_name, "\\s+"),  # split text into words
      misspelled = map(words, hunspell)
    )
  # # print(units_spellcheck %>% select(misspelled) %>% unnest(misspelled) %>% unlist() %>% unique(), n = 126)
  # #
  
  write_xlsx(units, "./utils/config/ReferenceLists/Consolidated_Units.xlsx")
  # Load an existing workbook
  wb <- loadWorkbook("./utils/config/ReferenceLists/Consolidated_Units.xlsx")
  # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
  renameWorksheet(wb, sheet = "Sheet1", newName = "Units")
  # Save the workbook with the updated sheet name
  saveWorkbook(wb, "./utils/config/ReferenceLists/Consolidated_Units.xlsx", 
               overwrite = TRUE)
  
}

# PREPROCESSING UNITS FOR NEW DATA ---------------------------------------------

#Only conversions in this file are reliable
units_base <- read_excel("./utils/config/ReferenceLists/Consolidated_Units.xlsx", 
                         sheet = "Units") %>% 
  mutate(code = str_replace(code, "^0+", ""))

file_exists <- length(list.files(pattern = "^Units_ems_jk_")) > 0

#if file matching pattern exists in this folder, it would be in this list
file_exists <- list.files(path = "./utils/config/ReferenceLists/", 
                             pattern = "^Units_ems_jk_", full.names = TRUE)

#ADD NEW FILE HERE; IF NO NEW FILE, CONSOLIDATED base unitS FILE WILL BE USED
#picks the latest file of all the files identified
if (length(file_exists) > 0) {
  latest_file <- file_exists[which.max(file.info(file_exists)$mtime)]
  message("Latest file found: ", latest_file)
  
  units <- read_csv(latest_file) %>% 
    rename_with(tolower) %>%
    rename_with(~ gsub("\\.", "_", .)) %>%
    mutate(meas_unit_cd = as.character(meas_unit_cd)) %>%
    mutate(code = as.character(code)) %>%
    dplyr::select(-conversion_factor) %>%
    mutate(code = str_replace(code, "^0+", ""))
  
  # Identify the new columns from units_base
  new_cols <- setdiff(names(units_base), names(units))
  
  # Create a tibble of just those new columns
  new_data <- units_base %>%
    dplyr::select(all_of(new_cols)) %>%
    mutate(across(everything(), ~ NA)) %>%
    unique() %>% slice(rep(1, nrow(units)))
  
  units <- units %>% bind_cols(new_data)
  
  units <- units %>% bind_rows(units_base %>% mutate(results = NA)) %>% unique()
  
} else {
  
  units <- units_base
  
}

# Columns to move
cols_to_move <- c("sample_unit_customId", "code", "sample_unit_name",
                  "sample_unit_short_name", "sample_unit_group",
                  "sample_unit_modifier", "results", "base unit",
                  "offset", "convertible")

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
  dplyr::filter(is.na(code)) %>%
  count() %>% unlist()

#allocating random code values to those rows so they do not get removed
set.seed(123)  # optional, for reproducibility
random_code_num <- sample(-1:-20, units_missing_code)

units$code[which(is.na(units$code))] <- random_code_num

# Reorder with selected columns at the end
units <- units %>%
  dplyr::select(all_of(cols_to_move), everything()) %>%
  group_by(code) %>%
  summarize(across(c(sample_unit_customId, sample_unit_name:sample_unit_groupID), ~ (if (all(is.na(.x))) NA else .x[!is.na(.x)][1])), 
            .groups = "drop") %>% 
  mutate(sample_unit_modifier = as.character(sample_unit_modifier)) %>% 
  mutate(convertible = if_else(is.na(convertible), FALSE, convertible)) %>%
  mutate(offset = if_else(is.na(offset), 0, offset)) %>%
  mutate(sample_unit_name = str_to_sentence(sample_unit_name)) %>%
  mutate(sample_unit_name = 
           str_replace_all(sample_unit_name, replacements)) %>%
  dplyr::filter(!is.na(sample_unit_name)) %>%
  group_by(sample_unit_name) %>%
  mutate(results = sum(results, na.rm = TRUE)) %>% 
  ungroup() %>%
  unique()

write_xlsx(units, "./utils/config/ReferenceLists/Consolidated_Units.xlsx")

# Load an existing workbook
wb <- loadWorkbook("./utils/config/ReferenceLists/Consolidated_Units.xlsx")

# Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
renameWorksheet(wb, sheet = "Sheet1", newName = "Units")

# Save the workbook with the updated sheet name
saveWorkbook(wb, "./utils/config/ReferenceLists/Consolidated_Units.xlsx", 
             overwrite = TRUE)

units <- units %>% 
  dplyr::select(conversion_factor, sample_unit_name, sample_unit_customId,
                convertible, conversion_factor, offset, sample_unit_group) %>%
  unique()

# # QA/QC for UNITS -------------------------------------------
# 
# #first post files
# post_check <- post_profiles("prod", "units", units)
# 
# # # #109 only; even though 126 in the units file - this is OK because many units
# #were merged from EMS for example % ww and % weight and % vv all become %
# get_check <- get_profiles("prod", "units")
# # #
# # # #no such units
# #units_na <- units %>% dplyr::filter(is.na(sample_unit_customId))
# # #
# units_missing <- units %>%
#   anti_join(get_check,
#             by = join_by("sample_unit_customId" == "customId"))
# 
# PREPROCESSING OF UNIT GROUPS --------------------------------------------

#list of unit groups
#sometimes this list may have one less or more unit groups than in ENV
#This is because AQS automatically creates Length if no unit groups present
#This added Unit Group may be called "SYS-REQUIRED - Length"
unit_groups <- units %>% 
  dplyr::select(sample_unit_group, convertible) %>% 
  group_by(across(everything())) %>%
  summarize(Count = n()) %>% 
  ungroup() %>%
  mutate(sample_unit_group = case_when(
    sample_unit_group == "Length" ~ "SYS-REQUIRED - Length",
    .default = sample_unit_group
  ))

write_xlsx(unit_groups, "./utils/config/ReferenceLists/Consolidated_Unit_Groups.xlsx")

# Load an existing workbook
wb <- loadWorkbook("./utils/config/ReferenceLists/Consolidated_Unit_Groups.xlsx")

# Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
renameWorksheet(wb, sheet = "Sheet1", newName = "unit_groups")

# Save the workbook with the updated sheet name
saveWorkbook(wb, "./utils/config/ReferenceLists/Consolidated_Unit_Groups.xlsx", overwrite = TRUE)

