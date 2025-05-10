# FILE TO PREPROCESS UNITS and UNIT GROUPS
source("./utils/config/api_functions.R")

# PREPROCESSING TO CONSOLIDATE OLDER UNITS FILES--------
run_init <- FALSE
if (run_init) {
  
  # initialization code
  units <- read_csv("./utils/config/ReferenceLists/Units_ems_jk_2025_04_16.csv") %>% mutate(MEAS_UNIT_CD = as.character(MEAS_UNIT_CD)) %>%
    mutate(CODE = as.character(CODE)) %>%
    dplyr::select(-CONVERSION_FACTOR) %>%
    mutate(CODE = str_replace(CODE, "^0+", ""))
  #Only conversions in this file are reliable
  units_base <- read_excel("./utils/config/ReferenceLists/Units.xlsx", sheet = "Units") %>% mutate(CODE = str_replace(CODE, "^0+", ""))
  #%>% dplyr::select(c(CODE, CONVERSION_FACTOR, OFFSET, Sample.Unit.Group, Sample.Unit.CustomId, Sample.Unit.Name))
  
  # Identify the new columns from units_base
  new_cols <- setdiff(names(units_base), names(units))
  
  # Create a tibble of just those new columns
  new_data <- units_base %>%
    dplyr::select(all_of(new_cols)) %>%
    mutate(across(everything(), ~ NA)) %>%
    unique() %>% slice(rep(1, nrow(units)))
  
  units <- units %>% bind_cols(new_data)
  
  units <- units %>% bind_rows(units_base %>% mutate(Results = NA)) %>% unique()
  
  # Columns to move
  cols_to_move <- c("CODE", "Sample.Unit.CustomId", "Sample.Unit.Name",
                    "Sample.Unit.Short.Name", "Sample.Unit.Group",
                    "Sample.Unit.Modifier", "Results", "Base Unit",
                    "OFFSET", "Convertible")
  
  # Reorder with selected columns at the end
  units <- units %>%
    dplyr::select(all_of(cols_to_move), everything()) %>%
    group_by(CODE) %>%
    summarize(across(c(Sample.Unit.CustomId, Sample.Unit.Name:CONVERSION_FACTOR), ~ (if (all(is.na(.x))) NA else .x[!is.na(.x)][1])), .groups = "drop") %>%
    mutate(Sample.Unit.Group = case_when(
      SHORT_NAME == "‰" ~ "DimensionlessRatio",
      SHORT_NAME == "mg/dscm" ~ "AirConcentration",
      SHORT_NAME == "% (Recovery)" ~ "DimensionlessRatio",
      SHORT_NAME == "N/A" ~ "None",
      .default = Sample.Unit.Group
    )) %>% mutate(Sample.Unit.CustomId = case_when(
      SHORT_NAME == "N/A" ~ "Unknown",
      SHORT_NAME == "‰" ~ "‰",
      SHORT_NAME == "mg/dscm" ~ "mg/dscm",
      .default = Sample.Unit.CustomId
    )) %>% mutate(Sample.Unit.Name = case_when(
      SHORT_NAME == "N/A" ~ "Unknown",
      SHORT_NAME == "‰" ~ "Per mille (0/00 VSMOW, isotope composition)",
      SHORT_NAME == "mg/dscm" ~ "Milligrams per dry standard cubic metre",
      .default = Sample.Unit.Name
    )) %>%
    mutate(CONVERSION_FACTOR = if_else(SHORT_NAME == "mg/dscm", 1000, CONVERSION_FACTOR))
  
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
    mutate(Sample.Unit.Modifier = as.character(Sample.Unit.Modifier))
  
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
    mutate(Sample.Unit.Name = str_to_sentence(Sample.Unit.Name)) %>%
    mutate(Sample.Unit.Name =
             str_replace_all(Sample.Unit.Name, replacements)) %>%
    mutate(Sample.Unit.Name = case_when(
      Sample.Unit.Name == "Centimetre" ~ "Centimetres",
      Sample.Unit.Name == "Micrometre" ~ "Micrometres",
      .default = Sample.Unit.Name)) %>%
    mutate(DESCRIPTION = str_replace_all(DESCRIPTION, "Tons", "US tons"))
  
  #need to insert unit groups now
  unitGroups <- units %>%
    dplyr::select(Sample.Unit.Group, Convertible) %>%
    #group_by(across(everything())) %>%
    #summarize(Count = n()) %>%
    #ungroup() %>%
    mutate(Sample.Unit.Group = case_when(
      Sample.Unit.Group == "Length" ~ "SYS-REQUIRED - Length",
      .default = Sample.Unit.Group
    )) %>% dplyr::filter(!is.na(Convertible)) %>%
    unique()
  
  post_check <- post_profiles("test", "unitgroups", unitGroups)
  
  unitGroupsProfiles <- get_profiles("test", "unitgroups")
  
  units <- units %>% left_join(unitGroupsProfiles %>%
                                 dplyr::select(id, customId, supportsConversion),
                               by = join_by("Sample.Unit.Group" == "customId")) %>%
    dplyr::select(-Convertible) %>%
    rename(Convertible = supportsConversion,
           Sample.Unit.GroupID = id)
  
  #fixing the units file for anomalous Count group associated with No/m2
  units <- units %>%
    #mutate(Convertible = ifelse(Sample.Unit.Name == "Number per square metre",
    #                            FALSE, Convertible)) %>%
    mutate(OFFSET = if_else(is.na(OFFSET), 0, OFFSET)) %>%
    mutate(Sample.Unit.Group = case_when(
      Sample.Unit.Group == "Length" ~ "SYS-REQUIRED - Length",
      Sample.Unit.Group == "Apperance" ~ "Appearance",
      .default = Sample.Unit.Group
    ))
  #
  units <- units %>% mutate(Convertible = if_else(is.na(Convertible), FALSE, Convertible)) %>% unique() %>% mutate(Convertible = if_else(Sample.Unit.Group == "SYS-REQUIRED - Length", TRUE, Convertible))
  
  #Spell check things before writing
  units_spellcheck <- units %>%
    mutate(
      words = str_extract_all(DESCRIPTION, "[A-Z][a-z]+"),  # splits CamelCase into words
      # words = strsplit(Sample.Unit.Name, "\\s+"),  # split text into words
      misspelled = map(words, hunspell)
    )
  # # print(units_spellcheck %>% select(misspelled) %>% unnest(misspelled) %>% unlist() %>% unique(), n = 126)
  # #
  
  write_xlsx(units, "./utils/config/ReferenceLists/Consolidated_units.xlsx")
  # Load an existing workbook
  wb <- loadWorkbook("./utils/config/ReferenceLists/Consolidated_units.xlsx")
  # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
  renameWorksheet(wb, sheet = "Sheet1", newName = "Units")
  # Save the workbook with the updated sheet name
  saveWorkbook(wb, "./utils/config/ReferenceLists/Consolidated_units.xlsx", 
               overwrite = TRUE)
  
}

# PREPROCESSING UNITS FOR NEW DATA ---------------------------------------------

#Only conversions in this file are reliable
units_base <- read_excel("./utils/config/ReferenceLists/consolidatedUnits.xlsx", 
                         sheet = "Units") %>% 
  mutate(CODE = str_replace(CODE, "^0+", ""))

file_exists <- length(list.files(pattern = "^Units_ems_jk_")) > 0

#if file matching pattern exists in this folder, it would be in this list
file_exists <- list.files(path = "./utils/config/ReferenceLists/", 
                             pattern = "^Units_ems_jk_", full.names = TRUE)

#ADD NEW FILE HERE; IF NO NEW FILE, CONSOLIDATED BASE UNITS FILE WILL BE USED
#picks the latest file of all the files identified
if (length(file_exists) > 0) {
  latest_file <- file_exists[which.max(file.info(file_exists)$mtime)]
  message("Latest file found: ", latest_file)
  
  units <- read_csv(latest_file) %>% 
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
saveWorkbook(wb, "./utils/config/ReferenceLists/consolidatedUnits.xlsx", 
             overwrite = TRUE)

units <- units %>% 
  dplyr::select(CONVERSION_FACTOR, Sample.Unit.Name, Sample.Unit.CustomId,
                Convertible, CONVERSION_FACTOR, OFFSET, Sample.Unit.Group) %>%
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
# #units_na <- units %>% dplyr::filter(is.na(Sample.Unit.CustomId))
# # #
# units_missing <- units %>%
#   anti_join(get_check,
#             by = join_by("Sample.Unit.CustomId" == "customId"))
# 
# PREPROCESSING OF UNIT GROUPS --------------------------------------------

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

