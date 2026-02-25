preprocessing_units_unitsgroups <- function(env, run_init){

  #' Title: preprocessing_units_unitsgroups
  #' @description
  #'  Pre-processes starter files for units and unit groups
  #' @param env AQS environment. Takes values "prod" or "test"
  #' @param run_init TRUE or FALSE depending on whether running from raw files (TRUE) or consolidated (FALSE)
  #' @returns NULL
  #' @import dplyr readxl writexl openxlsx stringr readr
  #' @importFrom magrittr %>%
  #' @export
  #' @examples

  # library(tidyverse)
  # library(dplyr)
  # library(readxl)
  # library(writexl)
  # library(openxlsx)
  # library(stringr)
  # library(readr)

# FILE TO PREPROCESS UNITS and UNIT GROUPS
source("./R/api_functions.R")

# PREPROCESSING TO CONSOLIDATE OLDER UNITS FILES--------
run_init <- TRUE
  if (run_init) {

   #Only conversions in this file are reliable
  units_base <- read_excel("./inst/extdata/Reference_Lists/Units.xlsx", sheet = "Units") %>%
    rename_with(tolower) %>%
    rename_with(~ gsub("\\.", "_", .)) %>%
    mutate(meas_unit_cd = as.character(meas_unit_cd)) %>%
    mutate(code = as.character(code)) %>%
    mutate(code = str_replace(code, "^0+", "")) %>%
    mutate(code = ifelse(short_name == "None", 0, code))
    # dplyr::select(c(code, conversion_factor, offset, convertible, sample_unit_group, sample_unit_customid,
    #                 sample_unit_name, sample_unit_simplified))
    #
    #  units <- units %>% full_join(units_base, by = "code")

	  #setting the compiled units file to an initial file of units_base
  units <- units_base %>% mutate(results = NA)

  units_new_to_enmods <- read_excel("./inst/extdata/Reference_Lists/Units_new_to_enmods_2025_08_12.xlsx",
                                      sheet = "NewUnits") %>%
      rename_with(tolower) %>%
      rename_with(~ gsub("\\.", "_", .))

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

   #Bind 2 update to the units_base
    units <- units %>%
      bind_rows(units_new_to_enmods) %>%
      unique()

    #units in the alpha list but not yet added
    #Only conversions in this file are reliable
    units_new_to_enmods <- read_excel("./inst/extdata/Reference_Lists/Units_alpha_list_SB_20250624.xlsx", sheet = "Units") %>%
      rename_with(tolower) %>%
      rename_with(~ gsub("\\.", "_", .)) %>%
      mutate(meas_unit_cd = as.character(meas_unit_cd)) %>%
      mutate(code = as.character(code)) %>%
      mutate(code = str_replace(code, "^0+", "")) #%>%

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

	  #Bind 3 update to the units_base
    units <- units %>%
      bind_rows(units_new_to_enmods) %>%
      unique()

    #initialization code
    units_ems <- read_csv("./inst/extdata/Reference_Lists/Units_ems_jk_sb_2025_08_12.csv") %>%
      rename_with(tolower) %>%
      rename_with(~ gsub("\\.", "_", .)) %>%
      mutate(meas_unit_cd = as.character(meas_unit_cd)) %>%
      mutate(code = as.character(code)) %>%
      dplyr::select(-conversion_factor) %>%
      mutate(code = str_replace(code, "^0+", ""))

    #Need to use the current version of units to add the sample_unit_customid
    units_ems <- units_ems %>% left_join(units %>%
                                           dplyr::select(code, short_name, sample_unit_customid),
                                         by = join_by(code, short_name))

    # Identify the new columns from units_base
    new_cols <- setdiff(names(units_base), names(units_ems))

    # Create a tibble of just those new columns
    new_data <- units_base %>%
      dplyr::select(all_of(new_cols)) %>%
      mutate(across(everything(), ~ NA)) %>%
      unique() %>% slice(rep(1, nrow(units_ems)))

    units_ems <- units_ems %>% bind_cols(new_data) %>%
      mutate(sample_unit_modifier = as.character(sample_unit_modifier))

    #Bind 1 update to the units_base
    units <- units %>% bind_rows(units_ems) %>% unique()

  #Was ending up with deleted new units due to missing code value
  #Adding code needed to setup the parameter "code" here
    units <- units %>%
      mutate(sample_unit_customid = case_when(
      short_name == "N/A" ~ "Unknown",
      short_name == "mg/dscm" ~ "mg/dscm",
      short_name == "\u2030" ~ "‰",
      .default = sample_unit_customid
    ))

    na_groups <- units %>%
      dplyr::filter(is.na(code)) %>%
      distinct(sample_unit_customid) %>%
      mutate(random_val = sample(-200:-1, n(), replace = FALSE)) %>% # generate random values
      mutate(random_val = as.character(random_val))

    units <- units %>%
      left_join(na_groups, by = "sample_unit_customid") %>%
      mutate(
        code = if_else(is.na(code), random_val, code)
      ) %>%
      select(-random_val)  # remove helper column

    # Replacement dictionary as a named vector
    replacements <- c("Ph units" = "pH units",
                      "hg" = "Hg",
                      "Absorbance units per centimetre" = "Absorbance Units per centimetre",
                      #"Air dried tonnes per day" = "Air Dried Tonnes per day",
                      "Cenitmetre" = "Centimetres",
                      "cenitmetre" = "Centimetres",
                      #"Colony forming units per 100 ml" = "Colony Forming Units per 100 mL",
                      #"Colony forming units per ml" = "Colony Forming Units per mL",
                      #"Colony forming units per gram" = "Colony Forming Units per gram",
                      "Color units" = "Color Units",
                      "Count" =  "Counts",
                      "Counts per ml" = "Counts per mL",
                      "^Day$" =  "Days",
                      "Degrees celsius" = "Degrees Celsius",
                      "Degrees fahrenheit" = "Degrees Fahrenheit",
                      #"Imperial gallons per day" = "Imperial Gallons per day",
                      "turbidity units" = "Turbidity Units",
                      "Micro grams per kilogram" = "Micrograms per kilogram",
                      "Kilopascal" = "Kilopascals",
                      "Microequivelents per litre" = "Microequivalents per litre",
                      #"Micrograms per Litre" = "Micrograms per litre",
                      "Micromole per gram" = "Micromoles per gram",
                      "meter" = "metre",
                      "per Gram" = "per gram",
                      "milisiemens per centimetre" = "Millisiemens per centimetre",
                      "Milisiemens per centimetre" = "Millisiemens per centimetre",
                      "Milliequivalent" = "Milliequivalents",
                      #"Million imperial gallons per day" = "Million Imperial Gallons per day",
                      #"Million litres per day" = "Million Litres per day",
                      "Most probable number" = "Most Probable Number",
                      #"Nephelometric turbidity units" = "Nephelometric Turbidity Units",
                      "Number of Organisms" = "Number of organisms",
                      "Relative fluorescence units" = "Relative Fluorescence Units",
                      "Single wavelength color units" = "Single Wavelength Color Units",
                      "Tons" = "US tons",
                      "Total absorbed color units" = "Total Absorbed Color units",
                      "total organisms per sample" = "Total organisms per sample",
                      "Us gallons" = "US gallons",
                      "Us gallons per day" = "US gallons per day",
                      "Us gallons per minute" = "US gallons per minute",
                      "per Litre" = "per litre",
                      "per mL" = "per ml",
                      "per 100 mL" = "per 100 ml",
                      "per 100 Grams" = "per 100 gram",
                      "Kilograms per air dried unbleached tonne" = "Kilograms per Air Dried Unbleached Tonne")

    # Columns to move
    cols_to_move <- c("code", "sample_unit_customid", "sample_unit_name",
                      "sample_unit_simplified", "sample_unit_group",
                      "sample_unit_modifier", "results", "base unit",
                      "offset", "convertible", "conversion_factor")

    # Reorder with selected columns at the beginning
    units <- units %>%
      dplyr::select(all_of(cols_to_move), everything()) %>%
      group_by(code) %>%
      summarize(across(c(short_name, sample_unit_customid:conversion_factor, sample_unit_modifier, results),
                       ~ (if (all(is.na(.x))) NA else .x[!is.na(.x)][1])),
                .groups = "drop")  %>%
      unique() %>%
      mutate(sample_unit_group = case_when(
        short_name == "\u2030" ~ "DimensionlessRatio",
        short_name == "mg/dscm" ~ "AirConcentration",
        short_name == "% (Recovery)" ~ "DimensionlessRatio",
        short_name == "N/A" ~ "None",
        .default = sample_unit_group
      )) %>% mutate(sample_unit_name = case_when(
        short_name == "N/A" ~ "Unknown",
        short_name == "\u2030" ~ "Per mille (0/00 VSMOW, isotope composition)",
        short_name == "mg/dscm" ~ "Milligrams per dry standard cubic metre",
        sample_unit_name == "Centimetre" ~ "Centimetres",
        sample_unit_name == "Micrometre" ~ "Micrometres",
        .default = sample_unit_name
      )) %>%
      mutate(conversion_factor = case_when(short_name == "mg/dscm" ~ 1000,
                                           short_name == "\u2030" ~ 0.1,
                                           TRUE ~ conversion_factor)) %>% unique()

    #Sentence case in general for sample unit names
    #Accounting for special cases
    units <- units %>%
      mutate(sample_unit_modifier = as.character(sample_unit_modifier)) %>%
      mutate(convertible = if_else(is.na(convertible), FALSE, convertible)) %>%
      mutate(offset = if_else(is.na(offset), 0, offset)) %>%
      #mutate(sample_unit_name = str_to_sentence(sample_unit_name)) %>%
      mutate(sample_unit_name = str_replace_all(sample_unit_name, replacements)) %>%
      dplyr::filter(!is.na(sample_unit_name)) %>%
      mutate(sample_unit_name = case_when(
        sample_unit_name == "grams" ~ "Grams",
        .default = sample_unit_name
      )) %>%
      group_by(sample_unit_name) %>%
      mutate(results = sum(results, na.rm = TRUE)) %>%
      ungroup() %>%
      unique() %>%
      mutate(sample_unit_simplified = case_when(
        #sample_unit_customid == "\u2030" ~ "",
        is.na(sample_unit_simplified) ~ sample_unit_customid,
        .default = sample_unit_simplified
      )) %>%
      mutate(sample_unit_name = case_when(
        sample_unit_simplified == "" ~ sample_unit_name,
        #sample_unit_simplified == "Unknown" ~ sample_unit_name,
        #sample_unit_simplified == "Vehicles" ~ sample_unit_name,
        #sample_unit_simplified == "pH Units" ~ sample_unit_name,
        #sample_unit_simplified == "None" ~ sample_unit_name,
        .default = str_c(sample_unit_simplified, " - ", sample_unit_name),
      )) %>%
      mutate(code = case_when(
        code == "58" ~ "058",
        code == "80" ~ "080",
        code == "90" ~ "090",
        .default = code
      ))

    #fixing the units file for anomalous Count group associated with No/m2
    units <- units %>%
      mutate(sample_unit_group = case_when(
        #sample_unit_group == "Length" ~ "SYS-REQUIRED - Length",
        sample_unit_group == "Apperance" ~ "Appearance",
        .default = sample_unit_group
      ))

    #Bring in base unit group and correct convertible values in the units table if any
    unit_groups_base <- read_csv("./inst/extdata/Reference_Lists/Unit_Groups_Base.csv")

    units <- units %>% left_join(unit_groups_base,
                                 by = join_by(sample_unit_group == customId)) %>%
             dplyr::select(-convertible) %>%
             rename(convertible = supportsConversion)

    #need to insert unit groups now
	#no longer changing length to SYS-REQUIRED - Length due to the two being different
    unit_groups <- units %>%
      dplyr::select(sample_unit_group, convertible) %>%
      #group_by(across(everything())) %>%
      #summarize(Count = n()) %>%
      #ungroup() %>%
      # mutate(sample_unit_group = case_when(
      #   sample_unit_group == "Length" ~ "SYS-REQUIRED - Length",
      #   .default = sample_unit_group
      # )) %>%
      dplyr::filter(!is.na(convertible)) %>%
      group_by(sample_unit_group) %>%
      mutate(convertible = any(convertible)) %>%
      ungroup() %>%
      unique()

	write_xlsx(unit_groups, "./inst/extdata/Reference_Lists/Consolidated_Unit_Groups.xlsx")

	# Load an existing workbook
	wb <- loadWorkbook("./inst/extdata/Reference_Lists/Consolidated_Unit_Groups.xlsx")

	# Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
	renameWorksheet(wb, sheet = "Sheet1", newName = "Unit_Groups")

	# Save the workbook with the updated sheet name
	saveWorkbook(wb, "./inst/extdata/Reference_Lists/Consolidated_Unit_Groups.xlsx",
	             overwrite = TRUE)

    #post_check <- post_profiles("prod", "unit_groups", unit_groups)

    #unit_groups_profiles <- get_profiles("prod", "unit_groups")

	#this should happen in POST
    #units <- units %>% left_join(unit_groups_profiles %>%
    #                               dplyr::select(id, customId, supportsConversion),
    #                             by = join_by("sample_unit_group" == "customId")) %>%
    #  dplyr::select(-convertible) %>%
    #  rename(convertible = supportsConversion,
    #         sample_unit_groupid = id)
	#
    #units <- units %>%
    #  mutate(convertible = if_else(is.na(convertible), FALSE, convertible)) %>%
    #  unique() %>%
    #  mutate(convertible = if_else(sample_unit_group == "SYS-REQUIRED - Length",
    #                               TRUE, convertible)) %>% unique()

    # #Spell check things before writing
    # units_spellcheck <- units %>%
    #   mutate(
    #     words = str_extract_all(description, "[A-Z][a-z]+"),  # splits CamelCase into words
    #     # words = strsplit(sample_unit_name, "\\s+"),  # split text into words
    #     misspelled = map(words, hunspell)
    #   )
    # # print(units_spellcheck %>% select(misspelled) %>% unnest(misspelled) %>% unlist() %>% unique(), n = 126)

    write_xlsx(units, "./inst/extdata/Reference_Lists/Consolidated_Units.xlsx")
    # Load an existing workbook
    wb <- loadWorkbook("./inst/extdata/Reference_Lists/Consolidated_Units.xlsx")
    # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
    renameWorksheet(wb, sheet = "Sheet1", newName = "Units")
    # Save the workbook with the updated sheet name
    saveWorkbook(wb, "./inst/extdata/Reference_Lists/Consolidated_Units.xlsx",
                 overwrite = TRUE)
  }

# PREPROCESSING UNITS FOR NEW DATA ---------------------------------------------

# run_new <- FALSE
#
# #Only conversions in this file are reliable
# units_base <- read_excel("./inst/extdata/Reference_Lists/Consolidated_Units.xlsx",
#                          sheet = "Units") %>%
#   rename_with(tolower) %>%
#   rename_with(~ gsub("\\.", "_", .)) %>%
#   mutate(code = str_replace(code, "^0+", ""))
#
# run_new <- length(list.files(pattern = "^Units_ems_jk_")) > 0
#
# #ADD NEW FILE HERE; IF NO NEW FILE, CONSOLIDATED BASE UNITS FILE WILL BE USED
# #picks the latest file of all the files identified
# if (run_new) {
#   #if file matching pattern exists in this folder, it would be in this list
#   file_exists <- list.files(path = "./inst/extdata/Reference_Lists/",
#                             pattern = "^Units_ems_jk_", full.names = TRUE)
#
#   latest_file <- file_exists[which.max(file.info(file_exists)$mtime)]
#   message("Latest file found: ", latest_file)
#
#   units <- read_csv(latest_file) %>%
#     rename_with(tolower) %>%
#     rename_with(~ gsub("\\.", "_", .)) %>%
#     mutate(meas_unit_cd = as.character(meas_unit_cd)) %>%
#     mutate(code = as.character(code)) %>%
#     rename(conversion_factor_backup = conversion_factor) %>%
#     dplyr::select(-conversion_factor) %>%
#     left_join(units_base %>% dplyr::select(code, conversion_factor), by = join_by(code)) %>%
#     mutate(conversion_factor = ifelse(is.na(conversion_factor), conversion_factor_backup, conversion_factor)) %>%
#     dplyr::select(-conversion_factor_backup) %>%
#     mutate(code = str_replace(code, "^0+", ""))
#
#   # Identify the new columns from units_base
#   new_cols <- setdiff(names(units_base), names(units))
#
#   # Create a tibble of just those new columns
#   new_data <- units_base %>%
#     dplyr::select(all_of(new_cols)) %>%
#     mutate(across(everything(), ~ NA)) %>%
#     unique() %>% slice(rep(1, nrow(units)))
#
#   units <- units %>% bind_cols(new_data)
#
#   # Columns to move
#   cols_to_move <- c("sample_unit_customid", "code", "sample_unit_name",
#                     "sample_unit_simplified", "sample_unit_group",
#                     "sample_unit_modifier", "results", "base unit",
#                     "offset", "convertible")
#
#   # Count the number of units with NA in code; these are units not in EMS
#   units_missing_code <- units %>%
#     dplyr::filter(is.na(code)) %>%
#     count() %>% unlist()
#
#   #allocating random code values to those rows so they do not get removed
#   set.seed(123)  # optional, for reproducibility
#   random_code_num <- sample(-1:-100, units_missing_code, replace = FALSE)
#
#   units$code[which(is.na(units$code))] <- random_code_num
#
#   # Replacement dictionary as a named vector
#   replacements <- c("Ph units" = "pH units",
#                     "hg" = "Hg",
#                     "Absorbance units per centimetre" = "Absorbance Units per centimetre",
#                     "Air dried tonnes per day" = "Air Dried Tonnes per day",
#                     "Cenitmetre" = "Centimetres",
#                     "cenitmetre" = "Centimetres",
#                     "Colony forming units per 100 ml" = "Colony Forming Units per 100 mL",
#                     "Colony forming units per ml" = "Colony Forming Units per mL",
#                     "Colony forming units per gram" = "Colony Forming Units per gram",
#                     "Color units" = "Color Units",
#                     "Count" =  "Counts",
#                     "Counts per ml" = "Counts per mL",
#                     "Day" =  "Days",
#                     "Degrees celsius" = "Degrees Celsius",
#                     "Degrees fahrenheit" = "Degrees Fahrenheit",
#                     "Imperial gallons per day" = "Imperial Gallons per day",
#                     "Jackson turbidity units" = "Jackson Turbidity Units",
#                     "Micro grams per kilogram" = "Micrograms per kilogram",
#                     "Kilopascal" = "Kilopascals",
#                     "Microequivelents per litre" = "Microequivalents per litre",
#                     "Micromole per gram" = "Micromoles per gram",
#                     "meter" = "metre",
#                     "Milisiemens per centimetre" = "Millisiemens per centimetre",
#                     "Milliequivalent" = "Milliequivalents",
#                     "Million imperial gallons per day" = "Million Imperial Gallons per day",
#                     "Million litres per day" = "Million Litres per day",
#                     "Most probable number per gram" = "Most Probable Number per gram",
#                     "Most probable number per 100 grams" = "Most Probable Number per 100 grams",
#                     "Most probable number per 100 ml" = "Most Probable Number per 100 mL",
#                     "Nephelometric turbidity units" = "Nephelometric Turbidity Units",
#                     "Number of organisms" = "Number of Organisms",
#                     "Relative fluorescence units" = "Relative Fluorescence Units",
#                     "Single wavelength color units" = "Single Wavelength Color Units",
#                     "Tons" = "US Tons",
#                     "Total absorbed color units" = "Total Absorbed Color units",
#                     "Us gallons" = "US Gallons",
#                     "Us gallons per day" = "US Gallons per day",
#                     "Us gallons per minute" = "US Gallons per minute")
#
#
#   # Reorder with selected columns at the end
#   units <- units %>%
#     dplyr::select(all_of(cols_to_move), everything()) %>%
#     group_by(code) %>%
#     summarize(across(c(sample_unit_customid, sample_unit_name:conversion_factor),
#                      ~ (if (all(is.na(.x))) NA else .x[!is.na(.x)][1])),
#               .groups = "drop") %>%
#     mutate(sample_unit_group = case_when(
#       short_name == "\u2030" ~ "DimensionlessRatio",
#       short_name == "mg/dscm" ~ "AirConcentration",
#       short_name == "% (Recovery)" ~ "DimensionlessRatio",
#       short_name == "N/A" ~ "None",
#       .default = sample_unit_group
#     )) %>% mutate(sample_unit_customid = case_when(
#       short_name == "N/A" ~ "Unknown",
#       short_name == "\u2030" ~ "‰",
#       short_name == "mg/dscm" ~ "mg/dscm",
#       .default = sample_unit_customid
#     )) %>% mutate(sample_unit_name = case_when(
#       short_name == "N/A" ~ "Unknown",
#       short_name == "\u2030" ~ "Per mille (0/00 VSMOW, isotope composition)",
#       short_name == "mg/dscm" ~ "Milligrams per dry standard cubic metre",
#       sample_unit_name == "Centimetre" ~ "Centimetres",
#       sample_unit_name == "Micrometre" ~ "Micrometres",
#       .default = sample_unit_name
#     )) %>%
#     mutate(sample_unit_modifier = as.character(sample_unit_modifier)) %>%
#     mutate(convertible = if_else(is.na(convertible), FALSE, convertible)) %>%
#     mutate(offset = if_else(is.na(offset), 0, offset)) %>%
#     mutate(sample_unit_name = str_to_sentence(sample_unit_name)) %>%
#     mutate(sample_unit_name =
#              str_replace_all(sample_unit_name, replacements)) %>%
#     dplyr::filter(!is.na(sample_unit_name)) %>%
#     mutate(sample_unit_name = case_when(
#       sample_unit_name == "grams" ~ "Grams",
#       .default = sample_unit_name
#     )) %>%
#     group_by(sample_unit_name) %>%
#     mutate(results = sum(results, na.rm = TRUE)) %>%
#     ungroup() %>%
#     unique() %>%
#     mutate(sample_unit_simplified = case_when(
#       sample_unit_customid == "\u2030" ~ "",
#       is.na(sample_unit_simplified) ~ sample_unit_customid,
#       .default = sample_unit_simplified
#     )) %>%
#     mutate(sample_unit_name = case_when(
#       sample_unit_simplified == "" ~ sample_unit_name,
#       sample_unit_simplified == "Unknown" ~ sample_unit_name,
#       sample_unit_simplified == "Vehicles" ~ sample_unit_name,
#       sample_unit_simplified == "pH Units" ~ sample_unit_name,
#       sample_unit_simplified == "None" ~ sample_unit_name,
#       .default = str_c(sample_unit_simplified, " - ", sample_unit_name),
#     ))
#
#   units <- units %>% bind_rows(units_base %>% mutate(results = NA)) %>% unique()
#
# } else {
#
#   units <- units_base
#
# }
#
# write_xlsx(units, "./inst/extdata/Reference_Lists/Consolidated_Units.xlsx")
#
# # Load an existing workbook
# wb <- loadWorkbook("./inst/extdata/Reference_Lists/Consolidated_Units.xlsx")
#
# # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
# renameWorksheet(wb, sheet = "Sheet1", newName = "Units")
#
# # Save the workbook with the updated sheet name
# saveWorkbook(wb, "./inst/extdata/Reference_Lists/Consolidated_Units.xlsx",
#              overwrite = TRUE)
#
# units <- units %>%
#   dplyr::select(sample_unit_name, sample_unit_customid,
#                 convertible, conversion_factor, offset, sample_unit_group) %>%
#   unique()

# PREPROCESSING OF UNIT GROUPS --------------------------------------------

# #list of unit groups
# #sometimes this list may have one less or more unit groups than in ENV
# #This is because AQS automatically creates Length if no unit groups present
# #This added Unit Group may be called "SYS-REQUIRED - Length"
# unit_groups <- units %>%
#   dplyr::select(sample_unit_group, convertible) %>%
#   group_by(across(everything())) %>%
#   summarize(Count = n()) %>%
#   ungroup() #`%>%
#   # mutate(sample_unit_group = case_when(
#   #   sample_unit_group == "Length" ~ "SYS-REQUIRED - Length",
#   #   .default = sample_unit_group
#   # ))

#write_xlsx(unit_groups, "./inst/extdata/Reference_Lists/Consolidated_Unit_Groups.xlsx")

## Load an existing workbook
#wb <- loadWorkbook("./inst/extdata/Reference_Lists/Consolidated_Unit_Groups.xlsx")

## Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
#renameWorksheet(wb, sheet = "Sheet1", newName = "Unit_Groups")

# Save the workbook with the updated sheet name
#saveWorkbook(wb, "./inst/extdata/Reference_Lists/Consolidated_Unit_Groups.xlsx", overwrite = TRUE)

}
