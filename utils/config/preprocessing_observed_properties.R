preprocessing_observed_properties <- function(env, run_init) {
  #' Title: preprocessing_observed_properties
  #' @description
  #'  Takes older observed property files and consolidates them with new files
  #' @param env AQS environment. Takes values "prod" or "test"
  #' @param run_init TRUE or FALSE depending on whether running from raw files (TRUE) or consolidated (FALSE)
  #' @returns a profile in a data frame/tibble format
  #' @import stringr dplyr tidyverse PubChemR
  #' @importFrom magrittr %>%
  #' @export
  #' @examples

  # FILE TO PREPROCESS OBSERVED PROPERTIES
  source("./R/api_functions.R")

  # # Function to safely get synonyms from PubChem using get_synonym
  # get_synonyms_safe <- function(param) {
  #   tryCatch(
  #     {
  #       syn_data <- get_synonyms(identifier = param, namespace = "name") %>%
  #         unlist() %>%
  #         head(15) %>%
  #         keep(~ str_count(.x, "-") == 2 && !str_detect(.x, "[A-Za-z]")) %>%
  #         first()
  #
  #       if (is.null(syn_data)) {
  #         return(NA)
  #       }
  #
  #       # Collapse synonyms into a single semicolon-separated string
  #       paste(unique(syn_data), collapse = "; ")
  #     },
  #     error = function(e) {
  #       message(paste("Error for:", param, "->", e$message))
  #       return(NA)
  #     }
  #   )
  #   # return(syn_data)
  # }

  # PREPROCESSING TO CONSOLIDATE OLDER OP FILES -----------------------------

  # run_init <- TRUE
  if (run_init) {
    # Need to keep Excel versions as sub- and superscripts are only accessible in that format
    # Need to use CSVs so leading zeros are preserved in parm and method code
    # EMS exported observed_properties for when the OPs are used at least once (results>0) or
    # EMS exported OPs were newly created at the time of EMS to EnMoDS shift
    observed_properties <- readxl::read_excel("./inst/extdata/Reference_Lists/EMS_base_OP_20250814.xlsx",
      col_types = c("text", "guess", "text", rep("guess", 14))
    ) %>%
      dplyr::rename_with(tolower) %>%
      dplyr::rename_with(~ gsub("\\.", "_", .)) %>%
      dplyr::rename_with(~ gsub(" ", "_", .)) %>%
      # mutate(across(c(first_used, last_used, when_created, when_updated),
      #               ~ lubridate::parse_date_time(., orders = c("mdy HMS p", "mdy")))) %>%
      #              ~ as.POSIXct(., format = "%m/%d/%Y %H:%M:%S"))) %>%
      dplyr::filter(results > 0 | when_created >= as.POSIXct("2023-01-01")) %>%
      mutate(parameter_clean = parameter %>%
        str_replace_all(",", ";")) %>%
      mutate(parameter_clean = case_when(
        parameter == "CAS RN: 101200-48-0 Tribenuron-methyl" ~ "Tribenuron-methyl CAS RN: 101200-48-0",
        parameter == "CAS RN: 32598-13-3 (3;3';4;4'-tetrachlorobiphenyl)" ~ "3;3';4;4'-tetrachlorobiphenyl CAS RN: 32598-13-3",
        parameter == "CAS RN: 32598-14-4 (2;3;3';4;4'-pentachlorobiphenyl)" ~ "2;3;3';4;4'-pentachlorobiphenyl CAS RN: 32598-14-4",
        parameter == "CAS RN: 32774-16-6 (3;3';4;4';5;5'-hexachlorobiphenyl)" ~ "3;3';4;4';5;5'-hexachlorobiphenyl CAS RN: 32774-16-6",
        parameter == "CAS RN: 57465-28-8 (3;3';4;4';5-pentachlorobiphenyl)" ~ "3;3';4;4';5-pentachlorobiphenyl CAS RN: 57465-28-8",
        parameter == "CAS RN: 74051-80-2 Sethoxydim" ~ "Sethoxydim CAS RN: 74051-80-2",
        parameter == "CAS RN: 74223-64-6 Metsulfuron-methyl" ~ "Metsulfuron-methyl CAS RN: 74223-64-6",
        parameter == "CAS RN: 79277-27-3 Thifensulfuron-methyl" ~ "Thifensulfuron-methyl CAS RN: 79277-27-3",
        parameter == "CAS RN: 81405-85-8 Imazamethabenz-methyl" ~ "Imazamethabenz-methyl CAS RN: 81405-85-8",
        parameter == "CAS RN: 87820-88-0 Tralkoxydim" ~ "Tralkoxydim CAS RN: 87820-88-0",
        parameter == "CAS RN: 95266-40-3 Trinexapac-ethyl" ~ "Trinexapac-ethyl CAS RN: 95266-40-3",
        .default = parameter_clean
      )) %>%
      mutate(
        method_code_clean = str_replace(method_code, "^0+", ""),
        parm_code_clean = str_replace(parm_code, "^0+", "")
      ) %>%
      mutate(cas = str_extract(parameter_clean, "(?:,\\s*|\\s+)CAS.*")) %>%
      mutate(cas = cas %>%
        str_replace_all("(?i)(,\\s*)?CAS(:\\s*| RN:\\s*|\\s*)", "") %>% # remove CAS variants
        str_replace_all("(?i)\\band\\b", ";") %>% # replace 'and' with ;
        str_replace_all(",", ";") %>%
        str_replace("^ ", "") %>%
        str_replace_all(" ", ";") %>%
        str_replace_all(";{1,}", ";") %>%
        str_replace_all(";", "; ") %>%
        str_replace_all("[ \t]+", " ")) %>% # remove all whitespace)
      mutate(parameter_clean = parameter_clean %>%
        str_replace_all("\\(", " (") %>% # add space before (
        str_replace_all("\\(CAS", "CAS") %>%
        str_replace_all("(?<![A-Za-z])CAS", " CAS") %>%
        str_replace_all("[ \t]+", " ") %>% # collapse multiple spaces/tabs into one
        str_replace_all("(,)? CAS.*", "") %>%
        str_replace_all("\\?", "") %>% # remove all question marks
        str_replace_all("[[:space:][:punct:]&&[^)\\]\\}]]+$", "") %>%
        str_trim()) %>%
      mutate(method_clean = method %>%
        str_replace_all("\\(", " (") %>% # add space before (
        str_replace_all("\\(CAS", "CAS") %>%
        str_replace_all("(?<![A-Za-z])CAS", " CAS") %>%
        str_replace_all("[ \t]+", " ") %>% # collapse multiple spaces/tabs into one
        str_replace_all("(,)? CAS.*", "") %>%
        str_replace_all("\\?", "") %>% # remove all question marks
        str_replace_all("[[:space:][:punct:]&&[^)\\]\\}]]+$", "") %>%
        str_trim()) %>%
      mutate(parm_code = case_when(
        str_detect(parm_code, "^[0-9]{1,3}$") ~ str_pad(parm_code, width = 4, pad = "0"),
        TRUE ~ parm_code
      )) %>%
      mutate(method_code = case_when(
        str_detect(method_code, "^[0-9]{1,3}$") ~ str_pad(method_code, width = 4, pad = "0"),
        TRUE ~ method_code
      ))

    # observed_properties_check <- observed_properties %>%
    #   dplyr::select(method_code, method, method_description) %>%
    #   dplyr::filter(method_code == "E540") %>%
    #   unique()

    # observed_properties_parm_method_code <-
    #   read_csv("./inst/extdata/Reference_Lists/EMS_base_OP_20250715.csv") %>%
    #   dplyr::rename_with(tolower) %>%
    #   dplyr::rename_with(~ gsub("\\.", "_", .)) %>%
    #   dplyr::rename_with(~ gsub(" ", "_", .)) %>%
    #   mutate(across(c(first_used, last_used, when_created, when_updated),
    #                 ~ lubridate::parse_date_time(., orders = c("mdy HMS p", "mdy")))) %>%
    #   #              ~ as.POSIXct(., format = "%m/%d/%Y %H:%M:%S"))) %>%
    #   dplyr::filter(results>0 | when_created >= as.POSIXct("2023-01-01")) %>%
    #   mutate(parameter_clean = parameter %>%
    #            str_replace_all(",", ";")) %>%
    #   mutate(parameter_clean = case_when(
    #     parameter == "CAS RN: 101200-48-0 Tribenuron-methyl" ~ "Tribenuron-methyl CAS RN: 101200-48-0",
    #     parameter == "CAS RN: 32598-13-3 (3;3';4;4'-tetrachlorobiphenyl)" ~ "3;3';4;4'-tetrachlorobiphenyl CAS RN: 32598-13-3",
    #     parameter == "CAS RN: 32598-14-4 (2;3;3';4;4'-pentachlorobiphenyl)" ~ "2;3;3';4;4'-pentachlorobiphenyl CAS RN: 32598-14-4",
    #     parameter == "CAS RN: 32774-16-6 (3;3';4;4';5;5'-hexachlorobiphenyl)" ~ "3;3';4;4';5;5'-hexachlorobiphenyl CAS RN: 32774-16-6",
    #     parameter == "CAS RN: 57465-28-8 (3;3';4;4';5-pentachlorobiphenyl)" ~ "3;3';4;4';5-pentachlorobiphenyl CAS RN: 57465-28-8",
    #     parameter == "CAS RN: 74051-80-2 Sethoxydim" ~ "Sethoxydim CAS RN: 74051-80-2",
    #     parameter == "CAS RN: 74223-64-6 Metsulfuron-methyl" ~ "Metsulfuron-methyl CAS RN: 74223-64-6",
    #     parameter == "CAS RN: 79277-27-3 Thifensulfuron-methyl" ~ "Thifensulfuron-methyl CAS RN: 79277-27-3",
    #     parameter == "CAS RN: 81405-85-8 Imazamethabenz-methyl" ~ "Imazamethabenz-methyl CAS RN: 81405-85-8",
    #     parameter == "CAS RN: 87820-88-0 Tralkoxydim" ~ "Tralkoxydim CAS RN: 87820-88-0",
    #     parameter == "CAS RN: 95266-40-3 Trinexapac-ethyl" ~ "Trinexapac-ethyl CAS RN: 95266-40-3",
    #     .default = parameter_clean
    #   )) %>%
    #   mutate(method_code_clean = str_replace(method_code, "^0+", ""),
    #          parm_code_clean = str_replace(parm_code, "^0+", "")) %>%
    #   mutate(cas = str_extract(parameter_clean, "(?:,\\s*|\\s+)CAS.*")) %>%
    #   mutate(cas = cas %>%
    #            str_replace_all("(?i)(,\\s*)?CAS(:\\s*| RN:\\s*|\\s*)", "") %>%  # remove CAS variants
    #            str_replace_all("(?i)\\band\\b", ";") %>%                        # replace 'and' with ;
    #            str_replace_all("," , ";") %>%
    #            str_replace("^ ", "") %>%
    #            str_replace_all(" ", ";") %>%
    #            str_replace_all(";{1,}", ";") %>%
    #            str_replace_all(";", "; ") %>%
    #            str_replace_all("[ \t]+", " ")) %>%                                           # remove all whitespace)
    #   mutate(parameter_clean = parameter_clean %>%
    #            str_replace_all("\\(", " (") %>%               # add space before (
    #            str_replace_all("\\(CAS", "CAS") %>%
    #            str_replace_all("(?<![A-Za-z])CAS", " CAS") %>%
    #            str_replace_all("[ \t]+", " ") %>%             # collapse multiple spaces/tabs into one
    #            str_replace_all("(,)? CAS.*", "") %>%
    #            str_replace_all("\\?", "") %>%                 # remove all question marks
    #            str_replace_all("[[:space:][:punct:]&&[^)\\]\\}]]+$", "") %>%
    #            str_trim()) %>%
    #   mutate(method_clean = method %>%
    #            str_replace_all("\\(", " (") %>%               # add space before (
    #            str_replace_all("\\(CAS", "CAS") %>%
    #            str_replace_all("(?<![A-Za-z])CAS", " CAS") %>%
    #            str_replace_all("[ \t]+", " ") %>%             # collapse multiple spaces/tabs into one
    #            str_replace_all("(,)? CAS.*", "") %>%
    #            str_replace_all("\\?", "") %>%                 # remove all question marks
    #            str_replace_all("[[:space:][:punct:]&&[^)\\]\\}]]+$", "") %>%
    #            str_trim()) %>%
    #   dplyr::select(parm_code, parm_code_clean, parameter_clean,
    #                 method_code, method_code_clean, method_clean)

    # observed_properties <- observed_properties %>%
    #   rename(parm_code_backup = parm_code,
    #          method_code_backup = method_code) %>%
    #   left_join(observed_properties_parm_method_code,
    #             by = join_by("parm_code_clean" == "parm_code_clean",
    #                          "method_code_clean" == "method_code_clean",
    #                          "parameter_clean" == "parameter_clean",
    #                          "method_clean" == "method_clean")) %>%
    #   mutate(parm_code = ifelse(is.na(parm_code)|parm_code == "",
    #                             parm_code_backup, parm_code),
    #          method_code = ifelse(is.na(method_code)|method_code == "",
    #                               method_code_backup,
    #                               method_code)) %>%
    #   dplyr::select(-c(parm_code_backup, method_code_backup))

    # need to bring the corrupted file in and use its columns where I can
    # Corrupted EMS exported observed_properties
    # observed_properties_old <- readxl::read_excel("./inst/extdata/Reference_Lists/Observed_Properties_ems_jk_2025-04-22.xlsx",
    #                                                   col_types = c(rep("guess", 30), "text")) %>%

    observed_properties_old <- read_csv("./inst/extdata/Reference_Lists/Observed_Properties_ems_jk_2025-04-22.csv") %>%
      dplyr::rename_with(tolower) %>%
      dplyr::rename_with(~ gsub("\\.", "_", .)) %>%
      mutate(parameter_clean = parameter %>%
        str_replace_all(",", ";")) %>%
      mutate(newnameid = newnameid %>%
        # str_replace_all("\\(", " (") %>%               # add space before (
        str_replace_all("[ \t]+", " ") %>% # collapse multiple spaces/tabs into one
        str_replace_all("\\?", "") %>% # remove all question marks
        str_replace_all("[[:space:][:punct:]&&[^)\\]\\}]]+$", "") %>%
        str_trim()) %>% # remove leading/trailing spaces
      mutate(
        method_code_clean = str_replace(method_code, "^0+", ""),
        parm_code_clean = str_replace(parm_code, "^0+", "")
      ) %>%
      mutate(cas = ifelse(is.na(cas), str_extract(parameter, "(?:,\\s*|\\s+)CAS.*"), cas)) %>%
      mutate(cas = cas %>%
        str_replace_all("(?i)(,\\s*)?CAS(:\\s*| RN:\\s*|\\s*)", "") %>% # remove CAS variants
        str_replace_all("(?i)\\band\\b", ";") %>% # replace 'and' with ;
        str_replace_all(",", ";") %>%
        str_replace("^ ", "") %>%
        str_replace_all(" ", ";") %>%
        str_replace_all(";{1,}", ";") %>%
        str_replace_all(";", "; ") %>%
        str_replace_all("[ \t]+", " ")) %>% # remove all whitespace)
      mutate(parameter_clean = parameter %>%
        str_replace_all("\\(", " (") %>% # add space before (
        str_replace_all("\\(CAS", "CAS") %>%
        str_replace_all("(?<![A-Za-z])CAS", " CAS") %>%
        str_replace_all("[ \t]+", " ") %>% # collapse multiple spaces/tabs into one
        str_replace_all("\\?", "") %>% # remove all question marks
        str_replace_all("(,)? CAS.*", "") %>%
        str_replace_all("[[:space:][:punct:]&&[^)\\]\\}]]+$", "") %>%
        str_trim()) %>%
      mutate(method_clean = method %>%
        str_replace_all("\\(", " (") %>% # add space before (
        str_replace_all("\\(CAS", "CAS") %>%
        str_replace_all("(?<![A-Za-z])CAS", " CAS") %>%
        str_replace_all("[ \t]+", " ") %>% # collapse multiple spaces/tabs into one
        str_replace_all("(,)? CAS.*", "") %>%
        str_replace_all("\\?", "") %>% # remove all question marks
        str_replace_all("[[:space:][:punct:]&&[^)\\]\\}]]+$", "") %>%
        str_trim()) %>%
      rename(mdl_backup = mdl) %>%
      mutate(parm_code = case_when(
        str_detect(parm_code, "^[0-9]{1,3}$") ~ str_pad(parm_code, width = 4, pad = "0"),
        TRUE ~ parm_code
      )) %>%
      mutate(method_code = case_when(
        str_detect(method_code, "^[0-9]{1,3}$") ~ str_pad(method_code, width = 4, pad = "0"),
        TRUE ~ method_code
      )) %>%
      dplyr::mutate(op_group = stringr::str_replace_all(op_group, ";", "")) %>%
      mutate(op_group = case_when(
        op_group == "Disinfection by Products (DBPs)" ~ "Disinfection By Products (DBPs)",
        .default = op_group
      ))

    # #Ended up updating E540 method name and description in the JK April 22 file as it was
    # #bringing in duplicates
    # observed_properties_check <- observed_properties_old %>%
    #   dplyr::select(method_code, method, method_description) %>%
    #   dplyr::filter(method_code == "E540") %>%
    #   unique()

    # for correcting parm names that have been corrected manually already
    # for correcting parm_codes that have lost their leading zeroes
    observed_properties <- observed_properties %>%
      rename(
        parm_code_backup = parm_code,
        parameter_clean_backup = parameter_clean
      ) %>%
      left_join(
        observed_properties_old %>%
          dplyr::select(parm_code_clean, parm_code, parameter_clean, op_group) %>%
          unique(),
        by = join_by(parm_code_clean)
      ) %>%
      mutate(
        parm_code = ifelse(is.na(parm_code) | parm_code == "", parm_code_backup, parm_code),
        parameter_clean = ifelse(is.na(parameter_clean) | parameter_clean == "",
          parameter_clean_backup, parameter_clean
        )
      ) %>%
      dplyr::select(-c(parm_code_backup, parameter_clean_backup, parm_code_clean)) %>%
      unique()

    # #This duplication is occurring due to multiple OP groups corresponding to same parm_code and method_code
    # observed_properties_check <- observed_properties %>%
    #   group_by(parm_code, method_code) %>%
    #   filter(n() > 1) %>%
    #   ungroup()

    # for correcting method names that have been corrected manually already
    # for correcting method_codes that have lost their leading zeroes
    observed_properties <- observed_properties %>%
      rename(
        method_code_backup = method_code,
        method_clean_backup = method_clean,
        method_description_backup = method_description
      ) %>%
      left_join(
        observed_properties_old %>%
          dplyr::select(method_code_clean, method_code, method_clean, method_description) %>%
          unique(),
        by = join_by(method_code_clean)
      ) %>%
      mutate(
        method_code = ifelse(is.na(method_code) | method_code == "",
          method_code_backup,
          method_code
        ),
        method_clean = ifelse(is.na(method_clean) | method_clean == "",
          method_clean_backup,
          method_clean
        ),
        method_description = ifelse(is.na(method_description) | method_description == "",
          method_description_backup,
          method_description
        )
      ) %>%
      dplyr::select(-c(
        method_code_backup, method_clean_backup,
        method_code_clean, method_description_backup
      )) %>%
      unique()

    # #This duplication is occurring
    # observed_properties_check <- observed_properties %>%
    #   dplyr::select(method_code, method_clean, method_description) %>%
    #   unique() %>%
    #   group_by(method_code) %>%
    #   dplyr::filter(n()>1) %>%
    #   ungroup() %>%
    #   unique()
    #
    # #This duplication is occurring
    # observed_properties_check <- observed_properties %>%
    #   group_by(parm_code, method_code) %>%
    #   filter(n() > 1) %>%
    #   ungroup()

    # This line and those similar to this line get commented when generating crosswalk for SALUS (BF)
    ## The units don't matter for non-convertable OP units. For example microbial units. So remove them from consideration.
    # observed_properties$sample_unit[observed_properties$convertable_in_samples == "N"] <- ""

    # observed_properties_test <- observed_properties %>% dplyr::filter(nchar(parm_code) <= 3)
    #
    # observed_properties_test <- observed_properties %>% dplyr::filter(nchar(method_code) <= 3)

    # parm_code and method_code together uniquely identify OPs from EMS
    # cannot use method_clean because ICP method name exists for two method codes
    observed_properties <- observed_properties %>%
      rename(cas_backup = cas) %>%
      dplyr::select(-c(field_test_ind, ministry_approved_ind, active_ind)) %>%
      left_join(
        observed_properties_old %>%
          dplyr::select(-c(
            first_used, last_used, when_created:who_updated,
            method, method_clean, method_description, op_group,
            parameter, parm_code_clean, parameter_clean, results, unit
          )),
        by = join_by(parm_code, method_code)
      ) %>%
      mutate(mdl = ifelse(is.na(mdl_backup) | mdl_backup == "", mdl, mdl_backup)) %>%
      dplyr::select(-mdl_backup) %>%
      mutate(cas = ifelse(is.na(cas), cas_backup, cas)) %>%
      group_by(parameter_clean) %>%
      mutate(
        first_non_na = first(cas[!is.na(cas)]), # find first non-NA value in the group
        cas = if_else(is.na(cas), first_non_na, cas)
      ) %>%
      ungroup() %>%
      mutate(cas = case_when(
        parameter_clean == "Dichlorophenoxy(2;4-) Butyric Acid; 4- [2;4-DB]" ~ "94-82-6",
        # parameter_clean == "Aspon" ~ "3244-90-4",
        parameter_clean == "Aldicarb Sulfoxide" ~ "1646-87-3",
        parameter_clean == "Phaeophytin A" ~ "603-17-8",
        parameter_clean == "Amyl OH" ~ "6032-29-7",
        parameter_clean == "Abietic Acid" ~ "514-10-3",
        .default = cas
      )) %>%
      group_by(parameter_clean) %>%
      mutate(
        first_non_na = first(analysis_type[!is.na(analysis_type)]), # find first non-NA value in the group
        analysis_type = if_else(is.na(analysis_type), first_non_na, analysis_type)
      ) %>%
      ungroup() %>%
      mutate(analysis_type = case_when(
        parameter_clean == "Ammonia" ~ "Chemical",
        parameter_clean == "Condensable Particulate Matter (CPM)" ~ "Chemical",
        parameter_clean == "Condensable Inorganic Particulate Matter" ~ "Chemical",
        parameter_clean == "Condensable Organic Particulate Matter" ~ "Chemical",
        parameter_clean == "Total Organic Compounds" ~ "Chemical",
        .default = analysis_type
      )) %>%
      mutate(op_group = case_when(
        parameter_clean == "Ammonia" ~ "",
        parameter_clean == "Carbon Monoxide" ~ "",
        parameter_clean == "Condensable Particulate Matter (CPM)" ~ "Particulates",
        parameter_clean == "Condensable Inorganic Particulate Matter" ~ "Particulates",
        parameter_clean == "Condensable Organic Particulate Matter" ~ "Particulates",
        parameter_clean == "Total Organic Compounds" ~ "Aggregate Organics",
        parameter_clean == "Volume Flow Rate-Stk" ~ "Physical Properties",
        .default = op_group
      )) %>%
      dplyr::select(-c(cas_backup, first_non_na))

    observed_properties <- observed_properties %>%
      mutate(description = case_when(
        is.na(description) & !is.na(op_group) ~ str_c(op_group, "; EMS code: ", parm_code),
        is.na(description) & is.na(op_group) ~ str_c("EMS code: ", parm_code),
        .default = description
      )) %>%
      unique()

    # #No new duplication is occurring
    # observed_properties_check <- observed_properties %>%
    #   group_by(parm_code, method_code) %>%
    #   filter(n() > 1) %>%
    #   ungroup()


    observed_properties_missing_cas <- observed_properties %>%
      dplyr::filter(is.na(cas)) %>%
      dplyr::select(parameter_clean, cas) %>%
      unique()

    # Loop over each parameter and get synonyms
    # observed_properties_found_cas <- observed_properties_missing_cas %>%
    observed_properties_found_cas <- observed_properties_missing_cas %>%
      rowwise() %>%
      mutate(cas = get_synonyms_safe(parameter_clean)) %>%
      ungroup()

    observed_properties <- observed_properties %>%
      rename(cas_backup = cas) %>%
      left_join(observed_properties_found_cas, by = join_by(parameter_clean)) %>%
      mutate(cas = ifelse(is.na(cas), cas_backup, cas)) %>%
      dplyr::select(-cas_backup)

    # #No new duplication is occurring
    # observed_properties_check <- observed_properties %>%
    #   group_by(parm_code, method_code) %>%
    #   filter(n() > 1) %>%
    #   ungroup()

    # clean up modifers
    observed_properties$modifier[is.na(observed_properties$modifier)] <- ""

    # #Add an empty result_type column
    observed_properties$result_type <- ""

    observed_properties$last_upload <- ""

    # #No new duplication is occurring
    # observed_properties_check <- observed_properties %>%
    #   group_by(parm_code, method_code) %>%
    #   filter(n() > 1) %>%
    #   ungroup()

    # #Keep the core columns at the front
    # cols_to_move <- c("newnameid", "parm_code", "description",
    #                   "analysis_type", "result_type", "sample_unit_group",
    #                   "sample_unit","CAS", "op_group")

    # get the unique list of OP IDs
    observed_properties <- observed_properties %>% unique()

    ## observed_properties new to EnMoDS not in EMS
    observed_properties_new <-
      readxl::read_excel("./inst/extdata/Reference_Lists/Observed_Properties_New.xlsx",
        sheet = "Observed_Properties_New"
      ) %>%
      dplyr::rename_with(tolower) %>%
      dplyr::rename_with(~ gsub("\\.", "_", .)) %>%
      mutate(parm_code = case_when(
        str_detect(parm_code, "^[0-9]{1,3}$") ~ str_pad(parm_code, width = 4, pad = "0"),
        TRUE ~ parm_code
      )) %>%
      mutate(method_code = case_when(
        str_detect(method_code, "^[0-9]{1,3}$") ~ str_pad(method_code, width = 4, pad = "0"),
        TRUE ~ method_code
      )) %>%
      unique()

    # ## observed_properties new to EnMoDS not in EMS
    # observed_properties_new_parm_method_code <-
    #   read_csv("./inst/extdata/Reference_Lists/Observed_Properties_New.csv") %>%
    #   dplyr::rename_with(tolower) %>%
    #   dplyr::rename_with(~ gsub("\\.", "_", .)) %>%
    #   dplyr::select(newnameid, parm_code, method_code) %>%
    #   unique()
    #
    # observed_properties_new <- observed_properties_new %>%
    #   rename(parm_code_backup = parm_code,
    #          method_code_backup = method_code) %>%
    #   left_join(observed_properties_new_parm_method_code,
    #             by = join_by(newnameid)) %>%
    #   mutate(method_code = ifelse(is.na(method_code)|method_code == "",
    #                               method_code_backup,
    #                               method_code),
    #          parm_code = ifelse(is.na(parm_code)|parm_code == "",
    #                             parm_code_backup,
    #                             parm_code)) %>%
    #   dplyr::select(-c(parm_code_backup, method_code_backup)) %>%
    #   unique()

    # fixing the missing sample group id issue in the list
    observed_properties_new <- observed_properties_new %>%
      dplyr::mutate(sample_unit_group = if_else(newnameid == "Biological Sample Volume (vol.)",
        "Volume", sample_unit_group
      ))

    # Identify the new columns compared to observed_properties
    new_cols <- setdiff(names(observed_properties), names(observed_properties_new))

    # Create a tibble of just those new columns
    new_data <- observed_properties %>%
      dplyr::select(all_of(new_cols)) %>%
      dplyr::mutate(across(everything(), ~NA)) %>%
      unique() %>%
      dplyr::slice(rep(1, nrow(observed_properties_new)))

    observed_properties_new <- observed_properties_new %>%
      dplyr::bind_cols(new_data) %>%
      mutate(parameter_clean = newnameid)

    observed_properties <- observed_properties %>%
      dplyr::bind_rows(observed_properties_new %>%
        dplyr::mutate(results = NA)) %>%
      unique()

    # observed_properties %>% dplyr::filter(is.na(parameter))

    # #No new duplication is occurring
    # observed_properties_check <- observed_properties %>%
    #   group_by(parm_code, method_code) %>%
    #   filter(n() > 1) %>%
    #   ungroup()

    # UPDATE 20260105: Adding non-EMS method details for Taxonomic OPs in EMS columns
    ## Taxonomic observed_properties
    observed_properties_taxonomic <- utils::read.csv("./inst/extdata/Reference_Lists/Taxonomic_OP.csv",
      stringsAsFactors = F
    ) %>%
      dplyr::rename_with(tolower) %>%
      dplyr::rename_with(~ gsub("\\.", "_", .)) # 1607

    # Create duplicate rows with modified NewNameID
    observed_properties_taxonomic_duplicated <- observed_properties_taxonomic %>%
      # Create a copy of the entire dataframe
      bind_rows(observed_properties_taxonomic) %>%
      # arrange(newnameid) %>%
      # Add a grouping indicator to distinguish original from duplicate
      mutate(row_group = rep(c("original", "duplicate"), each = nrow(observed_properties_taxonomic))) %>%
      # Modify NewNameID for duplicate rows only
      mutate(
        newnameid = ifelse(row_group == "duplicate",
          paste0(newnameid, " (mass)"),
          newnameid
        ),
        sample_unit_group = ifelse(row_group == "duplicate",
          "Mass",
          sample_unit_group
        ),
        method = "Lab based analysis of taxonomic counts",
        method_clean = "Lab based analysis of taxonomic counts",
        method_code = "TAXA",
        method_description = "Place holder method used for lab based analysis of taxonomic counts. This method has no particular meaning and was not used in EMS where no method was assigned to biological count data."
      ) %>%
      # Remove the helper column
      select(-row_group)

    # Save the result (adjust file path as needed)
    # write.csv(df_duplicated, "output_file.csv", row.names = FALSE)
    # write.table(df_duplicated, "output_file.txt", sep = "\t", row.names = FALSE)

    # get the unique list of OP IDs
    observed_properties_taxonomic <- observed_properties_taxonomic_duplicated %>% unique()

    # JEREMY SUSPECTED WON'T WORK ON THE EDT END OR AQI END; COMMENTING FOR NOW
    # #Bring in taxon units since sample units in raw file are completely empty
    # observed_properties_taxonomic_units <- utils::read.csv("./inst/extdata/Reference_Lists/EMS_taxon_results_20250709.csv",
    #                                                  stringsAsFactors = F) %>%
    #   dplyr::rename_with(tolower) %>%
    #   dplyr::rename_with(~ gsub("\\.", "_", .)) %>% #1607
    #   dplyr::select(result_species_code, mdl_unit) %>%
    #   unique()
    #
    # #get the unique list of OP IDs
    # observed_properties_taxonomic <- observed_properties_taxonomic %>%
    #   left_join(observed_properties_taxonomic_units,
    #             by = join_by(parm_code == result_species_code)) %>%
    #   rename(sample_unit = mdl_unit) %>%
    #   unique()

    # Identify the new columns compared to observed_properties
    new_cols <- setdiff(names(observed_properties), names(observed_properties_taxonomic))

    # Create a tibble of just those new columns
    new_data <- observed_properties %>%
      dplyr::select(all_of(new_cols)) %>%
      dplyr::mutate(across(everything(), ~NA)) %>%
      unique() %>%
      dplyr::slice(rep(1, nrow(observed_properties_taxonomic)))

    observed_properties_taxonomic <- observed_properties_taxonomic %>%
      dplyr::bind_cols(new_data) %>%
      mutate(parameter_clean = newnameid)

    observed_properties <- observed_properties %>%
      dplyr::bind_rows(observed_properties_taxonomic %>%
        dplyr::mutate(results = NA)) %>%
      unique()

    # observed_properties %>% dplyr::filter(is.na(parameter))

    ## Merge all observed_properties and process

    observed_properties <- observed_properties %>%
      # dplyr::bind_rows(observed_properties, observed_properties_new, observed_properties_taxonomic) %>%
      #        unique() %>%
      dplyr::group_by(newnameid) %>%
      dplyr::mutate(count = n()) %>%
      dplyr::ungroup()

    # the total number of unique newnameid should be the same as the total number of unique rows
    new_name_id_unique <- observed_properties$newnameid %>% unique()

    observed_properties <- observed_properties %>%
      dplyr::mutate(sample_unit = case_when(
        sample_unit == "pH Units" ~ "pH units",
        # sample_unit == NA ~ Unit,
        # sample_unit == "" ~ NA,
        .default = sample_unit
      ), )

    # rename the required observed_properties so they show up otherwise they are in the background?
    observed_properties <- observed_properties %>%
      dplyr::mutate(sample_unit_group = case_when(
        newnameid == "Biological Sex (cat.)" ~ "",
        newnameid == "Biological Life Stage (cat.)" ~ "",
        # sample_unit_group == "Length" ~ "SYS-REQUIRED - Length",
        # sample_unit_group == "Appearance" ~ "Apperance",
        # unit == "per mille" ~ "DimensionlessRatio"
        .default = sample_unit_group
      )) %>%
      dplyr::mutate(analysis_type = case_when(
        newnameid == "Biological Sex (cat.)" ~ NA,
        newnameid == "Biological Life Stage (cat.)" ~ NA,
        # sample_unit_group == "Length" ~ "SYS-REQUIRED - Length",
        # sample_unit_group == "Appearance" ~ "Apperance",
        # unit == "per mille" ~ "DimensionlessRatio"
        .default = analysis_type
      ))

    observed_properties <- observed_properties %>%
      dplyr::mutate(fraction = case_when(
        fraction == "Total" ~ "Total",
        fraction == "Dissolved" ~ "Dissolved",
        .default = ""
      ))

    observed_properties <- observed_properties %>%
      dplyr::select(parm_code, parameter, unit, method_code, sample_unit_group, everything())

    # #No new duplication is occurring
    # observed_properties_check <- observed_properties %>%
    #   group_by(parm_code, method_code) %>%
    #   filter(n() > 1) %>%
    #   ungroup()

    # allocate sample_unit and sample_unit_group based on units crosswalk
    # units_crosswalk <- readxl::read_excel("./inst/extdata/Reference_Lists/aqs_units_temp_JK_SB_20250715.xlsx",
    units_crosswalk <- readxl::read_excel("./inst/extdata/Reference_Lists/ems_import_aqs_units_temp_crosswalk_20250812.xlsx",
      col_types = c(rep("text", 4), "guess")
    )

    # for a combination of parameter and unit, assign first non NA sample_unit_group to NA rows
    observed_properties <- observed_properties %>%
      rename(
        sample_unit_group_backup = sample_unit_group,
        sample_unit_backup = sample_unit
      ) %>%
      # mutate(sample_unit_backup = case_when(
      #   unit == "per mille" ~ "‰",
      #   .default = sample_unit_backup
      # )) %>%
      left_join(units_crosswalk %>%
        dplyr::select(EMS_SHORT_NAME, AQS_UNIT_GROUP, AQS_UNIT_CUSTOM_ID) %>%
        unique() %>%
        dplyr::filter(!is.na(EMS_SHORT_NAME)), by = join_by("unit" == "EMS_SHORT_NAME")) %>%
      rename(sample_unit = AQS_UNIT_CUSTOM_ID, sample_unit_group = AQS_UNIT_GROUP)

    # observed_properties_check <- observed_properties %>%
    #   dplyr::filter(is.na(sample_unit)|is.na(sample_unit_group))

    observed_properties <- observed_properties %>%
      mutate(sample_unit = case_when(
        unit == "PSU" ~ "PSU",
        unit == "per mille" ~ "‰",
        .default = sample_unit
      )) %>%
      mutate(sample_unit_group = case_when(
        unit == "PSU" ~ "DimensionlessRatio",
        unit == "per mille" ~ "DimensionlessRatio",
        .default = sample_unit_group
      ))

    # observed_properties_check <- observed_properties %>%
    #   dplyr::filter(is.na(sample_unit)|is.na(sample_unit_group))

    observed_properties <- observed_properties %>%
      mutate(
        sample_unit = ifelse(is.na(sample_unit), sample_unit_backup, sample_unit),
        sample_unit_group = ifelse(is.na(sample_unit_group), sample_unit_group_backup,
          sample_unit_group
        )
      ) %>%
      unique() %>%
      dplyr::select(-c(sample_unit_backup, sample_unit_group_backup))

    # #No new duplication is occurring
    # observed_properties_check <- observed_properties %>%
    #   group_by(parm_code, method_code) %>%
    #   filter(n() > 1) %>%
    #   ungroup()

    # clean up modifers
    observed_properties$modifier[is.na(observed_properties$modifier)] <- ""

    # fixing missing newnameIDs
    observed_properties <- observed_properties %>%
      mutate(sample_unit_group_short_name = case_when(
        sample_unit_group == "Appearance" ~ "(app.)",
        # sample_unit_group == "Apperance" ~ "(app.)",
        sample_unit_group == "AirConcentration" ~ "(air conc.)",
        sample_unit_group == "DimensionlessRatio" ~ "(ratio)",
        sample_unit_group == "Flow" ~ "(flow)",
        sample_unit_group == "Flow" ~ "(flow)",
        sample_unit_group == "Mass" ~ "(mass)",
        sample_unit_group == "MassPerMass" ~ "(mass conc.)",
        sample_unit_group == "MassFlux" ~ "(mass flux)",
        sample_unit_group == "ElectricConductivity" ~ "(elec. cond.)",
        sample_unit_group == "Concentration" ~ "(fl. conc.)",
        sample_unit_group == "MicrobialFormation" ~ "(microb.)",
        sample_unit_group == "Acidity" ~ "(acidity)",
        .default = sample_unit_group_short_name
      ))

    # allocate newnameid using parameter_clean, sample_unit, and modifier
    observed_properties <- observed_properties %>%
      mutate(modifier = case_when(
        str_detect(unit, regex("wet", ignore_case = TRUE)) ~ "(wet)",
        .default = modifier
      )) %>%
      mutate(parameter_clean = parameter_clean %>%
        str_replace_all("\\(", " (") %>% # add space before (
        str_replace_all("\\(CAS", "CAS") %>%
        str_replace_all("\\CAS", " CAS") %>%
        str_replace_all("[ \t]+", " ") %>% # collapse multiple spaces/tabs into one
        str_replace_all("(,)? CAS.*", "") %>%
        str_replace_all("\\?", "") %>% # remove all question marks
        str_replace_all("[[:space:][:punct:]&&[^)\\]\\}]]+$", "") %>%
        str_trim()) %>%
      mutate(newnameid = case_when(
        is.na(newnameid) & modifier != "" & sample_unit_group_short_name != "" ~ str_c(parameter_clean, " ", modifier, " ", sample_unit_group_short_name),
        is.na(newnameid) & modifier == "" & sample_unit_group_short_name != "" ~ str_c(parameter_clean, " ", sample_unit_group_short_name),
        is.na(newnameid) & modifier != "" & sample_unit_group_short_name == "" ~ str_c(parameter_clean, " ", modifier),
        .default = newnameid
      ))

    # #No new duplication is occurring
    # observed_properties_check <- observed_properties %>%
    #   group_by(parm_code, method_code) %>%
    #   filter(n() > 1) %>%
    #   ungroup()

    observed_properties <- observed_properties %>%
      mutate(op_group = case_when(
        str_to_lower(op_group) == "disinfection by products (dbps)" ~ "Disinfection By-Products (DBPs)",
        .default = op_group
      ))

    observed_properties <- observed_properties %>%
      mutate(result_type = case_when(
        is.na(result_type) ~ "NUMERIC",
        result_type == "" ~ "NUMERIC",
        .default = result_type
      ))

    observed_properties <- observed_properties %>%
      mutate(analysis_type = case_when(
        parm_code == "2CLT" ~ "CHEMICAL",
        parm_code == "B004" ~ "CHEMICAL",
        parm_code == "B034" ~ "CHEMICAL",
        parm_code == "B035" ~ "CHEMICAL",
        parm_code == "B036" ~ "CHEMICAL",
        parm_code == "B038" ~ "CHEMICAL",
        parm_code == "C-13" ~ "CHEMICAL",
        parm_code == "C047" ~ "CHEMICAL",
        parm_code == "C066" ~ "CHEMICAL",
        parm_code == "MPA" ~ "CHEMICAL",
        parm_code == "N-15" ~ "CHEMICAL",
        parm_code == "NAG45" ~ "CHEMICAL",
        parm_code == "NAG70" ~ "CHEMICAL",
        parm_code == "NNP" ~ "CHEMICAL",
        parm_code == "NP" ~ "CHEMICAL",
        parm_code == "O2--" ~ "CHEMICAL",
        parm_code == "P030" ~ "CHEMICAL",
        parm_code == "PAH18" ~ "CHEMICAL",
        parm_code == "RU-D" ~ "CHEMICAL",
        parm_code == "SXO6" ~ "CHEMICAL",
        parm_code == "T065" ~ "CHEMICAL",
        parm_code == "T067" ~ "CHEMICAL",
        parm_code == "T068" ~ "CHEMICAL",
        parm_code == "T069" ~ "CHEMICAL",
        parm_code == "RU-T" ~ "CHEMICAL",
        parm_code == "TDSF" ~ "CHEMICAL",
        parm_code == "A001" ~ "CHEMICAL",
        parm_code == "A005" ~ "CHEMICAL",
        parm_code == "AC02" ~ "CHEMICAL",
        parm_code == "AC03" ~ "CHEMICAL",
        parm_code == "AFDW" ~ "CHEMICAL",
        parm_code == "B033" ~ "CHEMICAL",
        parm_code == "B041" ~ "CHEMICAL",
        parm_code == "B042" ~ "CHEMICAL",
        parm_code == "BUDE" ~ "CHEMICAL",
        parm_code == "C056" ~ "CHEMICAL",
        parm_code == "C065" ~ "CHEMICAL",
        parm_code == "CDS" ~ "CHEMICAL",
        parm_code == "GFE" ~ "CHEMICAL",
        parm_code == "H024" ~ "CHEMICAL",
        parm_code == "I009" ~ "CHEMICAL",
        parm_code == "M007" ~ "CHEMICAL",
        parm_code == "M028" ~ "CHEMICAL",
        parm_code == "M054" ~ "CHEMICAL",
        parm_code == "M4P2" ~ "CHEMICAL",
        parm_code == "P029" ~ "CHEMICAL",
        parm_code == "TCLB" ~ "CHEMICAL",
        TRUE ~ analysis_type
      ))

    # #No new duplication is occurring
    # observed_properties_check <- observed_properties %>%
    #   group_by(parm_code, method_code) %>%
    #   filter(n() > 1) %>%
    #   ungroup()

    observed_properties <- observed_properties %>%
      mutate(classification = case_when(
        parameter_clean == "Ammonia" ~ "LAB",
        parameter_clean == "Arsenic Total" ~ "LAB",
        parameter_clean == "Benzo(e)pyrene" ~ "LAB",
        parameter_clean == "Benzo(j)fluoranthene" ~ "LAB",
        parameter_clean == "Cadmium Total" ~ "LAB",
        parameter_clean == "Carbon Monoxide" ~ "LAB",
        parameter_clean == "Chromium Total" ~ "LAB",
        parameter_clean == "Condensable Inorganic Particulate Matter" ~ "LAB",
        parameter_clean == "Condensable Organic Particulate Matter" ~ "LAB",
        parameter_clean == "Condensable Particulate Matter (CPM)" ~ "LAB",
        parameter_clean == "E Coli" ~ "LAB",
        parameter_clean == "Enterococcus" ~ "LAB",
        parameter_clean == "Formaldehyde" ~ "LAB",
        parameter_clean == "Lead Total" ~ "LAB",
        parameter_clean == "Mercury Total" ~ "LAB",
        parameter_clean == "Nitrogen Dioxide" ~ "LAB",
        parameter_clean == "Particulate <10u" ~ "LAB",
        parameter_clean == "Particulate <2.5u" ~ "LAB",
        parameter_clean == "Particulate:Total" ~ "LAB",
        parameter_clean == "Phenols" ~ "LAB",
        parameter_clean == "Sulfur Dioxide" ~ "LAB",
        parameter_clean == "Total Organic Compounds" ~ "LAB",
        parameter_clean == "Volume Flow Rate-Stk" ~ "LAB",
        parameter_clean == "Nitrogen Ammonia Total" ~ "LAB",
        unit == "PSU" ~ "FIELD_RESULT",
        parm_code == "A001" ~ "LAB",
        parm_code == "A005" ~ "LAB",
        parm_code == "AC02" ~ "LAB",
        parm_code == "AC03" ~ "LAB",
        parm_code == "AFDW" ~ "LAB",
        parm_code == "B033" ~ "LAB",
        parm_code == "B041" ~ "LAB",
        parm_code == "B042" ~ "LAB",
        parm_code == "BUDE" ~ "LAB",
        parm_code == "C056" ~ "LAB",
        parm_code == "C065" ~ "LAB",
        parm_code == "CDS" ~ "LAB",
        parm_code == "GFE" ~ "LAB",
        parm_code == "H024" ~ "LAB",
        parm_code == "I009" ~ "LAB",
        parm_code == "M007" ~ "LAB",
        parm_code == "M028" ~ "LAB",
        parm_code == "M054" ~ "LAB",
        parm_code == "M4P2" ~ "LAB",
        parm_code == "P029" ~ "LAB",
        parm_code == "TCLB" ~ "LAB",
        .default = classification
      ))

    observed_properties_data_class <- readxl::read_excel("./inst/extdata/Reference_Lists/OPs_JK_dataClass_edits_20250715.xlsx",
      col_types = c(rep("text", 31))
    ) %>% # , rep("text", 7), rep("guess", 4))) %>%
      dplyr::rename_with(tolower) %>%
      dplyr::rename_with(~ gsub("\\.", "_", .)) %>%
      dplyr::mutate(method_description = str_replace_all(method_description, "\r", "")) %>%
      mutate(across(everything(), ~ {
        if (is.character(.)) {
          str_replace_all(., 'xml:space="preserve">', "")
        } else {
          .
        }
      }))

    observed_properties <- observed_properties %>%
      rename(classification_backup = classification) %>%
      left_join(
        observed_properties_data_class %>%
          dplyr::select(newnameid, parm_code, analysis_method_code, classification) %>% unique(),
        by = join_by(parm_code, newnameid, "method_code" == "analysis_method_code")
      ) %>%
      mutate(classification = ifelse(is.na(classification) | classification == "",
        classification_backup, classification
      )) %>%
      dplyr::select(-classification_backup)

    observed_properties <- observed_properties %>%
      mutate(classification = case_when(
        parm_code == "SECN" ~ "LAB",
        parm_code == "SEO4" ~ "LAB",
        .default = classification
      ))

    # observed_properties_check <- observed_properties %>%
    #   dplyr::filter(is.na(classification))

    # Fixing dioxins and furans so we can track all EMS codes in EnMoDS
    observed_properties <- observed_properties %>%
      group_by(across(all_of("newnameid"))) %>% # <-- set your grouping column here
      mutate(
        .n_in_group   = n(),
        .codes_group  = str_c(sort(unique(as.character(parm_code))), collapse = "; "),
        .codes_final  = if_else(.n_in_group > 1, .codes_group, as.character(parm_code))
      ) %>%
      ungroup() %>%
      mutate(
        description = case_when(
          op_group == "Dioxins & Furans" ~ str_c(op_group, "; EMS code: ", .codes_final),
          is.na(description) & !is.na(op_group) ~ str_c(op_group, "; EMS code: ", .codes_final),
          is.na(description) & is.na(op_group) ~ str_c("EMS code: ", .codes_final),
          .default = description
        )
      ) %>%
      select(-.n_in_group, -.codes_group, -.codes_final) %>%
      distinct()

    # observed_properties <- observed_properties %>%
    #   mutate(sample_unit_group = case_when(
    #     ,
    #     TRUE ~ sample_unit_group
    #   ))

    # #No new duplication is occurring
    # observed_properties_check <- observed_properties %>%
    #   group_by(parm_code, method_code) %>%
    #   filter(n() > 1) %>%
    #   ungroup()

    observed_properties <- observed_properties %>%
      rename(device_type = `device type`)

    # observed_properties <- observed_properties %>%
    #   mutate(first_used = first_used %>%
    #            strftime(mdy_hms(.), format = "%d-%b-%y %I.%M.%S.000000 %p",
    #                     tz = "UTC") %>% toupper()) %>%
    #   mutate(last_used = last_used %>%
    #            strftime(mdy_hms(.), format = "%d-%b-%y %I.%M.%S.000000 %p",
    #                     tz = "UTC") %>% toupper()) %>%
    #   mutate(when_created = when_created %>%
    #            strftime(mdy_hms(.), format = "%d-%b-%y %I.%M.%S.000000 %p",
    #                     tz = "UTC") %>% toupper()) %>%
    #   mutate(when_updated = when_updated %>%
    #            strftime(mdy_hms(.), format = "%d-%b-%y %I.%M.%S.000000 %p",
    #                     tz = "UTC") %>% toupper()) %>%
    #   mutate(last_upload = last_upload %>%
    #            strftime(mdy_hms(.), format = "%d-%b-%y %I.%M.%S.000000 %p",
    #                     tz = "UTC") %>% toupper())


    # #make a subset tracking all OP Groups associated with a given parameter
    # parameter_op_tracking <- observed_properties_old %>%
    #   dplyr::mutate(op_group = stringr::str_replace_all(op_group, ";", "")) %>%
    #   dplyr::select(parameter_clean, op_group) %>%
    #   unique()
    #
    # #remove semicolons from OP
    # observed_properties <- observed_properties %>%
    #   dplyr::mutate(op_group = stringr::str_replace_all(op_group, ";", "")) %>%
    #   dplyr::select(-op_group) %>%
    #   lefT_join(parameter_op_tracking, by = join_by(parameter_clean)) %>%
    #   dplyr::select(-parameter_clean)

    # Count of rows that have at least one NA
    na_row_count <- observed_properties %>%
      filter(if_any(everything(), is.na)) %>%
      nrow()

    # Count of NAs in each column, as a tibble
    na_column_summary <- tibble(
      column_name = names(observed_properties),
      na_count = sapply(observed_properties, function(x) sum(is.na(x)))
    ) %>% arrange(desc(na_count)) # Optional: sort by count

    # Count of NAs in each column, as a tibble
    blank_column_summary <- tibble(
      column_name = names(observed_properties),
      na_count = sapply(observed_properties %>% as.character(), function(x) sum(x == ""))
    ) %>% arrange(desc(na_count)) # Optional: sort by count

    # THIS IS NOT NEEDED SINCE WE ARE NOW USING JK CROSSWALK
    # %>%
    #    group_by(parameter, sample_unit) %>%
    #   mutate(
    #     first_non_na = first(sample_unit_group[!is.na(sample_unit_group)]),  # find first non-NA value in the group
    #     sample_unit_group = if_else(is.na(sample_unit_group), first_non_na, sample_unit_group)
    #   ) %>%
    #   select(-first_non_na) %>%
    #   ungroup()

    # THIS IS NOT CORRECT
    # #remove duplicate parameter method combinations that did not get a unit group
    # observed_properties <- observed_properties %>%
    #   group_by(parameter, method_code) %>%
    #   mutate(has_value = any(!is.na(sample_unit_group) & sample_unit_group != "")) %>%
    #   filter(!(has_value & (is.na(sample_unit_group) | sample_unit_group == ""))) %>%
    #   select(-has_value) %>%
    #   ungroup() %>% unique()

    # need to get unit group and unit IDs prior to importing observed_properties
    unit_groups <- get_profiles(env, "unit_groups") %>%
      dplyr::select(id, customId, supportsConversion) %>%
      dplyr::rename("unit_group_id" = "id")

    # they are not!
    # current pipeline will upload the first OP into the system
    # But its fine because...
    # they belong to unit groups that are convertible
    # Below code helps you make that check
    observed_properties_convertible <- observed_properties %>%
      dplyr::filter(count > 1) %>%
      dplyr::left_join(
        unit_groups %>%
          dplyr::select(customId, unit_group_id, supportsConversion),
        by = dplyr::join_by("sample_unit_group" == "customId")
      ) %>%
      dplyr::filter(supportsConversion == FALSE)

    # #for a given unit, allocate a sample unit group
    # units <- get_profiles("prod", "units") %>%
    #   unnest_wider(unitGroup, names_sep = "_") %>%
    #   dplyr::select(customId, unitGroup_customId)

    # THIS IS NOT NEEDED SINCE EVERYTHING HAS A SAMPLE UNIT GROUP NOW
    # observed_properties <- observed_properties %>%
    #   mutate(sample_unit = case_when(
    #     is.na(sample_unit) ~ unit,
    #     .default = sample_unit
    #   )) %>%
    #   left_join(units, by = join_by("sample_unit" == "customId")) %>%
    #   mutate(sample_unit_group = case_when(
    #     (parameter == "Benzo (e)pyrene" & sample_unit == "ug/m3") ~ "AirConcentration",
    #     (parameter == "Arsenic Total" & sample_unit == "ug/m3") ~ "AirConcentration",
    #     (parameter == "Benzo (j)fluoranthene" & sample_unit == "ug/g") ~ "MassPerMass",
    #     is.na(sample_unit_group) ~ unitGroup_customId,
    #     .default = sample_unit_group
    #   )) %>%
    #     dplyr::select(-unitGroup_customId)

    observed_properties <- observed_properties %>%
      dplyr::select(-count) %>%
      dplyr::select(parameter, method_code, sample_unit_group, everything()) %>%
      unique()

    # add GUID to the list of observed_properties for unit groups
    observed_properties <- dplyr::left_join(observed_properties, unit_groups,
      by = dplyr::join_by("sample_unit_group" == "customId"),
      keep = FALSE
    )

    # units without groups
    units <- get_profiles(env, "units") %>%
      dplyr::select(id, customId) %>%
      dplyr::rename("unit_id" = "id")

    # add GUID to the list of observed_properties for units
    observed_properties <- dplyr::left_join(observed_properties, units,
      by = dplyr::join_by("sample_unit" == "customId"),
      keep = FALSE
    )

    # analysis.Type must be ALL CAPS
    observed_properties$analysis_type <- toupper(observed_properties$analysis_type)
    observed_properties$result_type <- toupper(observed_properties$result_type)

    # correcting spacing in most EnMoDS columns but skipping EMS columns
    observed_properties <- observed_properties %>%
      mutate(across(
        -c(
          parameter, method_code, parm_code, unit,
          method, mdl, results, first_used:who_updated,
          newnameid, method_description
        ),
        ~ str_replace_all(., "(;|:)\\s*", "\\1 ") # Ensure one space after ; or :
      )) # %>%
    # mutate(across(
    #   newnameid,
    #   ~ .x %>%
    #     str_replace_all("\\((\\d+)", "\\1") %>%    # opening bracket before number → number
    #     str_replace_all("(\\d+)\\)", "\\1")        # number before closing bracket → number
    # ))

    # UPDATE 20260106 Adding an UNK method to all OPs by duplicating all rows and adding info for the UNK method
    # Create duplicate rows with modified NewNameID
    observed_properties_duplicated <- observed_properties %>%
      # Create a copy of the entire dataframe
      bind_rows(observed_properties) %>%
      # arrange(newnameid) %>%
      # Add a grouping indicator to distinguish original from duplicate
      mutate(row_group = rep(c("original", "duplicate"), each = nrow(observed_properties))) %>%
      # Modify NewNameID for duplicate rows only
      mutate(
        method = ifelse(row_group == "duplicate",
          "Unknown",
          method
        ),
        method_clean = ifelse(row_group == "duplicate",
          "Unknown",
          method_clean
        ),
        method_code = ifelse(row_group == "duplicate",
          "UNK",
          method_code
        ),
        method_description = ifelse(row_group == "duplicate",
          "Place holder method used for lab based analysis where the analysis method is not known.",
          method_description
        )
      ) %>%
      dplyr::filter((row_group == "duplicate" & (!(analysis_type == "BIOLOGICAL"))) | row_group == "original") %>%
      # Remove the helper column
      dplyr::select(-row_group)

    observed_properties <- observed_properties_duplicated %>% unique()

    observed_properties <- observed_properties %>%
      dplyr::select(-c(method_clean)) %>%
      dplyr::select(parm_code, method_code, sample_unit_group, results, everything()) %>%
      unique()

    # #No new duplication is occurring
    # observed_properties_check <- observed_properties %>%
    #   group_by(parm_code, method_code) %>%
    #   filter(n() > 1) %>%
    #   ungroup()

    # observed_properties_missing_unitgroup <- observed_properties %>%
    #   dplyr::filter(results>0) %>%
    #   dplyr::filter(is.na(unit_group_id))

    writexl::write_xlsx(observed_properties, "./inst/extdata/Reference_Lists/Consolidated_Observed_Properties.xlsx")

    # Load an existing workbook
    wb <- openxlsx::loadWorkbook("./inst/extdata/Reference_Lists/Consolidated_Observed_Properties.xlsx")

    # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
    openxlsx::renameWorksheet(wb, sheet = "Sheet1", newName = "Observed_Properties")

    # Save the workbook with the updated sheet name
    openxlsx::saveWorkbook(wb, "./inst/extdata/Reference_Lists/Consolidated_Observed_Properties.xlsx", overwrite = TRUE)

    # observed_properties <- observed_properties %>%
    #   dplyr::select(c("parm_code", "newnameid", "description", "analysis_type",
    #                   "result_type", "sample_unit_group", "sample_unit","cas", "op_group")) %>% unique()
  }

  # write.csv(observed_properties, "./inst/extdata/Reference_Lists/Consolidated_Observed_Properties.csv")

  # PREPROCESSING OP FOR NEW DATA --------------------------------------

  # Get base file; Unit and unit groups may have been updated; remove their IDs
  observed_properties <-
    readxl::read_excel("./inst/extdata/Reference_Lists/Consolidated_Observed_Properties.xlsx",
      col_types = c(rep("guess", 26), rep("text", 7), rep("guess", 4))
    ) %>%
    dplyr::rename_with(tolower) %>%
    dplyr::rename_with(~ gsub("\\.", "_", .)) %>%
    dplyr::mutate(method_description = str_replace_all(method_description, "\r", "")) %>%
    mutate(across(everything(), ~ {
      if (is.character(.)) {
        str_replace_all(., 'xml:space="preserve">', "")
      } else {
        .
      }
    })) %>%
    # mutate(across(everything(), ~ {
    #   if (is.character(.)) {
    #     str_replace_all(., ',', ";")
    #   } else {
    #     .
    #   }
    # })) %>%
    # collapse multiple spaces/tabs into one
    # mutate(across(everything(), ~ ifelse(is.character(.), str_replace_all(., 'xml:space="preserve">', ""),.))) %>%
    # dplyr::mutate(method_description = str_replace_all(method_description, 'xml:space="preserve">', "")) %>%
    # dplyr::mutate(method_description = str_replace_all(description, 'xml:space="preserve">', "")) %>%
    # dplyr::mutate(newnameid = str_replace_all(newnameid, 'xml:space="preserve">', "")) %>%
    # dplyr::mutate(op_group = str_replace_all(op_group, ";", "")) %>%
    dplyr::select(-c(supportsconversion, unit_group_id, unit_id)) %>%
    mutate(parm_code = case_when(
      str_detect(parm_code, "^[0-9]{1,3}$") ~ str_pad(parm_code, width = 4, pad = "0"),
      TRUE ~ parm_code
    )) %>%
    mutate(method_code = case_when(
      str_detect(method_code, "^[0-9]{1,3}$") ~ str_pad(method_code, width = 4, pad = "0"),
      TRUE ~ method_code
    ))

  # #No new duplication is occurring
  # observed_properties_check <- observed_properties_base %>%
  #   group_by(parm_code, method_code) %>%
  #   filter(n() > 1) %>%
  #   ungroup()

  # #Get base file; Unit and unit groups may have been updated; remove their IDs
  # observed_properties_base_parm_method_code <-
  # read_csv("./inst/extdata/Reference_Lists/Consolidated_Observed_Properties.csv") %>%
  #   dplyr::rename_with(tolower) %>%
  #   dplyr::rename_with(~ gsub("\\.", "_", .)) %>%
  #   dplyr::mutate(method_description = str_replace_all(method_description, "\r", "")) %>%
  #   mutate(across(everything(), ~ {
  #     if (is.character(.)) {
  #       str_replace_all(., 'xml:space="preserve">', "")
  #     } else {
  #       .
  #     }
  #   })) %>%
  #   # mutate(across(everything(), ~ {
  #   #   if (is.character(.)) {
  #   #     str_replace_all(., ',', ";")
  #   #   } else {
  #   #     .
  #   #   }
  #   # })) %>%
  #   # collapse multiple spaces/tabs into one
  #   #mutate(across(everything(), ~ ifelse(is.character(.), str_replace_all(., 'xml:space="preserve">', ""),.))) %>%
  #   # dplyr::mutate(method_description = str_replace_all(method_description, 'xml:space="preserve">', "")) %>%
  #   # dplyr::mutate(method_description = str_replace_all(description, 'xml:space="preserve">', "")) %>%
  #   # dplyr::mutate(newnameid = str_replace_all(newnameid, 'xml:space="preserve">', "")) %>%
  #   dplyr::select(newnameid, parameter, method, parm_code, method_code) %>%
  #   unique()
  #
  # observed_properties_base <- observed_properties_base %>%
  #   rename(parm_code_backup = parm_code,
  #          method_code_backup = method_code) %>%
  #   left_join(observed_properties_base_parm_method_code,
  #             by = join_by(newnameid, parameter, method)) %>%
  #   mutate(parm_code = ifelse(is.na(parm_code)|parm_code == "",
  #                             parm_code_backup, parm_code),
  #          method_code = ifelse(is.na(method_code)|method_code == "",
  #                               method_code_backup,
  #                               method_code)) %>%
  #   dplyr::select(-c(parm_code_backup, method_code_backup))

  # This line and those similar to this line get commented when generating crosswalk for SALUS (BF)
  ## The units don't matter for non-convertable OP units. For example microbial units. So remove them from consideration.
  # observed_properties_base$sample_unit[observed_properties_base$convertable_in_samples == "N"] <- ""

  # #clean up modifers
  # observed_properties_base$modifier[is.na(observed_properties_base$modifier)] <- ""

  # #checking if a new file is in the folder
  # file_exists <- length(base::list.files(pattern = "^Observed_properties_ems_jk_")) > 0
  #
  # #if file matching pattern exists in this folder, it would be in this list
  # file_exists <- base::list.files(path = "./inst/extdata/Reference_Lists/",
  #                                 pattern = "^Observed_properties_ems_jk_", full.names = TRUE)
  #
  # #ADD NEW FILE HERE; IF NO NEW FILE, CONSOLIDATED BASE OPS FILE WILL BE USED
  # #picks the latest file of all the files identified
  # if (length(file_exists) > 0) {
  #   latest_file <- file_exists[which.max(file.info(file_exists)$mtime)]
  #   message("Latest file found: ", latest_file)
  #
  #   observed_properties_new <- readxl::read_excel(latest_file) %>%
  #     dplyr::rename_with(tolower) %>%
  #     dplyr::rename_with(~ gsub("\\.", "_", .)) %>%
  #     dplyr::mutate(method_description = str_replace_all(method_description, "\r", "")) %>%
  #     dplyr::mutate(method_description = str_replace_all(method_description, 'xml:space="preserve">', "")) %>%
  #     dplyr::mutate(op_group = str_replace_all(op_group, ";", ""))
  #
  #   #remove semicolons from OP
  #   observed_properties_new <- observed_properties_new %>%
  #     dplyr::mutate(op_group = stringr::str_replace_all(op_group, ";", ""))
  #
  #   #This line and those similar to this line get commented when generating crosswalk for SALUS (BF)
  #   ##The units don't matter for non-convertable OP units. For example microbial units. So remove them from consideration.
  #   #observed_properties_new$sample_unit[observed_properties_new$convertable_in_samples == "N"] <- ""
  #
  #   #clean up modifers
  #   observed_properties_new$modifier[is.na(observed_properties_new$modifier)] <- ""
  #
  #   if (!"result_type" %in% names(observed_properties_new)) {
  #     #Add an empty result_type column
  #     observed_properties_new$result_type <- "" #want an empty string
  #   }
  #
  #   #Identify the new columns compared to observed_properties
  #   new_cols <- setdiff(names(observed_properties_base), names(observed_properties_new))
  #
  #   # Create a tibble of just those new columns
  #   new_data <- observed_properties_base %>%
  #     dplyr::select(all_of(new_cols)) %>%
  #     dplyr::mutate(across(everything(), ~ NA)) %>%
  #     unique() %>% dplyr::slice(rep(1, nrow(observed_properties_new)))
  #
  #   observed_properties_new <- observed_properties_new %>% dplyr::bind_cols(new_data)
  #
  #   observed_properties <- observed_properties_base %>%
  #     dplyr::bind_rows(observed_properties_new %>% dplyr::mutate(results = NA)) %>% unique()
  #
  #   # ## Merge all observed_properties
  #   # observed_properties <- observed_properties %>%
  #   #   #dplyr::bind_rows(observed_properties, observed_properties_new_to_EnMoDS, observed_properties_taxonomic) %>%
  #   #   #        unique() %>%
  #   #   dplyr::group_by(newnameid) %>%
  #   #   dplyr::mutate(count = n()) %>%
  #   #   dplyr::ungroup()
  #
  #   #the total number of unique newnameid should be the same as the total number of unique rows
  #   new_name_id_unique <- observed_properties$newnameid %>% unique()
  #
  #   observed_properties <- observed_properties %>%
  #     dplyr::mutate(sample_unit = case_when(
  #       sample_unit == "pH Units" ~ "pH units",
  #       #sample_unit == NA ~ Unit,
  #       #sample_unit == "" ~ NA,
  #       .default = sample_unit),
  #     )
  #
  #   #rename the required observed_properties so they show up otherwise they are in the background?
  #   observed_properties <- observed_properties %>%
  #     dplyr::mutate(sample_unit_group = case_when(
  #       newnameid == "Biological Sex (cat.)" ~ "",
  #       newnameid == "Biological Life Stage (cat.)" ~ "",
  #       sample_unit_group == "Length" ~ "SYS-REQUIRED - Length",
  #       sample_unit_group == "Apperance"~ "Appearance",
  #       .default = sample_unit_group
  #     )) %>%
  #     dplyr::mutate(analysis_type = case_when(
  #       newnameid == "Biological Sex (cat.)" ~ "BIOLOGICAL",
  #       newnameid == "Biological Life Stage (cat.)" ~ "BIOLOGICAL",
  #       .default = analysis_type
  #     ))
  #
  # } else {
  #
  #   #Unit and unit groups may have been updated; remove their IDs
  #   observed_properties <- observed_properties_base
  #
  # }

  # need to get unit group and unit IDs prior to importing observed_properties
  # hard coding test because that is what Salus and JK need
  unit_groups <- get_profiles(env, "unit_groups") %>%
    dplyr::select(id, customId, supportsConversion) %>%
    dplyr::rename("unit_group_id" = "id")

  # TESTING ONLY
  # #they are not!
  # #current pipeline will upload the first OP into the system
  # #But its fine because...
  # #they belong to unit groups that are convertible
  # #Below code helps you make that check
  # observed_properties_convertible <- observed_properties %>%
  #   dplyr::filter(count>1) %>%
  #   dplyr::left_join(unit_groups %>%
  #                      dplyr::select(customId, unit_group_id, supportsConversion),
  #                    by = dplyr::join_by("sample_unit_group" == "customId")) %>%
  #   dplyr::filter(supportsConversion == FALSE)

  # add GUID to the list of observed_properties for unit groups
  observed_properties <- dplyr::left_join(observed_properties, unit_groups,
    by = dplyr::join_by("sample_unit_group" == "customId"),
    keep = FALSE
  )

  # observed_properties_missing_unitgroup <- observed_properties %>%
  #   dplyr::filter(results != 0 & is.na(unit_group_id))

  # units without groups
  units <- get_profiles(env, "units") %>%
    dplyr::select(id, customId) %>%
    dplyr::rename("unit_id" = "id")

  # add GUID to the list of observed_properties for units
  observed_properties <- observed_properties %>%
    dplyr::left_join(units,
      by = dplyr::join_by("sample_unit" == "customId"),
      keep = FALSE
    )

  # analysis.Type must be ALL CAPS
  observed_properties$analysis_type <- toupper(observed_properties$analysis_type)
  observed_properties$result_type <- toupper(observed_properties$result_type)

  observed_properties <- observed_properties %>%
    mutate(across(
      -c(
        parameter, method_code, parm_code, unit,
        method, mdl, results, first_used:who_updated,
        newnameid, method_description
      ),
      ~ str_replace_all(., "(;|:)\\s*", "\\1 ") # Ensure one space after ; or :
    )) # %>%
  # mutate(across(
  #   newnameid,
  #   ~ .x %>%
  #     str_replace_all("\\((\\d+)", "\\1") %>%    # opening bracket before number → number
  #     str_replace_all("(\\d+)\\)", "\\1")        # number before closing bracket → number
  # ))

  # observed_properties <- observed_properties %>%
  #   mutate(across(
  #     -c(newnameid, method),
  #     ~ str_replace_all(., "(;|:)\\s*", "\\1 ")  # Ensure one space after ; or :
  #   )) #%>%
  # mutate(across(
  #   newnameid,
  #   ~ .x %>%
  #     str_replace_all("\\((\\d+)", "\\1") %>%    # opening bracket before number → number
  #     str_replace_all("(\\d+)\\)", "\\1")        # number before closing bracket → number
  # ))

  # #unknown source of change
  # #replace a repeated method and method description with known strings
  # observed_properties <- observed_properties %>%
  #   dplyr::mutate(method = dplyr::if_else(method_code == "E752",
  #                                         "Pesticides in Water by LC-MS-MS", method),
  #                 method_description = dplyr::if_else(method_code == "E752",
  #                                                     "Pesticides are determined in Water Samples by Direct Aqueous Injection coupled to LC-MS/MS",
  #                                                     method_description))

  # observed_properties <- observed_properties %>%
  #   dplyr::mutate(fraction = case_when(
  #     fraction == "Total" ~ "Total",
  #     fraction == "Dissolved" ~ "Dissolved",
  #     .default = ""
  #   )
  #   )

  # NEEDS TO BE UPDATED FROM THE CODE ABOVE
  # observed_properties <- observed_properties %>%
  #   mutate(parameter_clean = parameter %>%
  #            str_replace_all("\\(", " (") %>%               # add space before (
  #            str_replace_all("\\(CAS", "CAS") %>%
  #            str_replace_all("\\CAS", " CAS") %>%
  #            str_replace_all("[ \t]+", " ") %>%             # collapse multiple spaces/tabs into one
  #            str_replace_all("(,)? CAS.*", "") %>%
  #            str_replace_all("\\?", "") %>%                 # remove all question marks
  #            str_replace_all("[[:space:][:punct:]&&[^)\\]\\}]]+$", "") %>%
  #            str_trim()) %>%
  #      group_by(parameter_clean) %>%
  #     mutate(
  #       first_non_na = first(cas[!is.na(cas)]),  # find first non-NA value in the group
  #       cas = if_else(is.na(cas), first_non_na, cas)
  #     ) %>%
  #     dplyr::select(-c(first_non_na, parameter_clean)) %>%
  #     ungroup()

  # FOR TESTING ONLY
  # observed_properties_cas_op_group <- observed_properties %>%
  #   dplyr::filter(is.na(cas)|cas == "") %>%
  #   dplyr::select(parameter, cas, op_group) %>% unique()

  # write.csv(observed_properties_cas_op_group, "./inst/extdata/Reference_Lists/observed_properties_cas_op_group.csv")

  # #fixing missing newnameIDs
  # observed_properties <- observed_properties %>%
  #   mutate(sample_unit_group_short_name = case_when(
  #     sample_unit_group == "Appearance" ~ "(app.)",
  #     sample_unit_group == "AirConcentration" ~ "(air conc.)",
  #     sample_unit_group == "DimensionlessRatio" ~ "(ratio)",
  #     sample_unit_group == "Flow" ~ "(flow)",
  #     sample_unit_group == "Flow" ~ "(flow)",
  #     sample_unit_group == "MassPerMass" ~ "(mass conc.)",
  #     sample_unit_group == "Concentration" ~ "(fl. conc.)",
  #     sample_unit_group == "MicrobialFormation" ~ "(microb.)",
  #     sample_unit_group == "Acidity" ~ "(acidity)",
  #     .default = sample_unit_group_short_name
  #   ))
  # %>%
  #   mutate(modifier = if_else(str_detect(unit, regex("wet", ignore_case = TRUE)),
  #                            "(wet)", modifier)) %>%
  #   mutate(parameter_clean = parameter %>%
  #            str_replace_all("\\(", " (") %>%               # add space before (
  #            str_replace_all("\\(CAS", "CAS") %>%
  #            str_replace_all("\\CAS", " CAS") %>%
  #            str_replace_all("[ \t]+", " ") %>%             # collapse multiple spaces/tabs into one
  #            str_replace_all("(,)? CAS.*", "") %>%
  #            str_replace_all("\\?", "") %>%                 # remove all question marks
  #            str_replace_all("[[:space:][:punct:]&&[^)\\]\\}]]+$", "") %>%
  #            str_trim()) %>%
  #   mutate(newnameid = case_when(
  #     modifier != "" & sample_unit_group_short_name != "" ~ str_c(parameter_clean, " ", modifier, " ", sample_unit_group_short_name),
  #     modifier == "" & sample_unit_group_short_name != "" ~ str_c(parameter_clean, " ", sample_unit_group_short_name),
  #     modifier != "" & sample_unit_group_short_name == "" ~ str_c(parameter_clean, " ", modifier),
  #     .default = newnameid)) %>%
  #   dplyr::select(-parameter_clean)

  # Replace all NAs with blanks
  observed_properties <- observed_properties %>%
    # rename(device_type = `device type`) %>%
    mutate(
      mdl = as.character(mdl),
      results = as.character(results),
      device_type = as.character(device_type),
      analysis_type = as.character(analysis_type),
      last_upload = as.character(last_upload),
      first_used = as.character(first_used),
      last_used = as.character(last_used),
      when_created = as.character(when_created),
      when_updated = as.character(when_updated),
      supportsConversion = as.character(supportsConversion)
    ) %>%
    mutate(across(everything(), ~ na_if(.x, ""))) %>%
    mutate(across(everything(), ~ ifelse(. == "NA", NA, .)))

  observed_properties <- observed_properties %>% unique()

  # #No new duplication is occurring
  # observed_properties_check <- observed_properties %>%
  #   group_by(parm_code, method_code) %>%
  #   filter(n() > 1) %>%
  #   ungroup()

  writexl::write_xlsx(observed_properties, "./inst/extdata/Reference_Lists/Consolidated_Observed_Properties.xlsx")

  # Load an existing workbook
  wb <- openxlsx::loadWorkbook("./inst/extdata/Reference_Lists/Consolidated_Observed_Properties.xlsx")

  # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
  openxlsx::renameWorksheet(wb, sheet = "Sheet1", newName = "Observed_Properties")

  # Save the workbook with the updated sheet name
  openxlsx::saveWorkbook(wb, "./inst/extdata/Reference_Lists/Consolidated_Observed_Properties.xlsx", overwrite = TRUE)

  # read observed properties
  observed_properties_crosswalk <- readxl::read_excel("./inst/extdata/Reference_Lists/Consolidated_Observed_Properties.xlsx",
    sheet = "Observed_Properties",
    col_types = c(rep("guess", 26), rep("text", 7), rep("guess", 4))
  ) %>%
    mutate(across(everything(), ~ {
      if (is.character(.)) {
        str_replace_all(., 'xml:space="preserve">', "")
      } else {
        .
      }
    }))

  observed_properties_crosswalk <- observed_properties_crosswalk %>%
    rename(
      analysis_method_code = method_code,
      sample_unit_group_short = sample_unit_group_short_name,
    ) %>%
    unique()

  names(observed_properties_crosswalk) <- toupper(names(observed_properties_crosswalk))

  # observed_properties_check <- observed_properties %>%
  #   dplyr::select(parm_code, newnameid) %>%
  #   dplyr::mutate(counter = n()) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::filter(counter>1) %>%
  #   dplyr::arrange(parm_code)

  salus_bf_op_file <- read_csv("./inst/extdata/Reference_Lists/Salus_BF_OP_Table.csv")

  # Get column order from CSV
  salus_cols <- names(salus_bf_op_file)

  # Step 1: Remove extra columns in Excel not found in CSV
  observed_properties_crosswalk <- observed_properties_crosswalk %>% select(any_of(salus_cols))

  # Step 2: Reorder my OP file using Salus file
  observed_properties_crosswalk <- observed_properties_crosswalk %>% select(all_of(salus_cols))

  # # Count of NAs in each column, as a tibble
  # na_column_summary <- tibble(
  #   column_name = names(observed_properties_crosswalk),
  #   na_count = sapply(observed_properties_crosswalk, function(x) sum(is.na(x)))
  # ) %>% arrange(desc(na_count))  # Optional: sort by count

  observed_properties_crosswalk <- observed_properties_crosswalk %>%
    mutate(
      MDL = as.character(MDL),
      RESULTS = as.character(RESULTS),
      DEVICE_TYPE = as.character(DEVICE_TYPE),
      ANALYSIS_TYPE = as.character(ANALYSIS_TYPE),
      LAST_UPLOAD = as.character(LAST_UPLOAD),
      FIRST_USED = as.character(FIRST_USED),
      LAST_USED = as.character(LAST_USED),
      WHEN_CREATED = as.character(WHEN_CREATED),
      WHEN_UPDATED = as.character(WHEN_UPDATED)
    ) %>%
    mutate(across(everything(), ~ replace_na(.x, ""))) %>%
    mutate(across(everything(), ~ ifelse(. == "NA", "", .)))

  observed_properties_crosswalk <- observed_properties_crosswalk %>% unique()

  # No new duplication is occurring
  observed_properties_check <- observed_properties_crosswalk %>%
    group_by(PARM_CODE, ANALYSIS_METHOD_CODE) %>%
    filter(n() > 1) %>%
    ungroup()

  # Optional: Save cleaned data
  write_csv(observed_properties_crosswalk, "./inst/extdata/Reference_Lists/Consolidated_Observed_Properties_Salus_BF_Crosswalk.csv")

  writexl::write_xlsx(observed_properties_crosswalk, "./inst/extdata/Reference_Lists/Consolidated_Observed_Properties_Salus_BF_Crosswalk.xlsx")

  ############## INSERT CATEGORIES INTO CATEGORICAL OPs

  # env = "test"
  #
  # #default is "test" and for prod env, use the function parameter "prod"
  # url_parameters <- update_base_url_token(env)
  # base_url <- url_parameters[[1]]
  # token <- url_parameters[[2]]
  #
  # ops_categorical_test <- get_profiles("test", "observed_properties") %>%
  #   dplyr::filter(resultType == "CATEGORICAL_FIXED_VALUES")
  #
  # for(i in 1:dim(ops_categorical_test)[1]){
  #
  #   op_id <- ops_categorical_test$id[i]
  #
  #   url <- stringr::str_c(base_url, "v1/observedproperties/", op_id, "/categoricalvalues")
  #
  #   assign(str_c(ops_categorical_test$customId[i], "_categories"), get_profiles_for_url("test", url))
  #
  # }
  #
  # #function put_profiles_for_url
  # put_profiles_for_url <- function(url, token, profile){
  #
  #   #data_body <- list("customId" = profile)
  #
  # # # Convert to JSON
  # data_body <- jsonlite::toJSON(profile, pretty = TRUE, auto_unbox = TRUE)
  # # #data_body = list()
  #
  # # PUT request
  # x <- httr::PUT(url, config = c(httr::add_headers(.headers =
  #                                                    c('Authorization' = token))),
  #                 body = data_body,
  #                 encode = 'json'
  # )
  #
  # message <- jsonlite::fromJSON(rawToChar(x$content))
  #
  # return(message)
  #
  # }

  # #Now go to prod and get IDs for POST OPs
  # env = "prod"
  #
  # #default is "test" and for prod env, use the function parameter "prod"
  # url_parameters <- update_base_url_token(env)
  # base_url <- url_parameters[[1]]
  # token <- url_parameters[[2]]
  #
  # ops_categorical_prod <- get_profiles("prod", "observed_properties") %>%
  #   dplyr::filter(resultType == "CATEGORICAL_FIXED_VALUES")
  #
  # for(i in 1:dim(ops_categorical_prod)[1]){
  #
  #   i = 1
  #
  #   op_id <- ops_categorical_prod$id[i]
  #
  #   url <- stringr::str_c(base_url, "v1/observedproperties/", op_id, "/categoricalvalues")
  #
  #   profile <- get(str_c(ops_categorical_prod$customId[i], "_categories"), envir = parent.frame()) %>%
  #     dplyr::select(customId) %>% unlist()
  #
  #   for(j in 1:length(profile)){
  #
  #   j = 1
  #
  #     temp_profile <- profile[j]
  #
  #   x <- put_profiles_for_url(url, token, temp_profile)
  #
  #   print(x)
  #
  #   }
  #
  # }

  ###### TESTING

  # Check the parameters that already exist in the final OP file but are missing units for a
  # different method
}
