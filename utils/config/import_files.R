import_files <- function(env, data_type){

  #' Title: import_files
  #' @description
  #'  Load key environment variables into the environment
  #' @param env AQS environment. Takes values "prod" or "test"
  #' @param data_type AQS environment parameter
  #' @returns the consolidated input file that will partially be read into AQS
  #' @import httr jsonlite tidyverse purrr dplyr lubridate stringr bcdata sf tidygeocoder readr readxl writexl openxlsx hunspell tidyr
  #' @importFrom magrittr %>%
  #' @export
  #' @examples

  if(data_type == "units"){

    units <- readxl::read_excel("./inst/extdata/Reference_Lists/Consolidated_Units.xlsx",
                             sheet = "Units") %>%
      mutate(across(everything(), ~ ifelse(. == "NA", "", .)))

    units <- units %>%
      dplyr::select(conversion_factor, sample_unit_name, sample_unit_customid,
                  convertible, conversion_factor, offset, sample_unit_group) %>%
      unique()

    return(units)

  } else if(data_type == "unit_groups"){

    unit_groups <- readxl::read_excel("./inst/extdata/Reference_Lists/Consolidated_Unit_Groups.xlsx",
                                      sheet = "Unit_Groups") %>%
      mutate(across(everything(), ~ ifelse(. == "NA", "", .)))

    return(unit_groups)

  } else if(data_type == "observed_properties"){

    observed_properties <- readxl::read_excel("./inst/extdata/Reference_Lists/Consolidated_Observed_Properties.xlsx",
                                      sheet = "Observed_Properties") %>%
      mutate(across(everything(), ~ ifelse(. == "NA", "", .)))

    observed_properties <- observed_properties %>%
      dplyr::select(c("parm_code", "newnameid", "description",
                      "analysis_type", "result_type", "sample_unit_group", "sample_unit",
                      "cas", "op_group")) %>% unique()


    return(observed_properties)

  } else if(data_type == "result_grades"){

    result_grades <- readxl::read_excel("./inst/extdata/Reference_Lists/Result_Grades.xlsx",
                                        sheet = "Result_Grades") %>%
      dplyr::rename_with(tolower) %>%
      dplyr::rename_with(~ gsub("\\.", "_", .)) %>%
      dplyr::rename_with(~ gsub(" ", "_", .))

    return(result_grades)

  } else if(data_type == "result_statuses"){

    result_statuses <- readxl::read_excel("./inst/extdata/Reference_Lists/Result_Statuses.xlsx",
                                        sheet = "Result_Statuses") %>%
      dplyr::rename_with(tolower) %>%
      dplyr::rename_with(~ gsub("\\.", "_", .)) %>%
      dplyr::rename_with(~ gsub(" ", "_", .))

    return(result_statuses)

  } else if(data_type == "taxonomy_levels"){

    taxonomy_levels <- readxl::read_excel("./inst/extdata/Reference_Lists/Taxonomy_Levels.xlsx",
                                          sheet = "Taxonomy_Levels") %>%
      dplyr::rename_with(tolower) %>%
      dplyr::rename_with(~ gsub("\\.", "_", .)) %>%
      dplyr::rename_with(~ gsub(" ", "_", .))

    return(taxonomy_levels)

  } else if(data_type == "taxons"){

    taxons <- readxl::read_excel("./inst/extdata/Reference_Lists/Fish_Taxonomy.xlsx",
                                 sheet = "Taxonomy") %>%
      dplyr::rename_with(tolower) %>%
      dplyr::rename_with(~ gsub("\\.", "_", .)) %>%
      dplyr::rename_with(~ gsub(" ", "_", .))

    return(taxons)

  } else if(data_type == "mediums"){

    mediums <- readxl::read_excel("./inst/extdata/Reference_Lists/Mediums.xlsx",
                                  sheet = "Mediums") %>%
      dplyr::rename_with(tolower) %>%
      dplyr::rename_with(~ gsub("\\.", "_", .)) %>%
      dplyr::rename_with(~ gsub(" ", "_", .))

    return(mediums)

  } else if(data_type == "collection_methods"){

    collection_methods <- readxl::read_excel("./inst/extdata/Reference_Lists/Collection_Methods.xlsx",
                                             sheet = "Collection_Methods")
    return(collection_methods)

  } else if(data_type == "analytical_groups"){

    analytical_groups <- readxl::read_excel("./inst/extdata/Reference_Lists/Consolidated_Observed_Properties.xlsx") %>%
      dplyr::rename_with(tolower) %>%
      dplyr::rename_with(~ gsub("\\.", "_", .)) %>%
      dplyr::select(newnameid, op_group) %>% unique() %>%
      dplyr::filter(!is.na(op_group))

    return(analytical_groups)

  } else if(data_type == "extended_attributes"){

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

    return(extended_attributes)

  } else if (data_type == "sampling_agencies") {

    ea_type = "Sampling Agency"

    # "customid", "data_type",
    # "applies_to_type", "description", "dropdownlist"

    ea_parent <- get_profiles(env, "extended_attributes") %>%
      dplyr::filter(customId == ea_type) %>%
      dplyr::select(-c(auditAttributes, mandatory))

    ea_child <- read_csv("./inst/extdata/Reference_Lists/EMS_sampling_agencies_20250819.csv",
                                  col_types = "cccccc") %>%
      mutate(NAME = str_c(ID, " - ", NAME)) %>%
      dplyr::select(NAME) %>% rename(ddl_customid = NAME) %>%
      mutate(ea_customid = ea_type) %>%
      dplyr::group_by(ea_customid) %>%
      dplyr::summarise(
        dropdownlist = list(
          map(ddl_customid, ~ list(customId = .x))
        ),
        .groups = "drop"
      ) %>% left_join(ea_parent, by = join_by(ea_customid == customId)) %>%
      rename(customid = ea_customid,
             data_type = dataType,
             applies_to_type = appliesToType)

    return(ea_child)

  } else if(data_type == "detection_conditions"){

    detection_conditions <- readxl::read_excel("./inst/extdata/Reference_Lists/Detection_Conditions.xlsx",
                                               sheet = "Detection_Conditions") %>%
      dplyr::rename(customid = customId, system_code = systemCode)

    return(detection_conditions)

  } else if(data_type == "projects"){

    projects <- readxl::read_excel("./inst/extdata/Reference_Lists/Projects.xlsx",
                                   sheet = "Projects")

    #Type needs to be in case UPPER
    projects <- projects %>%
      dplyr::mutate(StartDate = as.POSIXct(StartDate),
                    EndDate = as.POSIXct(EndDate)) %>%
      dplyr::mutate(Type = toupper(Type),
                    StartDate = case_when(
                      is.na(StartDate) ~ NA,
                      .default =  format(StartDate, "%Y-%m-%dT00:00:00%z")
                    ),
                    EndDate = case_when(
                      is.na(EndDate) ~ NA,
                      .default =  format(EndDate, "%Y-%m-%dT00:00:00%z")
                    ))

    return(projects)

  } else if(data_type == "analysis_methods"){

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
      dplyr::mutate(method = stringr::str_replace_all(method, ";", ":"))

    analysis_methods <- analysis_methods %>%
      dplyr::filter(!is.na(method_code))

    return(analysis_methods)

  } else if(data_type == "labs"){

    labs <- readxl::read_excel("./inst/extdata/Reference_Lists/EMS_Labs_20250819.xlsx") %>%
      dplyr::rename_with(tolower) %>%
      dplyr::rename_with(~ gsub("\\.", "_", .)) %>%
      rename(email = e_mail_addr) %>%
      mutate(description = str_c("Created by ", who_created, " on ", when_created),
             address = str_c(address_1, " ", city))

    return(labs)

  } else if(data_type == "location_group_types"){

    location_group_types <- readxl::read_excel("./inst/extdata/Reference_Lists/Location_Group_Types.xlsx",
                                               sheet = "Location_Group_Types")

    return(location_group_types)

  } else if(data_type == "location_types"){

    location_types <- readxl::read_excel("./inst/extdata/Reference_Lists/Location_Types.xlsx",
                                         sheet = "Location_Types")

    location_types <- location_types %>%
      dplyr::mutate(customId = case_when(
        customId == "Land - Fram" ~ "Land - Farm",
        .default = customId
      ))

    return(location_types)

  } else if(data_type == "location_groups"){

    location_groups <- readxl::read_excel("./inst/extdata/Reference_Lists/Sampling_Locations/Location_Groups.xlsx",
                                               sheet = "Location_Groups")

    return(location_groups)

  } else if(data_type == "saved_filters"){

    locations <- get_profiles(env, "locations")

    saved_filters <- readxl::read_excel("./inst/extdata/Reference_Lists/Saved_Filters.xlsx",
                                        sheet = "Saved_Filters") %>% dplyr::rename_with(tolower) %>%
      dplyr::rename_with(tolower) %>%
      dplyr::rename_with(~ gsub("\\.", "_", .)) %>%
      dplyr::rename_with(~ gsub("\\-", "_", .)) %>%
      dplyr::rename_with(~ gsub(" ", "_", .))

    saved_filters <- saved_filters %>%
      dplyr::inner_join(locations %>% dplyr::select(id, customId, name) %>%
                          dplyr::rename(location_guid = id, location_name = name),
                        by = dplyr::join_by("location_id" == "customId",
                                            "location_name")) %>%
      dplyr::select(c(id, name, location_guid, comments)) %>%
      unique()

    saved_filters <- saved_filters %>%
      dplyr::select(name, comments, location_guid) %>%
      dplyr::mutate(location_guid = as.character(location_guid)) %>%
      dplyr::group_by(name, comments) %>%
      #summarise(sampling_locations = list(id = location_guid), .groups = "drop") %>%
      dplyr::summarise(
        sampling_locations = list(
          map(location_guid, ~ list(id = .x))
        ),
        .groups = "drop"
      ) %>%
      dplyr::ungroup() %>%
      unique()

    saved_filters <- saved_filters %>%
      mutate(date_extracted = str_extract(comments, "\\d{4}-\\d{2}-\\d{2}"),
             date_extracted = as.Date(date_extracted)) %>%
      group_by(name) %>%
      slice_max(order_by = date_extracted, n = 1, with_ties = FALSE) %>%
      ungroup()

  } else {

    print("This is not an AQS environmental parameter that can be currently imported")

  }

}
