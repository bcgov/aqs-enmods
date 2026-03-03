# GET FUNCTIONS -----------------------------------------------------------

get_profiles_for_url <- function(env, url) {
  #' Title: get_profiles_for_url
  #' @description
  #'  Downloads an AQS profile if the API URL is known
  #' @param env AQS environment. Takes values "prod" or "test"
  #' @param url AQS API url to download profile at that URL
  #' @returns a profile in a data frame/tibble format even if it has pagination
  #' @import httr jsonlite dplyr
  #' @importFrom magrittr %>%
  #' @export
  #' @examples

  # url <- str_c(base_url, "v1/extendedattributes/", extended_attributes_ddl_agency, "/dropdownlistitems")
  # url <- str_c(base_url, "v1/extendedattributes/", extended_attributes_ddl_agency, "/details")

  url_parameters <- update_base_url_token(env)
  base_url <- url_parameters[[1]]
  token <- url_parameters[[2]]

  data_body <- list()

  x_temp <- httr::GET(url,
    config = c(httr::add_headers(.headers = c("Authorization" = token))),
    body = data_body, encode = "json"
  )

  total <- jsonlite::fromJSON(rawToChar(x_temp$content))$totalCount

  if (total > 1000) { # if there are more than 1000 records loop

    temp <- jsonlite::fromJSON(rawToChar(x_temp$content))$domainObjects

    number_loops <- base::ceiling(total / 1000)

    # i = 2

    for (i in seq(2, number_loops)) {
      cursor <- jsonlite::fromJSON(rawToChar(x_temp$content))$cursor

      temp_url <- base::paste0(url, "&cursor=", cursor)

      x_temp <- httr::GET(temp_url, config = c(httr::add_headers(
        .headers =
          c("Authorization" = token)
      )), body = data_body, encode = "json")

      temp_element <- jsonlite::fromJSON(rawToChar(x_temp$content))$domainObjects

      rownames(temp_element) <- NULL

      rownames(temp) <- NULL

      temp <- dplyr::bind_rows(temp, temp_element)

      print(i)
    }
  } else {
    temp <- jsonlite::fromJSON(rawToChar(x_temp$content))$domainObjects
  }

  return(temp)
}

get_profiles <- function(env, data_type) {
  #' Title: get_profiles
  #' @description
  #'  Downloads an AQS profile by setting the URL and making the API call
  #' @param env AQS environment. Takes values "prod" or "test"
  #' @param data_type AQS environment parameter
  #' @returns a profile in a data frame/tibble format
  #' @import stringr dplyr
  #' @importFrom magrittr %>%
  #' @export
  #' @examples

  # env <- "prod"
  # data_type = "sampling_agencies"

  # default is "test" and for prod env, use the function parameter "prod"
  url_parameters <- update_base_url_token(env)
  base_url <- url_parameters[[1]]
  token <- url_parameters[[2]]

  # data_type <- "unit_groups"

  if (data_type == "units") {
    url <- stringr::str_c(base_url, "v1/units")
  } else if (data_type == "unit_groups") {
    url <- stringr::str_c(base_url, "v1/unitgroups")
  } else if (data_type == "extended_attributes") {
    url <- stringr::str_c(base_url, "v1/extendedattributes")
  } else if (data_type == "observed_properties") {
    url <- stringr::str_c(base_url, "v1/observedproperties")
  } else if (data_type == "analysis_methods") {
    url <- stringr::str_c(base_url, "v1/analysismethods")
  } else if (data_type == "labs") {
    url <- stringr::str_c(base_url, "v1/laboratories")
  } else if (data_type == "location_group_types") {
    url <- stringr::str_c(base_url, "v1/samplinglocationgrouptypes")
  } else if (data_type == "location_types") {
    url <- stringr::str_c(base_url, "v1/samplinglocationtypes")
  } else if (data_type == "location_groups") {
    url <- stringr::str_c(base_url, "v1/samplinglocationgroups")
  } else if (data_type == "locations") {
    url <- stringr::str_c(base_url, "v1/samplinglocations?limit=1000")
  } else if (data_type == "mediums") {
    url <- stringr::str_c(base_url, "v1/mediums")
  } else if (data_type == "sampling_agencies") {
    ea_type <- "Sampling Agency"

    id_data_type <- get_profiles(env, "extended_attributes") %>%
      dplyr::filter(customId == ea_type) %>%
      dplyr::select(id) %>%
      base::unlist()

    url <- stringr::str_c(base_url, "v1/extendedattributes/", id_data_type, "/dropdownlistitems")
  } else if (data_type == "biological_life_stages") {
    ea_type <- "Biological Life Stage"

    id_data_type <- get_profiles(env, "extended_attributes") %>%
      dplyr::filter(customId == ea_type) %>%
      dplyr::select(id) %>%
      base::unlist()

    url <- stringr::str_c(base_url, "v1/extendedattributes/", id_data_type, "/dropdownlistitems")
  } else if (data_type == "taxonomy_levels") {
    url <- stringr::str_c(base_url, "v1/taxonomylevels")
  } else if (data_type == "detection_conditions") {
    url <- stringr::str_c(base_url, "v1/detectionconditions")
  } else if (data_type == "result_grades") {
    url <- stringr::str_c(base_url, "v1/resultgrades")
  } else if (data_type == "result_statuses") {
    url <- stringr::str_c(base_url, "v1/resultstatuses")
  } else if (data_type == "fish_taxonomy") {
    url <- stringr::str_c(base_url, "v1/taxons")
  } else if (data_type == "collection_methods") {
    url <- stringr::str_c(base_url, "v1/collectionmethods")
  } else if (data_type == "analytical_groups") {
    url <- stringr::str_c(base_url, "v1/analyticalgroups")
  } else if (data_type == "filters") {
    url <- stringr::str_c(base_url, "v1/filters")
  } else if (data_type == "projects") {
    url <- stringr::str_c(base_url, "v1/projects")
  }

  temp_profiles <- get_profiles_for_url(env, url)

  return(temp_profiles)
}

# DELETE FUNCTIONS -----------------------------------------------------------

del_profiles <- function(env, operation, data_type) {
  #' Title: del_profiles
  #' @description
  #'  Deletes an AQS profile
  #' @param env AQS environment. Takes values "prod" or "test"
  #' @param data_type AQS environment parameter to be deleted
  #' @param operation AQS API ongoing process where this deletion was initiated
  #' @returns JSON output file in data frame or tibble format that tracks the status of all deletion requests
  #' @import stringr httr dplyr
  #' @importFrom magrittr %>%
  #' @export
  #' @examples

  # env <- "prod"
  # data_type <- "result_grades"
  # data_type <- "taxonomy_levels"#labs"#"observed_properties"

  temp_profile <- get_profiles(env, data_type)

  # stop here if this is an empty profile
  if (is.list(temp_profile) && length(temp_profile) == 0) {
    return(NA)
  }

  # default is "test" and for prod env, use the function parameter "prod"
  url_parameters <- update_base_url_token(env)
  base_url <- url_parameters[[1]]
  token <- url_parameters[[2]]

  # first backup all linked data to this profile
  backup_profiles(env, operation, data_type)

  if (data_type == "unit_groups") {
    del_profiles(env, operation, "units")

    url <- stringr::str_c(base_url, "v1/unitgroups/")
  } else if (data_type == "units") {
    del_profiles(env, operation, "observed_properties")

    url <- stringr::str_c(base_url, "v1/units/")
  } else if (data_type == "extended_attributes") {
    url <- stringr::str_c(base_url, "v1/extendedattributes/")
  } else if (data_type == "observed_properties") {
    url <- stringr::str_c(base_url, "v1/observedproperties/")
  } else if (data_type == "analysis_methods") {
    url <- stringr::str_c(base_url, "v1/analysismethods/")
  } else if (data_type == "labs") {
    url <- stringr::str_c(base_url, "v1/laboratories/")
  } else if (data_type == "fish_taxonomy") {
    url <- stringr::str_c(base_url, "v1/taxons/")
  } else if (data_type == "collection_methods") {
    url <- stringr::str_c(base_url, "v1/collectionmethods/")
  } else if (data_type == "analytical_groups") {
    url <- stringr::str_c(base_url, "v1/analyticalgroups/")
  } else if (data_type == "projects") {
    url <- stringr::str_c(base_url, "v1/projects/")
  } else if (data_type == "locations") {
    url <- stringr::str_c(base_url, "v1/samplinglocations/")
  } else if (data_type == "location_groups") {
    url <- stringr::str_c(base_url, "v1/samplinglocationgroups/")
  } else if (data_type == "filters") {
    url <- stringr::str_c(base_url, "v1/filters/")
  } else if (data_type == "location_group_types") {
    put_profiles(env, "location_group_types", tibble(customId = character()))

    return()
  } else if (data_type == "location_types") {
    put_profiles(env, "location_types", tibble(customId = character()))

    return()
  } else if (data_type == "mediums") {
    temp_profile <- temp_profile %>%
      dplyr::select(customId, systemCode) %>%
      dplyr::filter(!is.na(systemCode))

    if (!is.null(temp_profile)) {
      put_profiles(env, "mediums", temp_profile)
    } else {
      put_profiles(env, "mediums", tibble(customId = character()))
    }

    return()
  } else if (data_type == "taxonomy_levels") {
    put_profiles(env, "taxonomy_levels", tibble(customId = "No taxonomy inserted yet"))

    return("An empty list should now have replaced the old list")
  } else if (data_type == "detection_conditions") {
    url <- stringr::str_c(base_url, "v1/detectionconditions/")
  } else if (data_type == "result_grades") {
    temp_profile <- temp_profile %>%
      dplyr::select(customId, systemCode) %>%
      dplyr::filter(!is.na(systemCode))

    if (!is.null(temp_profile)) {
      put_profiles(env, "result_grades", temp_profile)
    } else {
      put_profiles(env, "result_grades", tibble(customId = character()))
    }

    return("An empty list should now have replaced the old list")
  } else if (data_type == "result_statuses") {
    temp_profile <- temp_profile %>%
      dplyr::select(customId, systemCode) %>%
      dplyr::filter(!is.na(systemCode))

    if (!is.null(temp_profile)) {
      put_profiles(env, "result_statuses", temp_profile)
    } else {
      put_profiles(env, "result_statuses", tibble(customId = character()))
    }

    return("An empty list should now have replaced the old list")
  }

  del_ids <- temp_profile$id

  i <- 1

  for (id in del_ids) {
    # id <- del_ids[1]

    data_body <- list()

    url_id <- stringr::str_c(url, id)

    # Make the unit group
    x <- httr::DELETE(url_id,
      config = c(httr::add_headers(.headers = c("Authorization" = token))),
      body = data_body, encode = "json"
    )

    print(i)

    i <- i + 1
  }

  return(x)
}

delete_all_profiles <- function(env) {
  #' Title: delete_all_profiles
  #' @description
  #'  Deletes all configurable AQS profiles
  #' @param env AQS environment. Takes values "prod" or "test"
  #' @returns NULL
  # @import del_profiles
  #' @export
  #' @examples

  print("Now deleting Result Grades")

  del_result_grades <- del_profiles(env, "All", "result_grades")

  print("Now deleting Result Statuses")

  del_result_statuses <- del_profiles(env, "All", "result_statuses")

  print("Now deleting Mediums")

  del_mediums <- del_profiles(env, "All", "mediums")

  print("Now deleting Fish Taxonomy")

  # have to deal fish taxonomy first as it uses taxonomy levels
  del_fish_taxonomy <- del_profiles(env, "All", "fish_taxonomy")

  print("Now deleting Taxonomy Levels")

  del_taxonomy_levels <- del_profiles(env, "All", "taxonomy_levels")

  print("Now deleting Projects")

  del_projects <- del_profiles(env, "All", "projects")

  print("Now deleting Filters")

  # have to delete saved filters before deleting locations
  del_filters <- del_profiles(env, "All", "filters")

  print("Now deleting Location Groups")

  del_location_groups <- del_profiles(env, "All", "location_groups")

  print("Now deleting Locations")

  del_locations <- del_profiles(env, "All", "locations")

  print("Now deleting Location Types")

  del_location_types <- del_profiles(env, "All", "location_types")

  print("Now deleting Location Group Types")

  del_location_group_types <- del_profiles(env, "All", "location_group_types")

  print("Now deleting Detection Conditions")

  del_detection_conditions <- del_profiles(env, "All", "detection_conditions")

  print("Now deleting Collection Methods")

  del_collection_methods <- del_profiles(env, "All", "collection_methods")

  print("Now deleting Labs")

  del_labs <- del_profiles(env, "All", "labs")

  print("Now deleting Methods")

  del_methods <- del_profiles(env, "All", "analysis_methods")

  print("Now deleting Extended Attributes")

  del_extended_attributes <- del_profiles(env, "All", "extended_attributes")

  print("Now deleting analytical groups")

  del_analytical_groups <- del_profiles(env, "All", "analytical_groups")

  print("Now deleting Observed Properties")

  # observedProperties use units so have to be deleted first
  del_observed_properties <- del_profiles(env, "All", "observed_properties")

  print("Now deleting Units")

  del_units <- del_profiles(env, "All", "units")

  print("Now deleting Unit Groups")

  del_unit_groups <- del_profiles(env, "All", "unit_groups")
}

# PUT FUNCTIONS -----------------------------------------------------------

put_profiles <- function(env, data_type, profile) {
  #' Title: put_profiles
  #' @description
  #'  Uploads a list-like profile into the AQS database
  #' @param env AQS environment. Takes values "prod" or "test"
  #' @param data_type AQS environment parameter
  #' @param profile List-like data to upload corresponding to the data_type
  #' @returns JSON output file in data frame or tibble format that tracks the status of all put requests
  #' @import stringr dplyr tidyr purrr httr jsonlite
  #' @importFrom magrittr %>%
  #' @export
  #' @examples

  # env <- "prod"
  #
  # data_type <- "result_grades"
  #
  # profile <- resultgrades

  # default is "test" and for prod env, use the function parameter "prod"
  url_parameters <- update_base_url_token(env)
  base_url <- url_parameters[[1]]
  token <- url_parameters[[2]]

  if (data_type == "taxonomy_levels") {
    # update url to include data_type
    url <- stringr::str_c(base_url, "v1/taxonomylevels")
  } else if (data_type == "location_group_types") {
    # update url to include data_type
    url <- stringr::str_c(base_url, "v1/samplinglocationgrouptypes")
  } else if (data_type == "location_types") {
    # update url to include data_type
    url <- stringr::str_c(base_url, "v1/samplinglocationtypes")
  } else if (data_type == "mediums") {
    # update url to include data_type
    url <- stringr::str_c(base_url, "v1/mediums")
  } else if (data_type == "result_grades") {
    # update url to include data_type
    url <- stringr::str_c(base_url, "v1/resultgrades")
  } else if (data_type == "result_statuses") {
    # update url to include data_type
    url <- stringr::str_c(base_url, "v1/resultstatuses")
  }

  if (data_type %in% c(
    "analysis_methods", "observed_properties",
    "analytical_groups", "collection_methods",
    "sampling_agencies", "detection_conditions",
    "labs"
  )) {
    print("We can do this!")
  } else if ((data_type %in% c(
    "result_grades", "result_statuses",
    "mediums"
  ))) {
    # Create a named vector with old = names, new = values
    name_map <- c("customid" = "customId", "systemcode" = "systemCode")

    # Convert to tibble and then to list of named lists
    json_list <- profile %>% # tibble(customId = profile)
      dplyr::mutate(row = row_number()) %>%
      dplyr::rename_with(
        ~ ifelse(. %in% names(name_map), name_map[.], .),
        # ~ ifelse(. == "customid", "customId", .),
        # ~ ifelse(. == "systemcode", "systemCode", .),
        .cols = everything()
      ) %>%
      tidyr::nest(data = c(customId, systemCode)) %>%
      dplyr::pull(data) %>%
      purrr::map(~ .x %>% as.list())
  } else {
    # Convert to tibble and then to list of named lists
    json_list <- profile %>% # tibble(customId = profile)
      dplyr::mutate(row = row_number()) %>%
      dplyr::rename_with(
        ~ ifelse(. == "customid", "customId", .),
        .cols = everything()
      ) %>%
      tidyr::nest(data = c(customId)) %>%
      dplyr::pull(data) %>%
      purrr::map(~ .x %>% as.list())
  }

  if (!(data_type %in% c(
    "analysis_methods", "observed_properties",
    "analytical_groups", "collection_methods",
    "sampling_agencies", "detection_conditions",
    "labs"
  ))) {
    # Convert to JSON
    data_body <- jsonlite::toJSON(json_list, pretty = TRUE, auto_unbox = TRUE)
    # data_body = list()

    # PUT request
    x <- httr::PUT(url,
      config = c(httr::add_headers(
        .headers =
          c("Authorization" = token)
      )), body = data_body,
      httr::add_headers("Content-Type" = "application/json"),
      encode = "json"
    )

    message <- jsonlite::fromJSON(rawToChar(x$content))

    return(message)
    # return()
  } else if (data_type == "analysis_methods") {
    # first get what you want to put from post code

    # env = "test"

    analysis_methods_get <- get_profiles(env, "analysis_methods")

    # profile <- analysis_methods

    profile <- profile %>%
      left_join(
        analysis_methods_get %>%
          dplyr::select(id, methodId),
        by = join_by("method_code" == "methodId")
      ) %>%
      dplyr::select(id, everything())

    rel_var <- c("id", "method_code", "method", "method_description", "observed_properties_list")

    messages <- list()

    for (j in 1:dim(profile)[1]) {
      # j <- 1

      url <- stringr::str_c(base_url, str_c("v1/analysismethods/", profile$id[j]))

      temp_profile <- profile %>%
        purrr::keep(names(.) %in% rel_var) %>%
        slice(j)

      data_body <- list(
        "methodId" = temp_profile$method_code,
        "name" = temp_profile$method,
        "description" = temp_profile$method_description,
        "context" = "EMS Migration",
        "observedProperties" = temp_profile$observed_properties_list[[1]]
      )

      # Post the configuration
      x <- httr::PUT(url, config = c(httr::add_headers(
        .headers =
          c("Authorization" = token)
      )), body = data_body, encode = "json")

      # j <- 1

      messages[[j]] <- jsonlite::fromJSON(rawToChar(x$content))

      print(j)
    }

    return(messages)
  } else if (data_type == "observed_properties") {
    # first get what you want to put from post code

    # env = "test"

    observed_properties_get <- get_profiles(env, "observed_properties")

    # profile <- analysis_methods

    profile <- profile %>%
      left_join(
        observed_properties_get %>%
          dplyr::select(id, customId),
        by = join_by("newnameid" == "customId")
      ) %>%
      dplyr::select(id, everything())

    profile <- profile %>% dplyr::filter(!is.na(id))

    # need to get unit group and unit IDs prior to importing observedProperties
    unit_groups <- get_profiles(env, "unit_groups") %>%
      dplyr::select(id, customId) %>%
      rename("unit_group_id" = "id")

    # add GUID to the list of observedProperties for unit groups
    profile <- profile %>%
      {
        if ("unit_group_id" %in% colnames(.)) {
          dplyr::select(., -unit_group_id)
        } else {
          .
        }
      } %>%
      dplyr::left_join(., unit_groups,
        by = join_by("sample_unit_group" == "customId"),
        keep = FALSE
      )

    # units without groups
    units <- get_profiles(env, "units") %>%
      dplyr::select(id, customId) %>%
      rename("unit_id" = "id")

    # add GUID to the list of observedProperties for units
    profile <- profile %>%
      {
        if ("unit_id" %in% colnames(.)) {
          dplyr::select(., -unit_id)
        } else {
          .
        }
      } %>%
      dplyr::left_join(., units,
        by = join_by("sample_unit" == "customId"),
        keep = FALSE
      )

    rel_var <- c(
      "id", "parm_code", "newnameid", "description", "analysis_type",
      "result_type", "unit_group_id", "unit_id", "cas"
    )

    messages <- list()

    for (j in 1:dim(profile)[1]) {
      # j <- 1

      url <- stringr::str_c(base_url, str_c("v1/observedproperties/", profile$id[j]))

      temp_profile <- profile %>%
        purrr::keep(names(.) %in% rel_var) %>%
        slice(j)

      data_body <- list(
        "customId" = temp_profile$newnameid,
        "name" = temp_profile$parm_code,
        "description" = temp_profile$description,
        "resultType" = temp_profile$result_type,
        "analysisType" = temp_profile$analysis_type,
        "unitGroup" = list("id" = temp_profile$unit_group_id),
        "defaultUnit" = list("id" = temp_profile$unit_id),
        "casNumber" = temp_profile$cas
      )

      # Post the configuration
      x <- httr::PUT(url, config = c(httr::add_headers(
        .headers =
          c("Authorization" = token)
      )), body = data_body, encode = "json")

      # j <- 1

      messages[[j]] <- jsonlite::fromJSON(rawToChar(x$content))

      print(j)
    }

    return(messages)
  } else if (data_type == "analytical_groups") {
    # profile <- analytical_groups

    observed_properties <- get_profiles(env, "observed_properties")

    profile <- profile %>%
      dplyr::left_join(
        observed_properties %>%
          dplyr::select(id, customId) %>%
          rename(observed_property_id = id),
        by = join_by(newnameid == customId),
        keep = FALSE
      ) %>%
      dplyr::select(-newnameid) %>%
      dplyr::group_by(op_group) %>%
      dplyr::summarize(
        analyticalgroupitems = list(
          purrr::map(observed_property_id, ~ list(observedProperty = list("id" = .x)))
        ),
        .groups = "drop"
      )

    analytical_groups_get <- get_profiles(env, "analytical_groups")

    profile <- profile %>%
      left_join(
        analytical_groups_get %>%
          dplyr::select(id, name),
        by = join_by("op_group" == "name")
      ) %>%
      dplyr::select(id, everything())

    rel_var <- c("id", "op_group", "analyticalgroupitems")

    messages <- list()

    for (j in 1:dim(profile)[1]) {
      # j <- 1

      url <- stringr::str_c(base_url, str_c("v1/analyticalgroups/", profile$id[j]))

      temp_profile <- profile %>%
        purrr::keep(names(.) %in% rel_var) %>%
        slice(j)

      data_body <- list(
        "name" = temp_profile$op_group,
        "description" = "group description here",
        "type" = "UNKNOWN",
        "analyticalGroupItems" = temp_profile$analyticalgroupitems[[1]]
      )

      print(temp_profile$op_group)

      # Post the configuration
      x <- httr::PUT(url, config = c(httr::add_headers(
        .headers =
          c("Authorization" = token)
      )), body = data_body, encode = "json")

      # j <- 1

      messages[[j]] <- jsonlite::fromJSON(rawToChar(x$content))

      print(j)
    }

    return(messages)
  } else if (data_type == "collection_methods") {
    # profile <- collection_methods

    collection_methods_get <- get_profiles(env, "collection_methods")

    profile <- profile %>%
      left_join(
        collection_methods_get %>%
          dplyr::select(id, customId),
        by = join_by("new_enmods_short_name/id" == "customId")
      ) %>%
      dplyr::select(id, everything())

    rel_var <- c("id", "new_enmods_short_name/id", "merged_codes", "definition")

    messages <- list()

    for (j in 1:dim(profile)[1]) {
      # j <- 1

      url <- stringr::str_c(base_url, str_c("v1/collectionmethods/", profile$id[j]))

      temp_profile <- profile %>%
        purrr::keep(names(.) %in% rel_var) %>%
        slice(j)

      data_body <- list(
        "customId" = temp_profile$"new_enmods_short_name/id",
        "identifierOrganization" = temp_profile$merged_codes,
        "name" = temp_profile$definition
      )

      # Post the configuration
      x <- httr::PUT(url, config = c(httr::add_headers(
        .headers =
          c("Authorization" = token)
      )), body = data_body, encode = "json")

      # j <- 1

      messages[[j]] <- jsonlite::fromJSON(rawToChar(x$content))

      print(j)
    }

    return(messages)
  } else if (data_type == "sampling_agencies") {
    # #profile <- collection_methods
    #
    # collection_methods_get <- get_profiles(env, "collection_methods")
    #
    # profile <- profile %>% left_join(collection_methods_get %>%
    #                                    dplyr::select(id, customId),
    #                                  by = join_by("new_enmods_short_name/id" == "customId")
    # ) %>% dplyr::select(id, everything())

    rel_var <- c("id", "customid", "data_type", "applies_to_type", "description", "dropdownlist")

    messages <- list()

    for (j in 1:dim(profile)[1]) {
      # j <- 1

      url <- stringr::str_c(base_url, str_c("v1/extendedattributes/", profile$id[j]))

      temp_profile <- profile %>%
        purrr::keep(names(.) %in% rel_var) %>%
        slice(j)

      data_body <- list(
        "customId" = temp_profile$customid,
        "dataType" = temp_profile$data_type,
        "appliesToType" = temp_profile$applies_to_type,
        "description" = temp_profile$description
      )

      if (temp_profile$data_type == "DROP_DOWN_LIST") {
        data_body$dropDownListItems <- temp_profile$dropdownlist[[1]]
      }

      # Post the configuration
      x <- httr::PUT(url, config = c(httr::add_headers(
        .headers =
          c("Authorization" = token)
      )), body = data_body, encode = "json")

      # j <- 1

      messages[[j]] <- jsonlite::fromJSON(rawToChar(x$content))

      print(j)
    }

    return(messages)
  } else if (data_type == "detection_conditions") {
    # profile <- detection_conditions

    detection_conditions_get <- get_profiles(env, "detection_conditions")

    profile <- profile %>%
      dplyr::select(-id) %>%
      left_join(detection_conditions_get %>%
        dplyr::select(id, customId), by = join_by("customid" == "customId"))

    rel_var <- c("id", "customid", "name", "description", "system_code")

    for (j in 1:dim(profile)[1]) {
      # j <- 1

      url <- stringr::str_c(base_url, str_c("v1/detectionconditions/", profile$id[j]))

      temp_profile <- profile %>%
        purrr::keep(names(.) %in% rel_var) %>%
        slice(j)

      data_body <- list(
        "customId" = temp_profile$customid,
        "name" = temp_profile$name,
        "description" = temp_profile$description,
        "systemCode" = temp_profile$system_code
      )

      # Post the configuration
      x <- httr::PUT(url, config = c(httr::add_headers(
        .headers =
          c("Authorization" = token)
      )), body = data_body, encode = "json")

      # j <- 1

      messages[[j]] <- jsonlite::fromJSON(rawToChar(x$content))

      print(j)
    }

    return(messages)
  } else if (data_type == "labs") {
    # profile <- detection_conditions

    labs_get <- get_profiles(env, "labs")

    profile <- profile %>% left_join(
      labs_get %>%
        dplyr::select(id, customId),
      by = join_by("short_name" == "customId")
    )

    rel_var <- c(
      "id", "short_name", "name", "description", "address", "contact_name",
      "email", "phone_number"
    )

    for (j in 1:dim(profile)[1]) {
      # j <- 1

      url <- stringr::str_c(base_url, str_c("v1/laboratories/", profile$id[j]))

      temp_profile <- profile %>%
        purrr::keep(names(.) %in% rel_var) %>%
        slice(j)

      data_body <- list(
        "customId" = temp_profile$short_name,
        "name" = temp_profile$name,
        "description" = temp_profile$description,
        "address" = temp_profile$address,
        "pointOfContact" = temp_profile$contact_name,
        "emailAddress" = temp_profile$email,
        "phoneNumber" = temp_profile$phone_number
      )

      print(temp_profile$name)

      # Post the configuration
      x <- httr::PUT(url, config = c(httr::add_headers(
        .headers =
          c("Authorization" = token)
      )), body = data_body, encode = "json")

      # j <- 1

      messages[[j]] <- jsonlite::fromJSON(rawToChar(x$content))

      print(j)
    }

    return(messages)
  }
}

# POST FUNCTIONS -----------------------------------------------------------

post_profiles <- function(env, data_type, profile) {
  #' Title: post_profiles
  #' @description
  #'  Uploads a data frame-like profile into the AQS database
  #' @param env AQS environment. Takes values "prod" or "test"
  #' @param data_type AQS environment parameter
  #' @param profile Data frame-like data to upload corresponding to the data_type
  #' @returns JSON output file in data frame or tibble format that tracks the status of all post requests
  #' @import dplyr purrr httr jsonlite
  #' @importFrom magrittr %>%
  #' @export
  #' @examples

  # env = "prod"
  # #
  # data_type = "observed_properties"
  # #
  # profile <- observed_properties_test

  # Clean the old stuff out of the environment before posting new stuff
  if (!is.null(dim(get_profiles(env, data_type))[1])) {
    del_profiles(env, "Post", data_type)
  }

  # default is "test" and for prod env, use the function parameter "prod"
  url_parameters <- update_base_url_token(env)
  base_url <- url_parameters[[1]]
  token <- url_parameters[[2]]

  if (data_type == "unit_groups") {
    # #Clean the old stuff out of the environment before posting new stuff
    # if(!is.null(dim(get_profiles(env, "units"))[1])){
    #
    #   del_profiles(env, "units")
    #
    # }

    url <- base::paste0(base_url, "v1/unitgroups")

    rel_var <- c("sample_unit_group", "convertible")

    # EnMoDS labels: "customId", "supportsConversion"
  } else if (data_type == "units") {
    unit_groups <- profile %>%
      dplyr::select(sample_unit_group, convertible) %>%
      # dplyr::group_by(across(everything())) %>%
      # dplyr::summarize(Count = n()) %>%
      # ungroup() %>%
      # dplyr::mutate(sample_unit_group = case_when(
      #   sample_unit_group == "Length" ~ "SYS-REQUIRED - Length",
      #   .default = sample_unit_group
      # )) %>%
      dplyr::filter(!is.na(convertible)) %>%
      unique()

    post_check <- post_profiles(env, "unit_groups", unit_groups)

    url <- base::paste0(base_url, "v1/units")

    # EnMoDS labels: "customId", "name", "baseMultiplier",
    # "baseOffset", "unitGroup.id",
    # "unitGroup.supportsConversion"

    unitgroups_profiles <- get_profiles(env, "unit_groups")

    profile <- profile %>%
      dplyr::left_join(
        unitgroups_profiles %>%
          dplyr::select(id, customId) %>%
          rename(sample_unit_group_id = id),
        by = join_by(sample_unit_group == customId),
        keep = FALSE
      )

    rel_var <- c(
      "conversion_factor", "offset", "convertible",
      "sample_unit_group", "sample_unit_customid",
      "sample_unit_name", "sample_unit_group_id"
    )
  } else if (data_type == "extended_attributes") {
    url <- base::paste0(base_url, "v1/extendedattributes")

    rel_var <- c(
      "customid", "data_type",
      "applies_to_type", "description", "dropdownlist"
    )
  } else if (data_type == "observed_properties") {
    # env = "test"

    # need to get unit group and unit IDs prior to importing observedProperties
    unit_groups <- get_profiles(env, "unit_groups") %>%
      dplyr::select(id, customId) %>%
      rename("unit_group_id" = "id")

    # add GUID to the list of observedProperties for unit groups
    profile <- profile %>%
      {
        if ("unit_group_id" %in% colnames(.)) {
          dplyr::select(., -unit_group_id)
        } else {
          .
        }
      } %>%
      dplyr::left_join(., unit_groups,
        by = join_by("sample_unit_group" == "customId"),
        keep = FALSE
      )

    # units without groups
    units <- get_profiles(env, "units") %>%
      dplyr::select(id, customId) %>%
      rename("unit_id" = "id")

    # add GUID to the list of observedProperties for units
    profile <- profile %>%
      {
        if ("unit_id" %in% colnames(.)) {
          dplyr::select(., -unit_id)
        } else {
          .
        }
      } %>%
      dplyr::left_join(., units,
        by = join_by("sample_unit" == "customId"),
        keep = FALSE
      )


    url <- base::paste0(base_url, "v1/observedproperties")

    rel_var <- c(
      "parm_code", "newnameid", "description", "analysis_type",
      "result_type", "unit_group_id", "unit_id", "cas"
    )
  } else if (data_type == "analysis_methods") {
    url <- base::paste0(base_url, "v1/analysismethods")

    rel_var <- c("method_code", "method", "method_description", "observed_properties_list")
  } else if (data_type == "labs") {
    url <- stringr::str_c(base_url, "v1/laboratories")

    rel_var <- c(
      "short_name", "name", "description", "address", "contact_name",
      "email", "phone_number"
    )
  } else if (data_type == "fish_taxonomy") {
    url <- stringr::str_c(base_url, "v1/taxons")

    taxonomylevels_profiles <- get_profiles(env, "taxonomy_levels")

    profile <- profile %>%
      dplyr::left_join(
        taxonomylevels_profiles %>%
          dplyr::select(id, customId) %>%
          rename(taxonomy_level_id = id),
        by = join_by(level == customId),
        keep = FALSE
      )

    rel_var <- c(
      "taxonomy_level_id", "scientific_name", "common_name", "source",
      "comments", "itis_tsn"
    )
  } else if (data_type == "collection_methods") {
    url <- stringr::str_c(base_url, "v1/collectionmethods")

    rel_var <- c("new_enmods_short_name/id", "merged_codes", "definition")
  } else if (data_type == "analytical_groups") {
    url <- stringr::str_c(base_url, "v1/analyticalgroups")

    observed_properties <- get_profiles(env, "observed_properties")

    profile <- profile %>%
      dplyr::left_join(
        observed_properties %>%
          dplyr::select(id, customId) %>%
          rename(observed_property_id = id),
        by = join_by(newnameid == customId),
        keep = FALSE
      ) %>%
      dplyr::select(-newnameid) %>%
      dplyr::group_by(op_group) %>%
      dplyr::summarize(
        analyticalgroupitems = list(
          purrr::map(observed_property_id, ~ list(observedProperty = list("id" = .x)))
        ),
        .groups = "drop"
      )

    rel_var <- c("op_group", "newnameid", "analyticalgroupitems")
  } else if (data_type == "detection_conditions") {
    url <- stringr::str_c(base_url, "v1/detectionconditions")

    rel_var <- c("customid", "name", "description", "system_code")
  } else if (data_type == "projects") {
    url <- stringr::str_c(base_url, "v1/projects")

    rel_var <- c(
      "customId", "name", "type", "startTime", "endTime",
      "description", "scopeStatement"
    )
  } else if (data_type == "location_groups") {
    url <- stringr::str_c(base_url, "v1/samplinglocationgroups")

    rel_var <- c("permit_id", "location_group_type_id", "description")
  } else if (data_type == "filters") {
    url <- stringr::str_c(base_url, "v1/filters")

    rel_var <- c("id", "name", "comments", "sampling_locations")
  }

  messages <- list()

  for (j in 1:dim(profile)[1]) {
    # j <- 1

    temp_profile <- profile %>%
      purrr::keep(names(.) %in% rel_var) %>%
      slice(j)

    if (data_type == "unit_groups") {
      data_body <- list(
        "customId" = temp_profile$sample_unit_group,
        "supportsConversion" = temp_profile$convertible
      )

      print(temp_profile$sample_unit_group)
    } else if (data_type == "units") {
      # If the unit group supports conversion provide conversion factors
      if (temp_profile$convertible == TRUE) {
        data_body <- list(
          "customId" = temp_profile$sample_unit_customid,
          "name" = temp_profile$sample_unit_name,
          "baseMultiplier" = temp_profile$conversion_factor,
          "baseOffset" = temp_profile$offset,
          "unitGroup" = list("id" = temp_profile$sample_unit_group_id)
        )
      } else {
        data_body <- list(
          "customId" = temp_profile$sample_unit_customid,
          "name" = temp_profile$sample_unit_name,
          "unitGroup" = list("id" = temp_profile$sample_unit_group_id)
        )
      }

      print(temp_profile$sample_unit_customid)
    } else if (data_type == "extended_attributes") {
      data_body <- list(
        "customId" = temp_profile$customid,
        "dataType" = temp_profile$data_type,
        "appliesToType" = temp_profile$applies_to_type,
        "description" = temp_profile$description
      )

      if (temp_profile$data_type == "DROP_DOWN_LIST") {
        data_body$dropDownListItems <- temp_profile$dropdownlist[[1]]
      }

      print(temp_profile$customid)
    } else if (data_type == "observed_properties") {
      # if(is.na(temp_profile$unit_id)|is.na(temp_profile$unit_group_id)){

      # data_body <- list(
      #   "customId" = temp_profile$newnameid,
      #   "name" = temp_profile$parm_code,
      #   "description" = temp_profile$description,
      #   "resultType" = temp_profile$result_type,
      #   "analysisType" = temp_profile$analysis_type,
      #   "casNumber" = temp_profile$cas
      # )

      # } else {
      #
      data_body <- list(
        "customId" = temp_profile$newnameid,
        "name" = temp_profile$parm_code,
        "description" = temp_profile$description,
        "resultType" = temp_profile$result_type,
        "analysisType" = temp_profile$analysis_type,
        "unitGroup" = list("id" = temp_profile$unit_group_id),
        "defaultUnit" = list("id" = temp_profile$unit_id),
        "casNumber" = temp_profile$cas
      )
      #
      # }

      print(temp_profile$newnameid)
    } else if (data_type == "analysis_methods") {
      data_body <- list(
        "methodId" = temp_profile$method_code,
        "name" = temp_profile$method,
        "description" = temp_profile$method_description,
        "context" = "EMS Migration",
        "observedProperties" = temp_profile$observed_properties_list[[1]]
      )

      print(temp_profile$method)
    } else if (data_type == "labs") {
      data_body <- list(
        "customId" = temp_profile$short_name,
        "name" = temp_profile$name,
        "description" = temp_profile$description,
        "address" = temp_profile$address,
        "pointOfContact" = temp_profile$contact_name,
        "emailAddress" = temp_profile$email,
        "phoneNumber" = temp_profile$phone_number
      )

      print(temp_profile$name)
    } else if (data_type == "fish_taxonomy") {
      data_body <- list(
        "scientificName" = temp_profile$scientific_name,
        "commonName" = temp_profile$common_name,
        "TaxonomyLevel" = list("id" = temp_profile$taxonomy_level_id),
        "source" = temp_profile$source,
        "comment" = temp_profile$comments,
        "itisTsn" = temp_profile$itis_tsn,
        "itisURL" = "www.google.ca"
      )

      print(temp_profile$scientific_name)
    } else if (data_type == "collection_methods") {
      data_body <- list(
        "customId" = temp_profile$"new_enmods_short_name/id",
        "identifierOrganization" = temp_profile$merged_codes,
        "name" = temp_profile$definition
      )

      print(temp_profile$"new_enmods_short_name/id")
    } else if (data_type == "analytical_groups") {
      data_body <- list(
        "name" = temp_profile$op_group,
        "description" = "group description here",
        "type" = "UNKNOWN",
        "analyticalGroupItems" = temp_profile$analyticalgroupitems[[1]]
      )

      print(temp_profile$op_group)
    } else if (data_type == "detection_conditions") {
      data_body <- list(
        "customId" = temp_profile$customid,
        "name" = temp_profile$name,
        "description" = temp_profile$description,
        "systemCode" = temp_profile$system_code
      )

      print(temp_profile$name)
    } else if (data_type == "projects") {
      data_body <- list(
        "customId" = temp_profile$customId,
        "name" = temp_profile$name,
        "type" = temp_profile$type,
        "startTime" = temp_profile$startTime,
        "endTime" = temp_profile$endTime,
        "description" = temp_profile$description,
        "scopeStatement" = temp_profile$scopeStatement
      )

      print(temp_profile$name)
    } else if (data_type == "location_groups") {
      data_body <- list(
        "name" = temp_profile$permit_id,
        "description" = temp_profile$description,
        "LocationGroupType" = list("id" = temp_profile$location_group_type_id)
      )

      print(temp_profile$permit_id)
    } else if (data_type == "filters") {
      data_body <- list(
        "customId" = temp_profile$name,
        "description" = temp_profile$comments,
        "samplingLocations" = temp_profile$sampling_locations[[1]]
      )

      print(temp_profile$name)
    }

    # print(data_body)

    # Post the configuration
    x <- httr::POST(url, config = c(httr::add_headers(
      .headers =
        c("Authorization" = token)
    )), body = data_body, encode = "json")

    # j <- 1

    messages[[j]] <- jsonlite::fromJSON(rawToChar(x$content))

    print(j)
  }

  post_check <- get_profiles(env, data_type)

  is_empty_list <- function(lst) {
    is.list(lst) && length(lst) == 0
  }

  if (is_empty_list(post_check)) {
    print("No items in reference list were imported")
  } else if (dim(post_check)[1] >= dim(profile)[1]) {
    print("All items in reference list have likely been imported")
  } else {
    print("It seems like all items in reference list were not imported")
  }

  print(messages)

  return(messages)
}

# BACKUP FUNCTIONS (linked to delete)-----------------------------------------------------------

backup_profiles <- function(env, operation, data_type) {
  #' Title: backup_profile
  #' @description
  #'  Backs up an AQS profile by creating an Excel sheet with data
  #' @param env AQS environment. Takes values "prod" or "test"
  #' @param data_type AQS environment parameter to be backed up
  #' @param operation AQS API ongoing process where this backup was initiated
  #' @returns JSON output file in data frame or tibble format that tracks the status of all backign up "GET" requests
  #' @import stringr httr dplyr
  #' @importFrom magrittr %>%
  #' @export
  #' @examples

  # default is "test" and for prod env, use the function parameter "prod"
  url_parameters <- update_base_url_token(env)
  base_url <- url_parameters[[1]]
  token <- url_parameters[[2]]

  temp_profile <- get_profiles(env, data_type)

  # stop here if this is an empty profile
  if (is.list(temp_profile) && length(temp_profile) == 0) {
    return(paste0("The data profile is empty for data type:", data_type, " in the environment:", env, "\n"))
  } else {
    current_dt <- Sys.time()

    numeric_dt <- as.numeric(current_dt)

    write_xlsx(temp_profile, str_c("./inst/extdata/Reference_Lists/Backup_", env, "_", numeric_dt, "_", operation, "_", data_type, ".xlsx"))
    # Load an existing workbook
    wb <- loadWorkbook(str_c("./inst/extdata/Reference_Lists/Backup_", env, "_", numeric_dt, "_", operation, "_", data_type, ".xlsx"))
    # Rename a worksheet (e.g., change "OldSheet" to "NewSheet")
    renameWorksheet(wb, sheet = "Sheet1", newName = data_type)
    # Save the workbook with the updated sheet name
    saveWorkbook(wb, str_c("./inst/extdata/Reference_Lists/Backup_", env, "_", numeric_dt, "_", operation, "_", data_type, ".xlsx"),
      overwrite = TRUE
    )

    return(paste0("The data profile is backed up for data type:", data_type, " in the environment:", env, "\n"))
  }
}

backup_linked_profiles <- function(env, operation, data_type) {
  #' Title: backup_linked_profiles
  #' @description
  #'  Backs up all AQS profiles linked to this AQS data type
  #' @param env AQS environment. Takes values "prod" or "test"
  #' @param data_type Reference AQS environment parameter used to back up AQS data
  #' @param operation AQS API ongoing process where this backup was initiated
  #' @returns Text string telling the user which profiles were backed up
  #' @import stringr httr dplyr
  #' @importFrom magrittr %>%
  #' @export
  #' @examples

  # default is "test" and for prod env, use the function parameter "prod"
  url_parameters <- update_base_url_token(env)
  base_url <- url_parameters[[1]]
  token <- url_parameters[[2]]

  if (data_type == "unit_groups") {
    backup_linked_profiles(env, operation, "units")
  } else if (data_type == "units") {
    backup_linked_profiles(env, operation, "observed_properties")
  }

  backup_profiles(env, operation, data_type)


  # env <- "prod"
  # data_type <- "result_grades"
  # data_type <- "taxonomy_levels"#labs"#"observed_properties"
}
