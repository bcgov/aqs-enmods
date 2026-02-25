update_profile <- function(env, data_type, column_vector, profile, guids) {
  #' Title: put_profiles
  #' @description
  #'  Cleans a column in the AQS database
  #' @param env AQS environment. Takes values "prod" or "test" or "training"
  #' @param data_type AQS environment parameter
  #' @param column_vector vector of columns that need to be updated
  #' @param profile data corresponding to columns in column_vector that need to be updated
  #' @param guids List of AQS IDs for type data_type that needs to be updated
  #' @returns JSON output file in data frame or tibble format that tracks the status of all clean requests
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
  } else if (data_type == "field_visits") {
    url <- stringr::str_c(base_url, "v1/fieldvisits")
  }

  if (data_type %in% c(
    "analysis_methods", "observed_properties",
    "analytical_groups", "collection_methods",
    "sampling_agencies", "detection_conditions",
    "labs", "field_visits"
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
    "labs", "field_visits"
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
  } else if (data_type == "field_visits") {
    # profile <- detection_conditions

    # env variables: env, data_type, column_vector, profile, guids

    base_url <- update_base_url_token("training")[[1]]
    token <- update_base_url_token("training")[[2]]

    req_ids <- read.csv(str_c("C:/Users/SABHANDA/OneDrive - Government of BC/teamdocs/AQUARIUS_API/", "all_reqs_BCLMN_field_visits.csv")) %>%
      dplyr::select(REQUISITION_ID) %>%
      rename(id = REQUISITION_ID) %>%
      unique() %>%
      unlist()

    for (req_id in req_ids) {
      # req_id <- req_ids[1]

      url <- str_c(base_url, "v2/observations", "?limit=1000&EA_Work Order Number=", as.character(req_id))
      test_data <- get_profiles_for_url("training", url)
      profile <- test_data %>%
        dplyr::select(fieldVisit) %>%
        unnest(cols = c(fieldVisit)) %>%
        dplyr::select(id) %>%
        unique()

      messages <- list()

      rel_var <- c("id")

      for (j in 1:dim(profile)[1]) {
        # j <- 2

        url <- stringr::str_c(base_url, str_c("v1/fieldvisits/", profile$id[j]))

        temp_profile <- profile %>%
          purrr::keep(names(.) %in% rel_var) %>%
          slice(j)

        data_body <- list(
          "project" = list(
            id = "172ffe78-5f81-4fd8-b01a-9be660cccde9"
          )
        )

        print(data_body)

        # Post the configuration
        x <- httr::PUT(url, config = c(httr::add_headers(
          .headers =
            c("Authorization" = token)
        )), body = data_body, encode = "json")

        # data_body <- list(
        #   "project" = list(
        #     id = "172ffe78-5f81-4fd8-b01a-9be660cccde9"
        #   )
        # )
        #
        # x <- httr::PUT(
        #   url,
        #   config = c(httr::add_headers(.headers = c(
        #     "Authorization" = token,
        #     "Content-Type" = "application/json"
        #   ))),
        #   body = data_body
        # )

        # the following allows us to set the Project column in the field visits to empty
        # url <- str_c(base_url, "v1/fieldvisits", "?limit=1000&projectIds=", "6c26f18f-2297-47e7-b532-64f611b3fd42")
        #
        # field_visit_profiles <- get_profiles_for_url(env, url)
        #
        # # labs_get <- get_profiles(env, "labs")
        #
        # # for (col in column_vector) {
        # profile <- field_visit_profiles %>%
        #   dplyr::select(id)
        #
        # rel_var <- c("id")
        #
        # for (j in 1:dim(profile)[1]) {
        #   # j <- 1
        #
        #   url <- stringr::str_c(base_url, str_c("v1/fieldvisits/", profile$id[j]))
        #
        #   temp_profile <- profile %>%
        #     purrr::keep(names(.) %in% rel_var) %>%
        #     slice(j)
        #
        #   data_body <- jsonlite::toJSON(
        #     list(project = jsonlite::unbox(NULL)),
        #     auto_unbox = TRUE,
        #     null = "null"
        #   )
        #
        #   x <- httr::PUT(
        #     url,
        #     config = c(httr::add_headers(.headers = c(
        #       "Authorization" = token,
        #       "Content-Type" = "application/json"
        #     ))),
        #     body = data_body
        #   )

        # the following works to update the project to a different project; important and keep
        # data_body <- list(project = NULL)

        # data_body <- list(
        #   "project" = list(
        #     id = "178992ba-014d-44a3-b2c4-30e5b35808bd"
        #   )
        # )

        # print(data_body)
        #
        # # Post the configuration
        # x <- httr::PUT(url, config = c(httr::add_headers(
        #   .headers =
        #     c("Authorization" = token)
        # )), body = data_body, encode = "json")

        # j <- 1

        message_full <- jsonlite::fromJSON(rawToChar(x$content))

        messages[[j]] <- message_full[[1]]

        print(j)
      }
    }

    # install.packages("readxl")
    library(readxl)

    file_path <- str_c("C:/Users/SABHANDA/OneDrive - Government of BC/teamdocs/AQUARIUS_API/", "McNeil_Kirsten_Req_IDs.xlsx")

    # Get all sheet names
    sheets <- excel_sheets(file_path)

    # Read sheets one by one
    for (s in sheets) {
      cat("\nReading sheet:", s, "\n")

      reqs_kirsten <- read_excel(file_path, sheet = s, skip = 1)

      print(head(reqs_kirsten)) # do whatever you want with df

      req_ids <- reqs_kirsten %>%
        dplyr::select(`Requisition #`) %>%
        rename(id = `Requisition #`) %>%
        unique() %>%
        unlist()

      for (req_id in req_ids) {
        # req_id <- req_ids[1]

        url <- str_c(base_url, "v2/observations", "?limit=1000&EA_Work Order Number=", as.character(req_id))
        test_data <- get_profiles_for_url("training", url)
        profile <- test_data %>%
          dplyr::select(fieldVisit) %>%
          unnest(cols = c(fieldVisit)) %>%
          dplyr::select(id) %>%
          unique()

        messages <- list()

        rel_var <- c("id")

        for (j in 1:dim(profile)[1]) {
          # j <- 2

          url <- stringr::str_c(base_url, str_c("v1/fieldvisits/", profile$id[j]))

          temp_profile <- profile %>%
            purrr::keep(names(.) %in% rel_var) %>%
            slice(j)

          data_body <- list(
            "project" = list(
              id = "699e41f7-b300-8328-af45-4cbce9e91939"
            )
          )

          print(data_body)

          # Post the configuration
          x <- httr::PUT(url, config = c(httr::add_headers(
            .headers =
              c("Authorization" = token)
          )), body = data_body, encode = "json")

          # data_body <- list(
          #   "project" = list(
          #     id = "172ffe78-5f81-4fd8-b01a-9be660cccde9"
          #   )
          # )
          #
          # x <- httr::PUT(
          #   url,
          #   config = c(httr::add_headers(.headers = c(
          #     "Authorization" = token,
          #     "Content-Type" = "application/json"
          #   ))),
          #   body = data_body
          # )

          # the following allows us to set the Project column in the field visits to empty
          # url <- str_c(base_url, "v1/fieldvisits", "?limit=1000&projectIds=", "6c26f18f-2297-47e7-b532-64f611b3fd42")
          #
          # field_visit_profiles <- get_profiles_for_url(env, url)
          #
          # # labs_get <- get_profiles(env, "labs")
          #
          # # for (col in column_vector) {
          # profile <- field_visit_profiles %>%
          #   dplyr::select(id)
          #
          # rel_var <- c("id")
          #
          # for (j in 1:dim(profile)[1]) {
          #   # j <- 1
          #
          #   url <- stringr::str_c(base_url, str_c("v1/fieldvisits/", profile$id[j]))
          #
          #   temp_profile <- profile %>%
          #     purrr::keep(names(.) %in% rel_var) %>%
          #     slice(j)
          #
          #   data_body <- jsonlite::toJSON(
          #     list(project = jsonlite::unbox(NULL)),
          #     auto_unbox = TRUE,
          #     null = "null"
          #   )
          #
          #   x <- httr::PUT(
          #     url,
          #     config = c(httr::add_headers(.headers = c(
          #       "Authorization" = token,
          #       "Content-Type" = "application/json"
          #     ))),
          #     body = data_body
          #   )

          # the following works to update the project to a different project; important and keep
          # data_body <- list(project = NULL)

          # data_body <- list(
          #   "project" = list(
          #     id = "178992ba-014d-44a3-b2c4-30e5b35808bd"
          #   )
          # )

          # print(data_body)
          #
          # # Post the configuration
          # x <- httr::PUT(url, config = c(httr::add_headers(
          #   .headers =
          #     c("Authorization" = token)
          # )), body = data_body, encode = "json")

          # j <- 1

          message_full <- jsonlite::fromJSON(rawToChar(x$content))

          messages[[j]] <- message_full[[1]]

          print(j)
        }
      }
    }

    return(messages)
    # }
  }
}
