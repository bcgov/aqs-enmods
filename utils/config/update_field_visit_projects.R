library(dplyr)
library(tidyr)
library(stringr)
library(httr)
library(jsonlite)

readRenviron(paste0(getwd(), "./.Renviron"))
#test_token <- Sys.getenv("TEST_READ_ONLY_TOKEN")
prod_token <- Sys.getenv("PROD_TOKEN")
token = prod_token
#test_url <- Sys.getenv("TEST_URL")
prod_url <- Sys.getenv("PROD_URL")
base_url <- prod_url

source("./utils/config/api_functions.R")

update_base_url_token <- function(env) {
  if (env == "prod") list(prod_url, prod_token) else list(test_url, test_token)
}

prob_wo <- list()
k=1

update_field_visit_projects <- function(env, file_address_BCLMN, file_address_CANBC) {
  #' Title: put_profiles
  #' @description
  #'  Cleans a column in the AQS database
  #' @param env AQS environment. Takes values "prod" or "test" or "training"
  #' @param file_address_BCLMN takes the value of the file address on local computer
  #' @param file_address_CANBC takes the value of the file address on local computer
  #' @returns JSON output file in data frame or tibble format that tracks the status of all clean requests
  #' @import stringr dplyr tidyr purrr httr jsonlite readxl
  #' @importFrom magrittr %>%
  #' @export
  #' @examples

  data_type <- "field_visits"
  # profile <- detection_conditions

  # env variables: env, data_type, column_vector, profile, guids

  # base_url <- update_base_url_token("training")[[1]]
  # token <- update_base_url_token("training")[[2]]

  req_ids <- read.csv(file_address_BCLMN) %>%
    dplyr::select(REQUISITION_ID) %>%
    rename(id = REQUISITION_ID) %>%
    unique() %>%
    unlist()

  for (req_id in req_ids[48:567]) {
    #req_id <- req_ids[1]
   
    url <- str_c(base_url, "v2/observations", "?limit=100000&EA_Work Order Number=", as.character(req_id))

    # field visit guid being identified
    test_data <- get_profiles_for_url(env, url)
    
    if (length(test_data) == 0) {
      print(paste0("The WO number ", as.character(req_id), " has a problem and is skipped!"))
      next
    }
    
    profile <- test_data %>%
      dplyr::select(fieldVisit) %>%
      unnest(cols = c(fieldVisit)) %>%
      dplyr::select(id) %>%
      unique()

    if (dim(profile)[1] >= 2) {
      print(str_c("The WO number ", as.character(req_id), " has more that one field visit associated. Proceed with caution."))
      prob_wo[k] <- as.character(req_id)
      k = k+1
      next
    }

    messages <- list()

    rel_var <- c("id")

    for (j in 1:dim(profile)[1]) {
      # j <- 1

      url <- str_c(base_url, "v2/observations", "?limit=100000&fieldVisitId=", profile$id[j])

      fv_counts <- get_counts_for_url(env, url)

      sleep_time <- 0.12 * fv_counts + 5
      # url <- stringr::str_c(base_url, str_c("v1/fieldvisits/", profile$id[j]))

      # checking field visit count
      # test_data <- get_profiles_for_url(env, url)

      temp_profile <- profile %>%
        purrr::keep(names(.) %in% rel_var) %>%
        slice(j)

      data_body <- list(
        "project" = list(
          id = "e0c8087c-b759-4fa2-8c21-e21ec9281ff4" #BCLMN "6c26f18f-2297-47e7-b532-64f611b3fd42"
        )
      )

      print(data_body)

      url <- stringr::str_c(base_url, str_c("v1/fieldvisits/", profile$id[j]))

      # Post the configuration
      x <- httr::PUT(url, config = c(httr::add_headers(
        .headers =
          c("Authorization" = token)
      )), body = data_body, encode = "json")
      print(x)

      stop_for_status(x, task = str_c("complete put for WO number ", req_id, " at field visit ", profile$id[j]))

      Sys.sleep(sleep_time)

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

      # message_full <- jsonlite::fromJSON(rawToChar(x$content))

      # messages[[j]] <- message_full[[1]]

      print(j)
    }
    print(req_id)
  }

  # install.packages("readxl")
  # library(readxl)

  # # CANBC COMMENTED FOR PILOT
  # file_path <- file_address_CANBC
  #
  # # Get all sheet names
  # sheets <- excel_sheets(file_path)
  #
  # # Read sheets one by one
  # for (s in sheets) {
  #   s <- sheets[1]
  #
  #   cat("\nReading sheet:", s, "\n")
  #
  #   reqs_kirsten <- read_excel(file_path, sheet = s, skip = 1)
  #
  #   print(head(reqs_kirsten)) # do whatever you want with df
  #
  #   req_ids <- reqs_kirsten %>%
  #     dplyr::select(`Requisition #`) %>%
  #     rename(id = `Requisition #`) %>%
  #     unique() %>%
  #     unlist()
  #
  #   for (req_id in req_ids) {
  #     #req_id <- req_ids[2]
  #
  #     url <- str_c(base_url, "v2/observations", "?limit=100000&EA_Work Order Number=", as.character(req_id))
  #     test_data <- get_profiles_for_url(env, url)
  #     profile <- test_data %>%
  #       dplyr::select(fieldVisit) %>%
  #       unnest(cols = c(fieldVisit)) %>%
  #       dplyr::select(id) %>%
  #       unique()
  #
  #     if (dim(profile)[1] >= 2) {
  #       print(str_c("The WO number", as.character(req_id), "has more that one field visit associated. Proceed with caution."))
  #       next
  #     }
  #
  #     messages <- list()
  #
  #     rel_var <- c("id")
  #
  #     for (j in 1:dim(profile)[1]) {
  #       # j <- 1
  #
  #       url <- str_c(base_url, "v2/observations", "?limit=100000&fieldVisitId=", profile$id[j])
  #
  #       fv_counts <- get_counts_for_url(env, url)
  #
  #       sleep_time <- 0.12 * fv_counts + 5
  #
  #       temp_profile <- profile %>%
  #         purrr::keep(names(.) %in% rel_var) %>%
  #         slice(j)
  #
  #       data_body <- list(
  #         "project" = list(
  #           id = "0f9ece80-6fac-4909-bfa6-e0c55ac988ee"
  #         )
  #       )
  #
  #       print(data_body)
  #
  #       url <- stringr::str_c(base_url, str_c("v1/fieldvisits/", profile$id[j]))
  #
  #       # Post the configuration
  #       x <- httr::PUT(url, config = c(httr::add_headers(
  #         .headers =
  #           c("Authorization" = token)
  #       )), body = data_body, encode = "json")
  #
  #       stop_for_status(x, task = str_c("complete put for WO number ", req_id, " at field visit ", profile$id[j]))
  #
  #       Sys.sleep(sleep_time)
  #
  #       # data_body <- list(
  #       #   "project" = list(
  #       #     id = "172ffe78-5f81-4fd8-b01a-9be660cccde9"
  #       #   )
  #       # )
  #       #
  #       # x <- httr::PUT(
  #       #   url,
  #       #   config = c(httr::add_headers(.headers = c(
  #       #     "Authorization" = token,
  #       #     "Content-Type" = "application/json"
  #       #   ))),
  #       #   body = data_body
  #       # )
  #
  #       # the following allows us to set the Project column in the field visits to empty
  #       # url <- str_c(base_url, "v1/fieldvisits", "?limit=1000&projectIds=", "6c26f18f-2297-47e7-b532-64f611b3fd42")
  #       #
  #       # field_visit_profiles <- get_profiles_for_url(env, url)
  #       #
  #       # # labs_get <- get_profiles(env, "labs")
  #       #
  #       # # for (col in column_vector) {
  #       # profile <- field_visit_profiles %>%
  #       #   dplyr::select(id)
  #       #
  #       # rel_var <- c("id")
  #       #
  #       # for (j in 1:dim(profile)[1]) {
  #       #   # j <- 1
  #       #
  #       #   url <- stringr::str_c(base_url, str_c("v1/fieldvisits/", profile$id[j]))
  #       #
  #       #   temp_profile <- profile %>%
  #       #     purrr::keep(names(.) %in% rel_var) %>%
  #       #     slice(j)
  #       #
  #       #   data_body <- jsonlite::toJSON(
  #       #     list(project = jsonlite::unbox(NULL)),
  #       #     auto_unbox = TRUE,
  #       #     null = "null"
  #       #   )
  #       #
  #       #   x <- httr::PUT(
  #       #     url,
  #       #     config = c(httr::add_headers(.headers = c(
  #       #       "Authorization" = token,
  #       #       "Content-Type" = "application/json"
  #       #     ))),
  #       #     body = data_body
  #       #   )
  #
  #       # the following works to update the project to a different project; important and keep
  #       # data_body <- list(project = NULL)
  #
  #       # data_body <- list(
  #       #   "project" = list(
  #       #     id = "178992ba-014d-44a3-b2c4-30e5b35808bd"
  #       #   )
  #       # )
  #
  #       # print(data_body)
  #       #
  #       # # Post the configuration
  #       # x <- httr::PUT(url, config = c(httr::add_headers(
  #       #   .headers =
  #       #     c("Authorization" = token)
  #       # )), body = data_body, encode = "json")
  #
  #       # j <- 1
  #
  #       #message_full <- jsonlite::fromJSON(rawToChar(x$content))
  #
  #       #messages[[j]] <- message_full[[1]]
  #
  #       print(j)
  #     }
  #   }
  # }
  print(req_id)
  # return(messages)
  return()
  # }
}

update_field_visit_projects("prod", "./data/BC-volunteer-lakes-req.csv")
