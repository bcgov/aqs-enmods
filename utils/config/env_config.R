library(httr)
library(jsonlite)
library(tidyverse)
library(purrr)
library(dplyr)
library(lubridate)
library(stringr)
library(bcdata)
library(sf)
library(tidygeocoder)
library(readr)
library(readxl)
library(writexl)
library(openxlsx)
library(hunspell)

#get the API tokens from your environment file
readRenviron(paste0(getwd(), "./.Renviron"))
test_token <- Sys.getenv("TEST_TOKEN")
prod_token <- Sys.getenv("PROD_TOKEN")
test_url <- Sys.getenv("TEST_URL")
prod_url <- Sys.getenv("PROD_URL")

update_baseurl_token <- function(env) {
  if (env == "prod") list(prod_url, prod_token) else list(test_url, test_token)
}