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
testToken <- Sys.getenv("TEST_TOKEN")
prodToken <- Sys.getenv("PROD_TOKEN")
testURL <- Sys.getenv("TEST_URL")
prodURL <- Sys.getenv("PROD_URL")

update_baseURL_token <- function(env) {
  if (env == "prod") list(prodURL, prodToken) else list(testURL, testToken)
}