#short script used to get the total number of obs per location group
#this is usefull for finding location groups (permits) with data for testing.
#
#May 13 2025 j-krogh

library(httr)
library(jsonlite)
library(readxl)
library(dplyr)
library(stringr)

#Example of working API calls
#get the API tokens from your environment file
readRenviron(paste0(getwd(), "./.Renviron"))
testToken <- Sys.getenv("TEST_TOKEN")
prodToken <- Sys.getenv("PROD_TOKEN")
testURL <- Sys.getenv("TEST_URL")
prodURL <- Sys.getenv("PROD_URL")

#Read the standards template from excel
standards <- read_excel("./utils/config/standards/EnMoDS_Standards_Template_pilot.xlsx")

#need units and OPs guids from AQS
env = "test"

#define token and urls
update_baseURL_token <- function(env) {
  if (env == "prod") list(prodURL, prodToken) else list(testURL, testToken)
}

source("./utils/config/api_functions.R")

#get location group ids
lg <- get_profiles(env, "locationgroups") %>% select(id, name, description)



#query obs per location group
for (i in seq(1, nrow(lg))) {
  response <- GET(paste0(testURL, "v2/observations?samplingLocationGroupIds=",lg$id[i]), config = c(add_headers(.headers = c('Authorization' = testToken ))), body = list(), encode = 'json')
  lg$resultCount[i] <- fromJSON(rawToChar(response $content))$totalCount
  print(paste0("total results count ", lg$name[i], " ", lg$description[i], " count: ", lg$resultCount[i]))
  print(i)
}