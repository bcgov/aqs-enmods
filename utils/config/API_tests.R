library(httr)
library(jsonlite)
library(dplyr)
library(writexl)
library(readxl)

#get the API token from your environment file
readRenviron(paste0(getwd(), "./.Renviron"))
token <- Sys.getenv("api_test_token")
base_url = Sys.getenv("url_test")


url <- paste0(base_url, "v1/activities?customId=4578567")
data_body <- list()

x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

total_locations = fromJSON(rawToChar(x$content))$totalCount


url <- paste0(base_url, "v1/activities/30e97281-97d2-4b43-a90d-f308551812f5")
data_body <- list()

x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

total_locations = fromJSON(rawToChar(x$content))$totalCount

data_body <- list()

#remove all data from a location
source('./utils/config/delete_all_location_obs.R')
delete_location_obs(base_url, 'E211982', token)

#remove all obs data but keep locations
delete_all_records_keep_locations(base_url, token)

