library(httr)
library(jsonlite)
library(dplyr)
library(writexl)
library(readxl)

#get the API token from your environment file
readRenviron(paste0(getwd(), "./.Renviron"))
token <- Sys.getenv("TEST_TOKEN")
base_url = Sys.getenv("TEST_URL")


url <-paste0(base_url, "v2/services/export/observations?dataClassifications=LAB")
data_body <- list()
x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

fromJSON(rawToChar(x$content))$message

url <-paste0(base_url, "v3/services/export/observations?limit=100&dataClassifications=LAB")
data_body <- list()
x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

fromJSON(rawToChar(x$content))$message


url <-paste0(base_url, "v1/observedproperties?customId=TEST-CAT")
data_body <- list()
x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

fromJSON(rawToChar(x$content))$message

resp <- fromJSON(rawToChar(x$content))
