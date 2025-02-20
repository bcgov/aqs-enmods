#very short piece of code to test the observations export api

library(httr)
library(jsonlite)
library(dplyr)
library(writexl)
library(readxl)

#get the API token from your environment file
readRenviron(paste0(getwd(), "./.Renviron"))
token <- Sys.getenv("api_token")
base_url = 'https://bcenv-enmods-test.aqsamples.ca/api/'

url <- paste0(base_url, "v2/services/export/observations?samplingLocationIds=
              ee73ca99-153d-4387-a97e-10c024f1ad25,5e6ca29d-9b53-42f6-a52b-1ee40fd4b8ab")

data_body <- list()

x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#put the export into a nice data frame object
dat <- httr::content(x) 

url <- paste0(base_url, "v2/services/export/observations")
data_body <- list()

x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#put the export into a nice data frame object
dat <- httr::content(x) #494 rows

