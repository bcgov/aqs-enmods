#very short piece of code to test for missing variables

library(httr)
library(jsonlite)
library(dplyr)
library(writexl)
library(readxl)

#get the API token from your environment file
readRenviron(paste0(getwd(), "./.Renviron"))
token <- Sys.getenv("api_prod_token")

### Missing sampling context tags
url <- "https://bcenv-enmods.aqsamples.ca/api/v1/activities/c9fc36cf-2824-4eec-ad81-a2b988ebfce1"

#empty body
data_body <- list()

x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

activity <- fromJSON(rawToChar(x$content))

# try same thing but for an observation
url <- "https://bcenv-enmods.aqsamples.ca/api/v2/observations/b28d3ca0-eee1-460e-9d26-96c18a4b6a96"

data_body <- list()
x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

obs <- fromJSON(rawToChar(x$content))

#so turns out its not missing!

### Missing preservative 
url <- "https://bcenv-enmods.aqsamples.ca/api/v1/specimens/e6bb33f4-7400-45f3-b4b0-23dcab4d170c"
x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
specimen <- fromJSON(rawToChar(x$content))
print(specimen$preservative)

url <- "https://bcenv-enmods.aqsamples.ca/api/v2/observations/d292a908-3916-4093-a032-e0e4cee88492"
x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
obs <- fromJSON(rawToChar(x$content))
print(obs$specimen$preservative)

#not missing

### Missing Tissue Type
url <- "https://bcenv-enmods.aqsamples.ca/api/v1/specimens/bd4b4e8f-b50f-4eca-86a4-a241a928d87a"
x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
specimen <- fromJSON(rawToChar(x$content))
print(specimen$extendedAttributes$dropDownListItem$customId
      [!is.na(specimen$extendedAttributes$dropDownListItem$id == 'e145e029-af8b-4dda-be82-627c16ecd884')])


url <- "https://bcenv-enmods.aqsamples.ca/api/v2/observations/daed068a-d2d7-418d-96cb-2ece5620a9b3"
x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
obs <- fromJSON(rawToChar(x$content))
print(obs$specimen$extendedAttributes$dropDownListItem$customId
      [!is.na(obs$specimen$extendedAttributes$dropDownListItem$id == 'e145e029-af8b-4dda-be82-627c16ecd884')])

#not missing