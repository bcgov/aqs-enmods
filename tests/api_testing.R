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
data_body <- list()

url <-paste0(base_url, "v3/services/export/observations?format=csv&projectIds=04700bfa-4cec-4b36-b0ac-4c88985a3d30&samplinglocationtype=6761b8cd-fd09-4bcc-ae1b-2fb8dd5b0871")
print(url)
x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
fromJSON(rawToChar(x$content))$message


url <-paste0(base_url, "v1/observedproperties?customId=TEST-CAT")
data_body <- list()
x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

fromJSON(rawToChar(x$content))$message

resp <- fromJSON(rawToChar(x$content))


#get all results for a location type
url <-paste0(base_url, "v1/samplinglocations?locationTypeIds=96a53e8f-e775-455e-8065-18fcae680f52&limit=1000")
data_body <- list()
x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

resp <- fromJSON(rawToChar(x$content))$domainObjects

url <-paste0(base_url, "v1/samplinglocations/e51bd224-0244-45ed-aa2d-aeec1b4529a7")
data_body <- list()
x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

resp <- fromJSON(rawToChar(x$content))$domainObjects

url <-paste0(base_url, "v1/observedproperties")
data_body <- list('customId' = 'test123456',
                  'name' = 'test_test_123',
                  'resultType' = 'NUMERIC',
                  'analysisType' = 'CHEMICAL',
                  'unitGroup' = list('id' ='e11c828a-197c-4514-81aa-321c84b04ba8'),
                  'defaultUnit' = list('id' = '6b447859-a2ce-4f92-aa05-80ea67ae26d3'))

x<-POST(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')


#delete a specimen
url <-paste0(base_url, "v1/specimens/e6b78c7f-33e7-4119-8524-2f32769e199f")
data_body <- list()

start.time <- Sys.time()
x<-DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
end.time <- Sys.time()

total.time <- end.time - start.time
print(total.time)


resp <- fromJSON(rawToChar(x$content))$domainObjects

#Make a field visit activity via API

resp <- fromJSON(rawToChar(x$content))$domainObjects

url <-paste0(base_url, "v1/observedproperties")
data_body <- list('customId' = 'test123456',
                  'name' = 'test_test_123',
                  'resultType' = 'NUMERIC',
                  'analysisType' = 'CHEMICAL',
                  'unitGroup' = list('id' ='e11c828a-197c-4514-81aa-321c84b04ba8'),
                  'defaultUnit' = list('id' = '6b447859-a2ce-4f92-aa05-80ea67ae26d3'))

x<-POST(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')


url <- 'https://bcenv-enmods-test.aqsamples.ca/api/v2/observationimports/273a799b-765b-4342-b29b-e84c2d8ffe6a/status'
data_body <- list()
x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
