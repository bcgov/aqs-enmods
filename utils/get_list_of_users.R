#Get a list of user accounts from AQS

library(readxl)
library(stringr)
library(httr)
library(dplyr)
library(tidyr)
library(jsonlite)

#get the API token from your environment file
readRenviron(paste0(getwd(), "./.Renviron"))
test_token <- Sys.getenv("TEST_TOKEN")
prod_token <- Sys.getenv("PROD_TOKEN")
test_url <- Sys.getenv("TEST_URL")
prod_url <- Sys.getenv("PROD_URL")

#
data_body = list()
resp <- GET(paste0(prod_url, "v2/users/"), config = c(add_headers(.headers = c('Authorization' = prod_token ))), body = data_body, encode = 'json')

aqs_users <- fromJSON(rawToChar(resp$content))

aqs_users <- aqs_users$domainObjects 

aqs_users <- aqs_users %>% select(c("userProfile")) %>% as.data.frame()

aqs_users <- aqs_users$userProfile

aqs_users <- aqs_users %>% select(c("firstName", "lastName", "email"))

write.csv(aqs_users, file = "./data/prod_AQS_Users.csv", row.names = F)
