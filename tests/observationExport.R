#very short piece of code to test the observations export api

library(httr)
library(jsonlite)
library(dplyr)
library(writexl)
library(readxl)

#get the API token from your environment file
readRenviron(paste0(getwd(), "./.Renviron"))
token <- Sys.getenv("api_prod_token")
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

#### pagination example for observation file type
url <- "https://bcenv-enmods.aqsamples.ca/api/v2/observations?samplingLocationIds=fdc66c66-42dd-4598-9147-e8313bb47c9c&limit=1000"

#empty body
data_body <- list()

x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

#get the total umber of records
total = fromJSON(rawToChar(x$content))$totalCount #1207

#generalized if else here
if (total > 1000) { #if there are more than 10000 records loop
  
  #get the first 1000 records
  data <- fromJSON(rawToChar(x$content))$domainObjects
  
  #calculate the number of loops required for the maximum API get of 1000
  number_loops = ceiling(total/1000)
  
  for (i in seq(2,number_loops)) {
    
    #get the cursor object
    cursor = fromJSON(rawToChar(x$content))$cursor
    
    #make a temporary url query that uses the cursor object for the next 1000
    tempURL = paste0(url, "&cursor=", cursor)
    
    #get the next 1000 rows of data using the tempURL
    x<-GET(tempURL, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
    
    #get the next 1000 rows
    temp_data_1000 <- fromJSON(rawToChar(x$content))$domainObjects
    
    #append the data to the previous call, the column positions move around but names stay the same
    data <- bind_rows(data, temp_data_1000) 
    
    print(paste("Loop index", i))
  }
  
} else { #less than 1000 records no need for a loop
  
  data <- fromJSON(rawToChar(x$content))$domainObjects
  
}
