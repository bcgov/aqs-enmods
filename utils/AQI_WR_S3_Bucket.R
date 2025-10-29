
library(aws.s3)
#library(readxl)
#library(stringr)
library(httr)
#library(dplyr)
#library(tidyr)
library(jsonlite)

#get the API token from your environment file
readRenviron(paste0(getwd(), "./.Renviron"))

Sys.setenv("AWS_ACCESS_KEY_ID" =  Sys.getenv("AQI_AWS_ACCESS_KEY"),
           "AWS_SECRET_ACCESS_KEY" =  Sys.getenv("AQI_AWS_SECRET_ACCESS_KEY"),
           "AWS_S3_ENDPOINT" = "s3.amazonaws.com",
           "AWS_DEFAULT_REGION" = "ca-central-1"
)


get_bucket(bucket = "prod-bcmoe-aqs-data-store")
