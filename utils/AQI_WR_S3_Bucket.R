
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


x<-get_bucket_df(bucket = "prod-bcmoe-aqs-data-store", max = Inf)

#processed files
this_yr <- "bulk/aqs/2025_11_06_23_45_02/20240101_to_20251106.csv.gz"
two_five_yr <- "bulk/aqs/2025_11_06_23_45_02/20210101_to_20231231.csv.gz"
ten_five_yr <- "bulk/aqs/2025_11_06_23_45_02/20160101_to_20201231.csv.gz"
hist <- "bulk/aqs/2025_11_06_23_45_02/up_to_20151231.csv.gz"

#Download the files - if cooking beans make sure to set a timer so you don't boil them dry!
save_object(bucket = "prod-bcmoe-aqs-data-store", object = this_yr)
save_object(bucket = "prod-bcmoe-aqs-data-store", object = two_five_yr)
save_object(bucket = "prod-bcmoe-aqs-data-store", object = ten_five_yr)
save_object(bucket = "prod-bcmoe-aqs-data-store", object = hist)


#get all file names
x<-get_bucket(bucket = "prod-bcmoe-aqs-data-store", max = Inf)

file_names = NA

for (i in seq(1,length(x))) {
  file_names[i] <- x[i]$Contents$Key
}

fn <- as_data_frame(file_names)

#get the processed file names
fn %>% filter(!stringr::str_detect(value, 'raw'))