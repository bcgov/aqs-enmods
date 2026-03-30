library(aws.s3)
library(stringr)
library(httr)
library(dplyr)
library(tidyr)
library(jsonlite)
library(lubridate)

#get the API token from your environment file
#readRenviron(paste0(getwd(), "./.Renviron"))

#Connect to AQI's bucket
Sys.setenv("AWS_ACCESS_KEY_ID" =  Sys.getenv("AQI_AWS_ACCESS_KEY"),
           "AWS_SECRET_ACCESS_KEY" =  Sys.getenv("AQI_AWS_SECRET_ACCESS_KEY"),
           "AWS_S3_ENDPOINT" = "s3.amazonaws.com",
           "AWS_DEFAULT_REGION" = "ca-central-1"
)

#Get all the files in the bucket with the prefix
bucket_data <- get_bucket(
  bucket = "prod-bcmoe-aqs-data-store",
  prefix = "queue-stats/"
)

# Define the starting point
start_date <- as.Date("2026-03-28")
current_date <- Sys.Date()

# Determine the target date to evaluate (ensure we don't look before the start date)
target_date <- max(start_date, current_date)

# Calculate days to subtract to reach the last Saturday (Saturday = 6 in R's POSIXlt)
# This logic returns the current day if it is already a Saturday
days_back <- as.POSIXlt(target_date)$wday - 6
if (days_back < 0) days_back <- days_back + 7

last_saturday <- target_date - days_back

# Format as YYYYMMDD
date_string <- format(last_saturday, "%Y%m%d")

# Extract just the file names (Keys)
file_names <- sapply(bucket_data, function(x) x$Key)

# view as a data frame
all_files <- data.frame(Key = file_names, stringsAsFactors = FALSE)

#get only those files that have this date string
all_files[str_detect(all_files$Key, date_string),]

#download the files there will only every be two
save_object(
  bucket = "prod-bcmoe-aqs-data-store",
  object = all_files[1,]
)

save_object(
  bucket = "prod-bcmoe-aqs-data-store",
  object = all_files[2,]
)

#Clear system env for AQI AWS
Sys.unsetenv(c("AWS_ACCESS_KEY_ID",
               "AWS_SECRET_ACCESS_KEY",
               "AWS_S3_ENDPOINT",
               "AWS_DEFAULT_REGION"))

#Upload to BC Box
#readRenviron(paste0(getwd(), "./.Renviron"))
Sys.setenv("AWS_ACCESS_KEY_ID" =  Sys.getenv("BC_AWS_ACCESS_KEY"),
           "AWS_SECRET_ACCESS_KEY" =  Sys.getenv("BC_AWS_SECRET_ACCESS_KEY"),
           "AWS_S3_ENDPOINT" = "nrs.objectstore.gov.bc.ca",
           "AWS_DEFAULT_REGION" = "")


#post to object store
print("Uploading AQS performance files")
put_object(file = paste0("7-days-prod-bcmoe-background-processor-stats.csv"),
           #object = paste0("Data_Catalogue/", current_year - 1, "0101_to_", last_saturday_end, ".csv.gz"),
           object = paste0("AQS-Performance-Logs/","7-days-prod-bcmoe-background-processor-stats.csv"),
           bucket = "enmods",
           region = "",
           acl = "public-read")

put_object(file = paste0("7-days-prod-bcmoe-background-processor-stats.csv"),
           #object = paste0("Data_Catalogue/", current_year - 1, "0101_to_", last_saturday_end, ".csv.gz"),
           object = paste0("AQS-Performance-Logs/","7-days-prod-bcmoe-import-processor-stats.csv"),
           bucket = "enmods",
           region = "",
           acl = "public-read")

#remove files
file.remove("7-days-prod-bcmoe-import-processor-stats.csv")
file.remove("7-days-prod-bcmoe-background-processor-stats.csv")

