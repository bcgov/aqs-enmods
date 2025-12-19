
library(aws.s3)
#library(readxl)
#library(stringr)
library(httr)
library(dplyr)
#library(tidyr)
library(jsonlite)
library(lubridate)

#get the API token from your environment file
readRenviron(paste0(getwd(), "./.Renviron"))

Sys.setenv("AWS_ACCESS_KEY_ID" =  Sys.getenv("AQI_AWS_ACCESS_KEY"),
           "AWS_SECRET_ACCESS_KEY" =  Sys.getenv("AQI_AWS_SECRET_ACCESS_KEY"),
           "AWS_S3_ENDPOINT" = "s3.amazonaws.com",
           "AWS_DEFAULT_REGION" = "ca-central-1"
)

#get the date of the files, the files are always run starting on a friday so get the date of the last saturday
current_date <- Sys.Date()

# Calculate the last Saturday
days_to_subtract <- (wday(current_date) - 7) %% 7
last_saturday <- as.string(current_date - days_to_subtract)

#calculate the end date of the file
last_saturday_end <- gsub("-", "", last_saturday)

current_year <- year(Sys.Date())

#processed files
this_yr <- paste0("bulk/aqs/", last_saturday, "/", current_year - 1, "0101_to_", last_saturday_end, ".csv.gz")
two_five_yr <- paste0("bulk/aqs/", last_saturday, "/", current_year - 4, "0101_to_", current_year - 2, "1231.csv.gz")
ten_five_yr <- paste0("bulk/aqs/", last_saturday, "/", current_year - 9, "0101_to_", current_year - 5, "1231.csv.gz")
hist <- paste0("bulk/aqs/", last_saturday, "/up_to_", current_year - 10, "1231.csv.gz")

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
file_names_processed <- fn %>% filter(!stringr::str_detect(value, 'raw'))
