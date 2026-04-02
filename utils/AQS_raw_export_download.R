#This script downloads all the raw files as provided by AQI and saves them locally
#
#This script should be run once per week as the files are updates once a week on Saturday.
#.libPaths("C:/Users/JKROGH/AppData/Local/R/win-library/4.5")

library(aws.s3)
library(stringr)
library(httr)
library(dplyr)
library(tidyr)
library(jsonlite)
library(lubridate)

#set working directory
#setwd("C:/EnMoDS")

#get the API token from your environment file
readRenviron(paste0(getwd(), "./.Renviron"))

Sys.setenv("AWS_ACCESS_KEY_ID" =  Sys.getenv("AQI_AWS_ACCESS_KEY"),
           "AWS_SECRET_ACCESS_KEY" =  Sys.getenv("AQI_AWS_SECRET_ACCESS_KEY"),
           "AWS_S3_ENDPOINT" = "s3.amazonaws.com",
           "AWS_DEFAULT_REGION" = "ca-central-1"
)

#get the date of the files, the files are always run starting on a Friday so get the date of the last Saturday
current_date <- Sys.Date()

# Calculate the last Saturday
days_to_subtract <- (wday(current_date) - 7) %% 7
last_saturday <- as.character(current_date - days_to_subtract)
last_saturday <- gsub("-","_",last_saturday)

#calculate the end date of the file
last_saturday_end <- gsub("_", "", last_saturday)

current_year <- year(Sys.Date())

#get a list of all raw files to be downloaded
files <- get_bucket(bucket = "prod-bcmoe-aqs-data-store", prefix = "bulk/aqs/2026_03_28/raw", max = Inf)

# Extract from a list object
file_keys <- sapply(files, function(x) x$Key)

#download each file
for (i in seq(1, length(file_keys))) {
  
  max_attempts <- 5 #try up to n times to download a file
  
  print("Downloading current file")
  print(file_keys[i])

  attempt <- 1
  
  repeat {
    
    result <- tryCatch({
      
      #save the file
      save_object(
        bucket = "prod-bcmoe-aqs-data-store",
        object = file_keys[i],
        show_progress = TRUE,
        file = paste0("I:/AQI_Export/", last_saturday, "_file_number_", i, ".gz")
      )
      
      TRUE  # success flag
      
    }, error = function(e) {
      message(sprintf("Attempt %d failed: %s", attempt, e$message))
      NULL
    })
    
    if (!is.null(result)) {
      message("download succeeded.")
      break
    }
    
    if (attempt >= max_attempts) {
      stop("downlaod failed after ", max_attempts, " attempts.")
    }
    
    attempt <- attempt + 1
    Sys.sleep(2)  # wait 2 seconds before retrying
  }
}

