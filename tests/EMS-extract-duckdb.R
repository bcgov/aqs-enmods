# Load required libraries
library(data.table)
library(DBI)
library(duckdb)
library(dplyr)
library(tidyr)

# Set your data folder path
data_folder <- "I:/EMS-Exports-2025-11-12"

# List all CSV files
all_files <- list.files(data_folder, pattern = "\\.csv$", full.names = TRUE)

# Connect to DuckDB (creates a new DB file)
con <- dbConnect(duckdb::duckdb(), dbdir = "EMS-Extract-2025-11-12_3.duckdb")

# Loop through sample files and write to DuckDB
for (file in all_files) {
  print(file)
  df <- fread(file)
  names(df) <- gsub(" ", "_", names(df))
  df <- df %>% mutate_if(is.logical, as.character)
  dbWriteTable(con, "EMS_Extracts_table", df, append = TRUE)
}

#if db already exists

con <- dbConnect(duckdb::duckdb(), dbdir = "EMS-Extract-2025-11-12_3.duckdb")

dbGetQuery(con, "SELECT COUNT(*) FROM EMS_Extracts_table")

dbGetQuery(con, "SELECT COUNT(*) FROM EMS_Extracts_table WHERE Medium = 'Water - Fresh'")

res <- dbGetQuery(con, "SELECT * FROM EMS_Extracts_table WHERE Medium = 'Water - Fresh' AND
           Project = 'BCLMN'")


mon_loc_id <- "E221544"

res <- dbGetQuery(con, paste0("SELECT * FROM EMS_Extracts_table WHERE Location_ID = '", mon_loc_id, "' AND
                              Field_Visit_Start_Time >= '2019-05-01 00:00:00' AND
                              Field_Visit_Start_Time <= '2021-01-01 23:59:59'"))


res <- dbGetQuery(con, paste0("SELECT * FROM EMS_Extracts_table WHERE Location_ID = '", mon_loc_id, "'"))

res <- dbGetQuery(con, paste0("Describe EMS_Extracts_table"))

#output for EDT
res$Lab_Arrival_Date_and_Time <- as.POSIXct(res$Lab_Arrival_Date_and_Time)
res$Project = "TRAIN"
res$QC_Type[res$QC_Type == ""] <- "REGULAR"
res$Method_Detection_Limit[is.na(res$Method_Detection_Limit)] <- 0 #for testing only
names(res) <- gsub("_", " ", names(res))

res <- res %>%
  mutate(across(where(is.character), ~replace_na(., "")))

#ISO8601 date time
fix_date <- function(dt) {
  if_else(
    is.na(dt),
    "",
    sub("(\\+|\\-)(\\d{2})(\\d{2})$", "\\1\\2:\\3",
        format(dt, format = "%Y-%m-%dT%H:%M:%S%z", tz = "America/Los_Angeles"))
  )
}



res$`Field Visit Start Time` <- fix_date(res$`Field Visit Start Time`)
  
res$`Field Visit End Time` <- fix_date(res$`Field Visit End Time`)

res$`Observed DateTime` <- fix_date(res$`Observed DateTime`)

res$`Observed Date Time End` <- fix_date(res$`Observed Date Time End`)

res$`Analyzed Date Time` <- fix_date(res$`Analyzed Date Time`)

res$`Lab Arrival Date and Time` <- fix_date(res$`Lab Arrival Date and Time`)

res$`Lab Prepared DateTime` <- fix_date(res$`Lab Prepared DateTime`)

options(scipen=999)
write.csv(res, file = paste0(mon_loc_id,".csv"), row.names = F, na = "")

dbDisconnect(con, shutdown = TRUE)
