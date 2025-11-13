# Load required libraries
library(data.table)
library(DBI)
library(duckdb)

# Set your data folder path
data_folder <- "I:/EMS-Exports-2025-11-12"

# List all CSV files
all_files <- list.files(data_folder, pattern = "\\.csv$", full.names = TRUE)

# Connect to DuckDB (creates a new DB file)
con <- dbConnect(duckdb::duckdb(), dbdir = "EMS-Extract-2025-11-14.duckdb")

# Loop through sample files and write to DuckDB
for (file in all_files) {
  print(file)
  df <- fread(file)
  df <- df %>% mutate_if(is.logical, as.character)
  dbWriteTable(con, "EMS_Extracts_table", df, append = TRUE)
}

#if db already exists

con <- dbConnect(duckdb::duckdb(), dbdir = "EMS-Extract-2025-11-14.duckdb")

dbGetQuery(con, "SELECT COUNT(*) FROM EMS_Extracts_table")

dbGetQuery(con, "SELECT COUNT(*) FROM EMS_Extracts_table WHERE Medium = 'Water - Fresh'")

res <- dbGetQuery(con, "SELECT * FROM EMS_Extracts_table WHERE Medium = 'Water - Fresh' AND
           Project = 'BCLMN'")

dbDisconnect(con, shutdown = TRUE)
