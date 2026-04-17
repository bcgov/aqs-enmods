# Load required libraries
library(data.table)
library(DBI)
library(duckdb)

# Set your data folder path
data_folder <- "I:/AQI_Export"

# List all gz files
# Point to your folder of .gz files
gz_files <- list.files("I:/AQI_Export", pattern = "\\.gz$", full.names = TRUE)

# Build a single CREATE TABLE statement using read_csv with all files at once
con <- dbConnect(duckdb(), dbdir = "I:/AQI_Raw_Data_2026_03_28.duckdb")

dbExecute(con, 
          "CREATE TABLE my_table AS SELECT * REPLACE (
      labResultDetailsDilutionFactor::DOUBLE AS labResultDetailsDilutionFactor
   )
   FROM read_csv(
    'I:/AQI_Export/*.gz',
    compression = 'gzip',
    header = true,
    union_by_name = true
  )"
)
#this will print progress as it loads takes awhile like an hour

#get the total number of rows
dbGetQuery(con, "SELECT COUNT(*) FROM my_table")

#get the name of all colums
columns <- dbGetQuery(con, "DESCRIBE my_table")

#close connection
con <- dbConnect(duckdb(), dbdir = "I:/AQI_Raw_Data_2026_03_28.duckdb")


all_files <- list.files(data_folder, pattern = "\\.csv$", full.names = TRUE)

# Connect to DuckDB (creates a new DB file)
con <- dbConnect(duckdb::duckdb(), dbdir = "AQI_Export_20260110.duckdb")

# Start timing
start_time <- Sys.time()

# Loop through sample files and write to DuckDB
for (file in all_files) {
  df <- fread(file)
  dbWriteTable(con, "benchmark_table", df, append = TRUE)
}

#Connect to an exisitng db
con <- dbConnect(duckdb::duckdb(), dbdir = "AQI_Export_20260110.duckdb")
