# Load required libraries
library(data.table)
library(DBI)
library(duckdb)

# Set your data folder path
data_folder <- "I:/AQI_Export"

# List all CSV files
all_files <- list.files(data_folder, pattern = "\\.csv$", full.names = TRUE)

# Connect to DuckDB (creates a new DB file)
con <- dbConnect(duckdb::duckdb(), dbdir = "benchmark.duckdb")

# Start timing
start_time <- Sys.time()

# Loop through sample files and write to DuckDB
for (file in all_files) {
  df <- fread(file)
  dbWriteTable(con, "benchmark_table", df, append = TRUE)
}

#Connect to an exisitng db
con <- dbConnect(duckdb::duckdb(), dbdir = "benchmark.duckdb")

#Test queries against the data
res <- dbGetQuery(con, "SELECT * FROM benchmark_table WHERE Ministry_Contact = 'Jeremy Krogh'")
#550 in AQS and export - PASS

#total count
res <- dbGetQuery(con, "SELECT COUNT(*) FROM benchmark_table")
#23,160,827 but AQS only reports 23,160,759

dbGetQuery(con, "SELECT COUNT(*) FROM benchmark_table WHERE Medium = 'Water - Fresh'")
#12,634,195 in AQS 12,634,191 four records missing...

dbGetQuery(con, "SELECT COUNT(*) FROM benchmark_table WHERE QC_Type = 'BLANK'")
#221,929 in both

dbGetQuery(con, "SELECT COUNT(*) FROM benchmark_table WHERE QC_Type = 'SPIKE'")
#2,493 in both

res <- dbGetQuery(con, "SELECT * FROM benchmark_table WHERE Project = 'BCLMN' AND Observed_Property_ID = 'Nickel Total (fl. conc.)'")
#64 in both

res <- dbGetQuery(con, "SELECT * FROM benchmark_table WHERE Location_Group LIKE '%100001%' AND Observed_Property_ID = 'Phosphorus Total (fl. conc.)'")
#pass 3,575 in both

res <- dbGetQuery(con, "SELECT * FROM benchmark_table WHERE Data_Classification = 'FIELD_RESULT' 
                  AND Observed_Property_ID = 'Specific Conductivity-Field (elec. cond.)'")
#count pass

res <- dbGetQuery(con, "SELECT * FROM benchmark_table WHERE QC_Type = 'BLANK' 
                  AND Observed_Property_ID = 'Nickel Total (fl. conc.)'")
#count pass

res <- dbGetQuery(con, "SELECT * FROM benchmark_table WHERE Location_ID = 'E207815'")


#Issues so far
#counts don't match but are very close
#LAB_SAMPLE_ID mapped to specimen name which is redundent should be lab sample ID
#no TZ offset on observation time
#Field_Comment - not mapped to field visit comments
#

# Clean up
dbDisconnect(con, shutdown = TRUE)
