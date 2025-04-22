#script to split Salus' large export files into smaller chuncks of n rows to test
#EDT.
#
#April 17 2025
library(tidyverse)

#read the file from Salus
fname <- "C:/Users/jkrogh/Downloads/water_split_part_28.csv"
rows <- 2000
write_name <- "C:/Users/jkrogh/Downloads/EDT/water-april-14"

#read the file from Salus, these can be big up to ~600,000
big_file <- readr::read_csv(fname)

for (i in seq(1,ceiling(nrow(big_file)/rows))) {
  
  if (i == 1) {
    temp <- big_file[c(seq(i, i*rows)),]
  } else {
    temp <- big_file[c(seq((i-1)*rows, i*rows)),]
  }
  
  #remove NA and replace with ""
  temp[] <- lapply(temp, function(x) {
    if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
      x <- format(x, "%Y-%m-%dT%H:%M:%S-08:00")  # or whatever format you prefer
      x[is.na(x)] <- ""                    # replace NA with ""
    } else {
      x[is.na(x)] <- ""
      x <- as.character(x)
    }
    x
  })
  
  temp$Project <- "AIR-TESTING"
  
  #files end up sci notation in the string values
  temp$`Method Detection Limit` <- format(as.numeric(temp$`Method Detection Limit`), scientific = FALSE, digits = 10, trim = T)
  temp$`Result Value` <- format(as.numeric(temp$`Result Value`), scientific = FALSE, digits = 10, trim = T)

  write.csv(temp, paste0(write_name, "-", i, ".csv"), row.names = F)
  
  print(i)

}
