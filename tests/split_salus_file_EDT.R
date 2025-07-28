#script to split Salus' large export files into smaller chuncks of n rows to test
#EDT.
#
#April 17 2025
#
#Updated for part 30 and removing extractable July 23 2025 JK

library(tidyverse)

#read the file from Salus
fname <- "I:/Data_Extracts_2025_06_17/Water/water_soil_split_part_28.csv"
rows <- 60 #number of obs per field visit (apx)
write_name <- "I:/Data_Extracts_2025_06_17/EDT-TEST-Files/EDT-28/June172025"

#read the file from Salus, these can be big up to ~600,000
big_file <- readr::read_csv(fname)

#fix units temp
big_file$`Result Unit`[big_file$`Result Unit` == "ug/g wet"] = "ug/g"
big_file$`Result Unit`[big_file$`Result Unit` == "C"] = "degC"
big_file$`Result Unit`[is.na(big_file$`Result Unit`)] = "None"

big_file$Fraction[big_file$Fraction == "Extractable"] = "" #added July 23 2025
big_file$Fraction[is.na(big_file$Fraction)] = ""


#remove debug columns
big_file <- big_file %>% select(-c("DEBUGGING PARM_CD","DEBUGGING ANALYSIS METHOD",
                                   "DEBUGGING_EMS_RESULT_UNIT"))

#add filter for only some locations
#big_file <- big_file %>% filter(`Location ID` %in% c('E102669','E102672','E102983','E102984','E102990'))

#700k rows
#group things by field visit
big_file$fieldVisitId <- paste(big_file$`Location ID`, big_file$`Field Visit Start Time`)

field_visits <- unique(big_file$fieldVisitId) #16k or avg of 43 obs per visit

for (i in seq(1,ceiling(length(field_visits)/rows))) {
  
  if (i == 1) {
    #temp <- big_file[c(seq(i, i*rows)),]
    temp <- big_file %>% filter(fieldVisitId %in% field_visits[seq(i, i*rows)])
  } else {
    #temp <- big_file[c(seq((i-1)*rows, i*rows)),]
    temp <- big_file %>% filter(fieldVisitId %in% field_visits[seq(((i-1)*rows) + 1, i*rows)])
  }
  
  #remove NA and replace with ""
  temp[] <- lapply(temp, function(x) {
    if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
      x <- format(x, "%Y-%m-%dT%H:%M:%S-08:00", tz="08:00")  # or whatever format you prefer
      x[is.na(x)] <- ""                    # replace NA with ""
    } else {
      x[is.na(x)] <- ""
      x[x == "NA"] <- "" #remove the NA string from SQL
      x <- as.character(x)
    }
    x
  })
  
  temp$Project <- "Part28-DATA-TESTING"
  
  #files end up sci notation in the string values
  temp$`Method Detection Limit` <- format(as.numeric(temp$`Method Detection Limit`), 
                                          scientific = FALSE, digits = 10, trim = T) #this adds back NA
  temp$`Method Detection Limit` <- as.character(temp$`Method Detection Limit`)
  temp$`Method Detection Limit`[temp$`Method Detection Limit` == "NA"] <- "" #remove NAs again
  
  temp$`Result Value` <- format(as.numeric(temp$`Result Value`), scientific = FALSE, digits = 10, trim = T)

  #remove the field visit id column
  temp <- temp %>% select(-fieldVisitId)
  
  write.csv(temp, paste0(write_name, "-", i, ".csv"), row.names = F)
  
  print(i)

}
