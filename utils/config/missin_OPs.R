#Script to help with finding solutions to all the missing observed properties
#and or methods that Salus ID'ed. Some will need new OPs, some already exist,
#some need new methods, some need new method associations.

library(httr)
library(jsonlite)
library(dplyr)
library(writexl)
library(readxl)

#get the API token from your environment file
readRenviron(paste0(getwd(), "./.Renviron"))
token <- Sys.getenv("api_test_token")
base_url = Sys.getenv("url_test")

#read the missing OPs
missing_op <- read.csv("./utils/config/ReferenceLists/unmapped-observed-properties.csv")

#read the units table to predict OP names
units <- read_excel("./utils/config/ReferenceLists/Units.xlsx", sheet = 3)

#select just columns that are needed
units <- units %>% select("SHORT_NAME","Sample.Unit.Short.Name","Sample.Unit.Modifier")

#clean up all the crud
units$Sample.Unit.Modifier[str_starts(units$Sample.Unit.Modifier, "0")] <- ""
units$Sample.Unit.Modifier[str_starts(units$Sample.Unit.Modifier, "FALSE")] <- ""
units$Sample.Unit.Modifier[is.na(units$Sample.Unit.Modifier)] <- ""
units <- units %>% filter(!is.na(SHORT_NAME))

#join
missing_op <- merge(missing_op, units, by.x = "EMS.Result.Unit", by.y = "SHORT_NAME",
                    all.x = T, all.y = F)

#add predicted names
missing_op$AQS.Name <- paste(missing_op$parameter_name, missing_op$Sample.Unit.Modifier, missing_op$Sample.Unit.Short.Name)
missing_op$AQS.Name <- str_replace(missing_op$AQS.Name, "  ", " ")


#check to see if
#1) the OP exists
#2) the method exists

for (i in seq(1, nrow(missing_op))) {
  url <- paste0(base_url, "v1/observedproperties?customId=", missing_op$AQS.Name[i])
  data_body <- list()
  x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  missing_op$name_in_aqs[i] <- fromJSON(rawToChar(x$content))$totalCount
  
  if (missing_op$name_in_aqs[i] > 0) { #if the OP is in aqs
    if (fromJSON(rawToChar(x$content))$domainObjects$analysisType == "CHEMICAL"){ #only chemicals have a cas
      if (!is.null(fromJSON(rawToChar(x$content))$domainObjects$casNumber)) {   
        missing_op$cas_in_aqs[i] <- fromJSON(rawToChar(x$content))$domainObjects$casNumber
      } else {
        missing_op$cas_in_aqs[i] <- "" #no cas in aqs
      }
    } else {
      missing_op$cas_in_aqs[i] <- "" #not a chemical
    }
  } else {missing_op$cas_in_aqs[i] <- ""} # not in aqs
  
  url <- paste0(base_url, "v1/labanalysismethods?search=", missing_op$Analysis.Method[i])
  data_body <- list()
  x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  missing_op$Analysis.Method_in_AQS[i] <- fromJSON(rawToChar(x$content))$totalCount
  
  print(i)
} 

write.csv(missing_op, "./utils/config/ReferenceLists/missing_op_actions.csv", row.names = F)
