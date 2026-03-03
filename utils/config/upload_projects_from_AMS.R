#Upload AMS Project to PROD

library(httr)
library(jsonlite)
library(readxl)
library(dplyr)
library(stringr)

#get the API tokens from your environment file
readRenviron(paste0(getwd(), "./.Renviron"))
testToken <- Sys.getenv("TEST_TOKEN")
prodToken <- Sys.getenv("PROD_TOKEN")
testURL <- Sys.getenv("TEST_URL")
prodURL <- Sys.getenv("PROD_URL")

#read latest extract from AMS
ams_permits <- read_excel("./data/All AMS Authorizations Feb_23_2026.xlsx") %>%
  rename_with(tolower) %>% 
  rename_with(~ gsub("\\.", "_", .)) %>% 
  rename_with(~ gsub("\\-", "_", .)) %>%
  rename_with(~ gsub(" ", "_", .))

#filter
ams_permits <- ams_permits %>% filter(state %in% c("Active", "Suspended", "Expired"))

#select columns needed
ams_permits <- ams_permits %>% select(authorization_number, authorization_type, company, issue_date,
                                      waste_type, wdr_schedule_one_or_two, facility_type___description)


#make nice for AQS import
ams_permits_aqs <- data.frame(id = ams_permits$authorization_number,
                              name = paste0(ams_permits$authorization_number, ", ", ams_permits$company),
                              startDate = ams_permits$issue_date,
                              comments = paste0(ams_permits$facility_type___description),
                              scope = ams_permits$wdr_schedule_one_or_two,
                              type = "Routine Monitoring")

#remove duplicate rows
ams_permits_aqs <- unique(ams_permits_aqs)

#double check
check <- ams_permits_aqs %>% group_by(id) %>% summarize(count = n())

#the following have issues with two compnay names fixed to align with e-licencing
#109747
#110385
#111988
#112246

for (i in seq(1,5205)) {
  
print(i)
  
data_body <- list('customId' = ams_permits_aqs$id[i],
                  'name' = ams_permits_aqs$name[i],
                  'type' = 'ROUTINE_MONITORING',
                  'description' = ams_permits_aqs$comments[i],
                  'scopeStatement' = ams_permits_aqs$scope[i],
                  'approved' = TRUE,
                  'active' = 'TRUE',
                  'approvalAgency' = 'Authorization Managment System (AMS)',
                  'startTime' = format(ams_permits_aqs$startDate[i], "%Y-%m-%dT%H:%M:%S%z")
                  )


y<-POST(paste0(prodURL, "v1/projects/"), config = c(add_headers(.headers = c('Authorization' = prodToken ))), body = data_body, encode = 'json')

print(y)

if (y$status_code != 200) {i = 52506}
#print(fromJSON(rawToChar(y$content)))

}

#done