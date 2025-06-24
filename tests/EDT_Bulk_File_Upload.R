#
#Script to upload files to the EDT for ingestion into EnMoDS. Files take aprx
# 300s (5 minutes) per 1300 rows.
#
#May 16 2025 J-Krogh

library(httr)
library(jsonlite)

readRenviron(paste0(getwd(), "./.Renviron"))
token <- Sys.getenv("EDT_TEST_TOKEN")
edt_url = Sys.getenv("EDT_TEST_URL")
edt_username = Sys.getenv("EDT_TEST_USERNAME")

#upload file path
paths = list.files("I:/Data_Extracts_2025_06_17/EDT-TEST-Files/EDT-10/", full.names = T)

#Push file to EDT
for(i in seq(1, 200)) {
  POST(edt_url, add_headers('x-api-key' = token),
     body = list(file=upload_file(paths[i]),
                 username = edt_username), 
     encode = 'multipart')
  
    Sys.sleep(10) #pause before uploading the next file
    
    print(paths[i])
    
}


