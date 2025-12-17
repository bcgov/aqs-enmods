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
paths = list.files("I:/EDT_Testing/Sept2-testing/part31", full.names = T)

#Push file to EDT
for(i in seq(250, 400)) {
  resp <- POST(edt_url, add_headers('x-api-key' = token),
     body = list(file=upload_file(paths[i]),
                 username = edt_username), 
     encode = 'multipart')
  print(paths[i])
    Sys.sleep(60*3) #pause before uploading the next file
    
   
    
}

for (i in seq(1,50)) {
resp <- POST(edt_url, add_headers('x-api-key' = token),
     body = list(file=upload_file('C:/Users/jkrogh/Downloads/ALS_Test4.csv'),
                 username = edt_username), 
     encode = 'multipart')
  Sys.sleep(5)
print(resp$status_code)
}

Sys.sleep(60*3) #pause before uploading the next file

print(paths[i])
