#Code to upload EA to AQS based on work from carmenriddel
#
#This code first extracts EAs as they are currently built in test, write them 
#to an excel sheet that can then be used to upload to another environment

library(httr)
library(jsonlite)
library(dplyr)
library(writexl)
library(readxl)

#Load tokens and urls
readRenviron(paste0(getwd(), "./.Renviron"))
token <- Sys.getenv("api_test_token")
base_url = Sys.getenv("url_test")

data_body <- list()
resp<-GET(paste0(base_url,'v1/extendedattributes/'), config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')

df_EA <- fromJSON(rawToChar(resp$content))$domainObjects

#filter to only those EAs with drop downs and those that do not contain 'DELETE'
df_EA <- df_EA %>% filter(!grepl("DELETE", customId)) %>% filter(!grepl("Delete", customId)) %>% filter(!grepl("delete", customId))

EA_drop_down <- df_EA %>% filter(dataType == "DROP_DOWN_LIST")

for (i in seq(1, nrow(EA_drop_down))) {
  url <- paste0(base_url,'v1/extendedattributes/', EA_drop_down$id[i],'/dropdownlistitems')
  resp_list <- GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json') #status 200

  temp <- fromJSON(rawToChar(resp_list$content))$domainObjects
  temp$EA_id <- EA_drop_down$id[i]
  if (i == 1) {
    df_DLL <- temp
  } else {
    df_DLL <- rbind(df_DLL, temp)
  }
}


df_DLL <- merge(x=df_DLL, y=select(EA_drop_down, c('id', 'customId')), by.x="EA_id", by.y="id", all.x=TRUE)
colnames(df_DLL) <- c("EA_Id", "DDL_Id","DDL_customId", "EA_customId")

write.csv(df_DLL,"./utils/config/ReferenceLists/EA_DropDownListItems.csv", row.names = FALSE)

#Read in files to post, they don't update 
df_EA <- read_excel("./utils/config/ReferenceLists/ExtendedAttributes.xlsx", sheet = 1)
df_DLL <- read_excel("./utils/config/ReferenceLists/ExtendedAttributes.xlsx", sheet = 2)


for (i in seq(1, nrow(df_EA))) {
  
  post_data <- list("customId" = df_EA$customId[i],
                    "dataType" = df_EA$dataType[i],
                    "appliesToType" = df_EA$appliesToType[i],
                    "description" = df_EA$description[i]
                    )
  
  if (df_EA$dataType[i] == 'DROP_DOWN_LIST') {
    df_DLL_temp <- df_DLL %>% filter(EA_customId == df_EA$customId[i])
    dl_items <- list()
    for (j in seq(1,nrow(df_DLL_temp))) {
      dl_list <- list("customId" = df_DLL_temp$DDL_customId[j])
      dl_items[j] <- list("customId" = dl_list)
    }
    post_data$dropDownListItems <- dl_items
  }
  print(post_data)
  POST(paste0(base_url,'v1/extendedattributes/'), config = c(add_headers(.headers = c('Authorization' = token))), encode="json", body = post_data)
}


