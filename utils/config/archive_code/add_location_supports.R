#Functions to add back locations and their supporting records, saved filters, location groups

add_location_groups <- function(base_url, token, customId){
  # #write function to remove a single location group but keep locations
  # 
  # #get all location groups from the system
  # url <- paste0(base_url, "v1/samplinglocationgroups")
  # data_body <- list()
  # x<-GET(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  # 
  # all_location_groups <- fromJSON(rawToChar(x$content))$domainObjects
  # 
  # #find the location group named by the user
  # group_guid <- all_location_groups %>% filter(name == customId)
  # 
  # #catch if no records found with that name
  # if (nrow(group_guid) < 1) {
  #   print("No location group found by that name")
  #   return(null)
  # }
  # 
  # #remove the location group
  # url <- paste0(base_url, "v1/samplinglocationgroups/", group_guid$id)
  # DELETE(url, config = c(add_headers(.headers = c('Authorization' = token))), body = data_body, encode = 'json')
  
}