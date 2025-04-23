
env = "prod"
# 
data_type = "methods"
#data_type = "observedproperties"
# 
profile <- Methods %>% 
  dplyr::filter(Method.Code == "0020")
  #dplyr::filter(NewNameID == "Biological Sample Volume (vol.)")

#Clean the old stuff out of the environment before posting new stuff
if(!is.null(dim(get_profiles(env, data_type))[1])){
  
  del_profiles(env, data_type)
  
}

#default is "test" and for prod env, use the function parameter "prod"
url_parameters <- update_base_url_token(env)
base_url <- url_parameters[[1]]
token <- url_parameters[[2]]

# profile <- profile %>% 
#   mutate(across(everything(), ~ replace(., is.na(.), "")))

#url <- paste0(base_url, "v1/observedproperties")
url <- paste0(base_url, "v1/analysismethods")

#rel_var <- c("Parm.Code", "NewNameID", "Description", "Analysis.Type",
#             "unit.group.id", "unit.id", "CAS")
rel_var <- c("Method.Code", "Method", "Method.Description", "OPs_list")

messages <- list()

#for(j in 1:dim(profile)[1]){

j <- 1

temp_profile <- profile %>% 
  keep(names(.) %in% rel_var) %>%
  slice(j) #%>%
#as.list()

# data_body <- list(
#   "customId" = temp_profile$NewNameID,
#   "name" = temp_profile$Parm.Code,
#   "description" = temp_profile$Description,
#   "resultType" = "NUMERIC",
#   "analysisType" = temp_profile$Analysis.Type,
#   "unitGroup" = list("id" = temp_profile$unit.group.id),
#   "defaultUnit" = list("id" = temp_profile$unit.id),
#   "casNumber" = temp_profile$CAS
# )

data_body <- list("methodId" = temp_profile$Method.Code,
                  "name" = temp_profile$Method,
                  "description" = temp_profile$Method.Description,
                  "context" = "EMS Migration",
                  "observedProperties" = temp_profile$OPs_list
)

#Post the configuration
x<-POST(url, config = c(add_headers(.headers = 
                                      c('Authorization' = token))), body = data_body, encode = 'json')

messages <- fromJSON(rawToChar(x$content))


