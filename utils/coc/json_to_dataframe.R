#Convert from json file to data frame

library(jsonlite)

pkgs <- fromJSON("./utils/coc/jsons/water_soil_analysisPackages_data.json", flatten = TRUE)

pkgs_df <- as.data.frame(pkgs)


library(tidyverse)

expanded <- unnest(pkgs_df, 'items.observedProperties')
