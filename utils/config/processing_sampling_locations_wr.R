#' Process sampling locations and assign watershed groups
#'
#' Title: processing_sampling_locations_wr
#' @description Takes the locations file in the GlobalEnv and joins each location
#' with its corresponding BC watershed group polygon. Note that:
#' 1. The data output here should not have any locations with NA lat or long so it can be mapped
#' 2.
#' @param env AQS environment. Takes values "prod" or "test"
#' @returns An `sf` object of sampling locations with added watershed group information.
#' @import readxl writexl openxlsx dplyr stringr readr httr jsonlite tidyverse lubridate stringr bcdata sf
#' @importFrom "utils" "data" "download.file" "methods" "read.csv" "write.csv"
#' @importFrom magrittr %>%
#' @export
#' @examples

processing_sampling_locations_wr <- function(env = "test") {

  if(exists("locations", envir = globalenv())){

    #Step 1: Getting locations data from env since preprocessing generates it
    locations <- get("locations", envir = globalenv())

    #Step 2: Filter NA lat long and convert to sf
    locations <-  locations %>%
      dplyr::filter(!(is.na(Latitude)|is.na(Longitude))) %>%
      dplyr::filter(!(Latitude == ""|Longitude == "")) %>%
      st_as_sf(coords = c("Longitude", "Latitude"), crs = "EPSG:4326", remove = FALSE) %>%
      st_transform(crs = "EPSG:3005")

    # Step 3: Download BC watershed group polygons
    bcdc_freshwater_atlas_watershed_groups <- bcdata::bcdc_query_geodata("51f20b1a-ab75-42de-809d-bf415a0f9c62") %>%
      dplyr::collect()

    bcdc_freshwater_atlas_watershed_groups <- bcdc_freshwater_atlas_watershed_groups %>%
      dplyr::select(WATERSHED_GROUP_CODE, WATERSHED_GROUP_NAME) %>%
      dplyr::distinct()

    # Step 4: Spatial join
    locations <- locations %>%
      st_join(bcdc_freshwater_atlas_watershed_groups, join = st_intersects, left = TRUE)

    # Step 5: Replace all NA values with an empty string ""
    locations <- locations %>%
      mutate(`EA_Closed Date` = as.character(`EA_Closed Date`)) %>%
      mutate(across(`EA_EMS When Created`:`EA_Established Date`, as.character)) %>%
      mutate(across(everything(), ~ replace(., is.na(.), ""))) #%>%
      #mutate(`EA_Closed Date` = as.POSIXct(`EA_Closed Date`)) %>%
      #mutate(across(`EA_EMS When Created`:`EA_Established Date`, as.POSIXct))

    return(locations)

  } else {

    locations <- get_profiles("locations")

    return(locations)

  }

  # Step 5: Return the resulting sf object
  return(locations)

}
