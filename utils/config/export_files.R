export_files <- function(env, data_type){

  #' Title: export_files
  #' @description
  #'  Export key environment variables from the AQS environment
  #' @param env AQS environment. Takes values "prod" or "test"
  #' @param data_type AQS environment parameter
  #' @returns the underlying configuration file that will partially be read into AQS
  #' @import httr jsonlite tidyverse purrr dplyr lubridate stringr bcdata sf tidygeocoder readr readxl writexl openxlsx hunspell tidyr
  #' @importFrom magrittr %>%
  #' @export
  #' @examples

  if(data_type == "units"){

    units <- get_profiles(env, data_type) %>%
      unnest(unitGroup, names_sep = "")

    #names in EMS
    #conversion_factor, sample_unit_name, sample_unit_customid,
    #convertible, conversion_factor, offset, sample_unit_group

    #names in EMS EDT Support Tables
    #Code, Unit, Measurement Unit Description

    units <- units %>%
      dplyr::select(customId, name, unitGroupcustomId, unitGroupsupportsConversion, baseMultiplier, baseOffset) %>%
      rename(sample_unit_customid = customId,
             sample_unit_name = name,
             sample_unit_group = unitGroupcustomId,
             convertible = unitGroupsupportsConversion,
             conversion_factor = baseMultiplier,
             offset = baseOffset) %>%
             #convertible, conversion_factor, offset, sample_unit_group)
      unique()

    units <- units %>% table_as_character()

    return(units)

  } else if(data_type == "unit_groups"){

    unit_groups <- get_profiles(env, data_type)

    unit_groups <- unit_groups %>%
      dplyr::select(customId, supportsConversion) %>%
      rename(sample_unit_group = customId,
             convertible = supportsConversion
             ) %>%
      #convertible, conversion_factor, offset, sample_unit_group)
      unique()

    unit_groups <- unit_groups %>% table_as_character()

    return(unit_groups)

  } else if(data_type == "observed_properties"){

    #observed_properties <- get_profiles(env, data_type)

    observed_properties <- get_profiles(env, data_type) %>%
      unnest(unitGroup, names_sep = "") %>%
      unnest(defaultUnit, names_sep = "")

    observed_properties <- observed_properties %>%
      dplyr::select(c("customId", "name", "description", "resultType",
                      "analysisType", "unitGroupcustomId", "defaultUnitcustomId", "casNumber")) %>%
       rename("newnameid"      = customId,
              "parm_code"      = name,
              "description"    = description,
              "result_type"    = resultType,
              "analysis_type"  = analysisType,
              "unit_group_id"  = unitGroupcustomId,
              "unit_id"        = defaultUnitcustomId,
              "cas"            = casNumber) %>%
      unique()

    observed_properties <- observed_properties %>% table_as_character()

    return(observed_properties)

  }

}
