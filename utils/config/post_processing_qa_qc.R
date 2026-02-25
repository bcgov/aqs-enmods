#QA/QC checks for uploads to the AQS environment

qa_qc_profile <- function(env, data_type){

  #' Title: qa_qc_all_profiles
  #' @description
  #'  Checks if all the profiles in the AQS system line up with those in the system environment
  #' @param env AQS environment. Takes values "prod" or "test"
  #' @param data_type AQS environment parameter
  #' @returns Data frame that tells the user how many entries to expect
  #' @import dplyr purrr
  #' @export
  #' @examples

  #Checks how many unique analytical groups exist and allocates OPs to them
  if(data_type == "analytical_groups"){

    analytical_groups_check <- analytical_groups %>%
      dplyr::group_by(op_group) %>%
      dplyr::summarize(
        analyticalgroupitems = list(
          purrr::map(newnameid, ~ list(observedProperty = list("id" = .x)))
        ),
        .groups = "drop"
      )

    return(analytical_groups_check)

  } else if(data_type == "observed_properties"){

    #Checks how many unique OPs exist
    observed_properties_check <- observed_properties %>%
      dplyr::select(newnameid) %>%
      unique()

    return(observed_properties_check)

  } else {

    "QA/QC checks have not been set up for this AQS environmental parameter"

  }

  #units_missing <-

  # # QA/QC for UNITS -------------------------------------------
  #
  # #first post files
  # post_check <- post_profiles("prod", "units", units)
  #
  # # # #109 only; even though 126 in the units file - this is OK because many units
  # #were merged from EMS for example % ww and % weight and % vv all become %
  # get_check <- get_profiles("prod", "units")
  # # #
  # # # #no such units
  # #units_na <- units %>% dplyr::filter(is.na(sample_unit_customid))
  # # #
  # units_missing <- units %>%
  #   anti_join(get_check,
  #             by = join_by("sample_unit_customid" == "customId"))

  # # QA/QC for observed_properties -----------------------------------------------------------
  #
  # # Checking which entries are not getting posted
  # get_check <- get_profiles("prod", "observedproperties")
  #
  # #not all observed_properties getting posted
  # #compare get_check for observed_properties with raw observed_properties - 0 but 4172 in AQS 4285 in data frame
  # observed_properties_not_posted <- observed_properties %>% anti_join(get_check,
  #                                                    by = join_by("newnameid" == "customId"))

  # # QA/QC for METHODS -------------------------------------------
  #
  # #first post files
  # post_check <- post_profiles(env, "methods", methods)
  #
  # get_check <- get_profiles(env, "methods")
  #
  # methods.missing <- methods %>%
  #   dplyr::anti_join(get_check,
  #             by = dplyr::join_by("method_code" == "methodId"))
  #
  #


}
