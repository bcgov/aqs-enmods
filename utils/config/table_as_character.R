table_as_character <- function(tbl){

  #' Title: table_as_character
  #' @description
  #'  Export key environment variables from the AQS environment
  #' @param env AQS environment. Takes values "prod" or "test"
  #' @param data_type AQS environment parameter
  #' @returns the underlying configuration file that will partially be read into AQS
  #' @import httr jsonlite tidyverse purrr dplyr lubridate stringr bcdata sf tidygeocoder readr readxl writexl openxlsx hunspell tidyr
  #' @importFrom magrittr %>%
  #' @export
  #' @examples

  tbl <- tbl %>%
    mutate(across(everything(), ~ as.character(.))) %>%
    mutate(across(everything(), ~ ifelse(is.na(.), "", .)))

}
