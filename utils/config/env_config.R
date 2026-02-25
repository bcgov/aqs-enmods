load_aqs_tokens_urls <- function(){

  #' Title: load_aqs_tokens_urls
  #' @description
  #'  Load environment variables from a file
  #' @returns an R environment with the necessary tokens and AQS base URLs
  #' @import httr jsonlite tidyverse purrr dplyr lubridate stringr bcdata sf tidygeocoder readr readxl writexl openxlsx hunspell tidyr
  #' @importFrom magrittr %>%
  #' @export
  #' @examples

# library(httr)
# library(jsonlite)
# library(tidyverse)
# library(purrr)
# library(dplyr)
# library(lubridate)
# library(stringr)
# library(bcdata)
# library(sf)
# library(tidygeocoder)
# library(readr)
# library(readxl)
# library(writexl)
# library(openxlsx)
# library(hunspell)

test_token <- base::Sys.getenv("TEST_TOKEN")
assign("test_token", test_token, envir = .GlobalEnv)
prod_token <- base::Sys.getenv("PROD_TOKEN")
assign("prod_token", prod_token, envir = .GlobalEnv)
test_url <- base::Sys.getenv("TEST_URL")
assign("test_url", test_url, envir = .GlobalEnv)
prod_url <- base::Sys.getenv("PROD_URL")
assign("prod_url", prod_url, envir = .GlobalEnv)
training_token <- base::Sys.getenv("TEST_TOKEN")
assign("training_token", test_token, envir = .GlobalEnv)
training_url <- base::Sys.getenv("TRAINING_URL")
assign("training_url", training_url, envir = .GlobalEnv)
#env <- base::Sys.getenv("env")

#update_base_url_token(env)

}

update_base_url_token <- function(env) {

  #' Title: update_base_url_token
  #' @description
  #'  Update the base URL and token that lines up with the selected environment
  #' @param env AQS environment. Takes values "prod" or "test"
  #' @returns the base URL and token combination based on the selected environment
  #' @import tidyverse
  #' @export
  #' @examples

  if (env == "prod") {

    list(prod_url, prod_token)

    } else if (env == "test") {

      list(test_url, test_token)

    } else if (env == "training") {
	
	  list(training_url, training_token)
	
	} else {

      print("The AQS environment can only be `prod`, `test` or `training`")

    }

}
