#' @param file Path to a file (e.g., ".env" or ".Renviron") containing key=value pairs
#' @return Invisibly returns TRUE if loading is successful
#' @export
#' load_env_vars <- function(file) {
#'   if (!file.exists(file)) stop("Environment file not found: ", file)
#'   lines <- readLines(file, warn = FALSE)
#'   valid <- grepl("^[A-Za-z_][A-Za-z0-9_]*=.*$", lines)
#'   kv_pairs <- lines[valid]
#'   for (line in kv_pairs) {
#'     parts <- strsplit(line, "=")[[1]]
#'     key <- trimws(parts[1])
#'     value <- trimws(paste(parts[-1], collapse = "="))
#'     assign(key, value)
#'     #Sys.setenv(key)
#'   }
#'   #invisible(TRUE)
#' }
#'
#' load_env_vars("./data/.env")

#Cannot be kept in the package
#readRenviron(paste0(getwd(), "./.Renviron"))
#get the API tokens from your environment filesource("./R/load_env_vars.R")
#load_env_vars("./data/.env")

## install.packages("dotenv")
#library(dotenv)

## This sets all environment variables in .env
#dotenv::load_dot_env(file = ".env")
   file = "./data/.env"
   if (!file.exists(file)) stop("Environment file not found: ", file)
   lines <- readLines(file, warn = FALSE)
   valid <- grepl("^[A-Za-z_][A-Za-z0-9_]*=.*$", lines)
   kv_pairs <- lines[valid]
   
   for (line in kv_pairs) {
     parts <- strsplit(line, "=")[[1]]
     key <- trimws(parts[1])
     value <- trimws(paste(parts[-1], collapse = "="))
     assign(key, value)
     #Sys.setenv(key)
   }

test_token <- Sys.getenv("TEST_TOKEN")
prod_token <- Sys.getenv("PROD_TOKEN")
test_url <- Sys.getenv("TEST_URL")
prod_url <- Sys.getenv("PROD_URL")
env <- Sys.getenv("env")

