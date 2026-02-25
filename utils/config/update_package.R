update_package <- function(){

  #' Title: update_package
  #' @description
  #'  Pre-processes starter files for other parameters
  #' @returns NULL
  #' @import usethis devtools
  #' @export
  #' @examples

#https://kbroman.org/AdvData/18_rpack_demo.html

# library(usethis)
# library(devtools)

#Avoid building and installing a package in the same session where the package is loaded.
#For example, before running devtools::install(), ensure the package isn't loaded:
devtools::unload("enmods.tools")
#If you're automating builds, ensure cleanup before installing:
unlink(file.path(.libPaths()[1], "00LOCK-enmods.tools"), recursive = TRUE)
#source("./R/load_env_vars.R")
#source("./R/env_config.R")
#update the documentation so any added functions are read in
devtools::document()
#check before installing
devtools::check()
#install the package to use it for a test
devtools::install(reload = TRUE, upgrade = "never", dependencies = TRUE)


# ✖ "aqs-api" is not a valid package name. To be allowed on CRAN, it should:
#   • Contain only ASCII letters, numbers, and '.'.
# • Have at least two characters.
# • Start with a letter.
# • Not end with '.'.
#create_package(str_c(dirname(getwd()), "/code/aqs.api"))
}
