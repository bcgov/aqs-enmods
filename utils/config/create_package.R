#https://kbroman.org/AdvData/18_rpack_demo.html

library(usethis)
source("./utils/config/env_config.R")

# ✖ "aqs-api" is not a valid package name. To be allowed on CRAN, it should:
#   • Contain only ASCII letters, numbers, and '.'.
# • Have at least two characters.
# • Start with a letter.
# • Not end with '.'.
#create_package(str_c(dirname(getwd()), "/code/aqs.api"))