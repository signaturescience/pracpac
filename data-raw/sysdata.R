## read in and prep use case data to be used internally in pkg
## NOTE: reading from the root of the pracpac project
## NOTE: using base functions to avoid SUGGEST-ing more pkgs

use_cases <- read.csv("inst/templates/use_cases/use_cases.csv",
                      stringsAsFactors = FALSE,
                      row.names = NULL)

usethis::use_data(use_cases, overwrite = TRUE, internal = TRUE)
