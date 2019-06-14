##################################################
# _install.R
#
# author: arnd.weber@bafg.de
# date:   04.02.2019
#
# purpose:
#   - install R packages required for the CI jobs
#   - install the repository version of hyd1d
#
##################################################

# update.packages
update.packages(lib.loc = .libPaths()[1], ask = FALSE, checkBuilt = TRUE)

# install dependencies
# ROracle (>= 1.1-1) needs an Oracle (Instant)Client
packages <- c("RJSONIO", "RCurl", "plotrix", "Rdpack", "DBI", "ROracle", 
              "RPostgreSQL", "testthat", "knitr", "rmarkdown", "stringr", 
              "devtools", "pkgdown", "roxygen2", "revealjs", "shiny", 
              "shinyTime", "lubridate", "usethis")

for (a_package in packages) {
    if (! (a_package %in% installed.packages()[, "Package"])) {
        install.packages(a_package, dependencies = TRUE)
    }
}

# install the local package
require(devtools)
devtools::install(".", quick = TRUE, dependencies = TRUE)

# exit
q("no")
