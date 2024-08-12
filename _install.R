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
# "ROracle"
packages <- c("jsonlite", "Rdpack", "DBI", "RPostgreSQL", "testthat",
              "knitr", "rmarkdown", "stringr", "devtools", "httr2", "curl",
              "pkgdown", "roxygen2", "revealjs", "shiny", "shiny.i18n",
              "shinyTime", "lubridate", "usethis", "bslib", "xml2")

for (a_package in packages) {
    if (! (a_package %in% installed.packages()[, "Package"])) {
        install.packages(a_package, dependencies = TRUE)
    }
}

# install the local package
library(devtools)
devtools::install(".", quick = TRUE, dependencies = TRUE)

# exit
q("no")
