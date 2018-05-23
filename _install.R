##################################################
# _install.R
#
# author: arnd.weber@bafg.de
# date:   23.05.2018
#
# purpose: 
#   - install R packages required for the CI jobs
#   - install the repository version of hyd1d
#
##################################################

# make this script executable only on aqualogy-cloud.de
if (Sys.info()["nodename"] != "lvps46-163-72-150.dedicated.hosteurope.de") {
    print("This script has to be executed on aqualogy-cloud.de!")
    q("no")
}

# configure output
verbose <- TRUE

# standard library path for the package install
lib <- .libPaths()[1]

# install dependencies
packages <- c("DBI", "RPostgreSQL", "RCurl", "RJSONIO", "plotrix", "Rdpack",
              "testthat", "knitr", "rmarkdown", "stringr", "devtools")
# ROracle (>= 1.1-1) needs an Oracle (Instant)Client

for (a_package in packages) {
    if (! (a_package %in% installed.packages(lib.loc = lib)[, "Package"])) {
        if (verbose) {
            print(paste0("Install ", a_package))
            install.packages(a_package, lib = lib, 
                             repos = "https://ftp.gwdg.de/pub/misc/cran/", 
                             dependencies = TRUE, quiet = FALSE)
        } else {
            install.packages(a_package, lib = lib, 
                             repos = "https://ftp.gwdg.de/pub/misc/cran/", 
                             dependencies = TRUE, quiet = TRUE)
        }
    }
}

# install hyd1d
require("devtools")
devtools::install(".")

# exit
q("no")