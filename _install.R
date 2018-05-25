##################################################
# _install.R
#
# author: arnd.weber@bafg.de
# date:   25.05.2018
#
# purpose: 
#   - install R packages required for the CI jobs
#   - install the repository version of hyd1d
#
##################################################

# configure output
verbose <- TRUE

# standard library path for the package install
R_version <- paste(sep = ".", R.Version()$major, R.Version()$minor)
lib <- paste0("~/R/", R_version, "/")
dir.create(lib, FALSE, TRUE)

# install dependencies
# ROracle (>= 1.1-1) needs an Oracle (Instant)Client
packages <- c("RJSONIO", "RCurl", "plotrix", "Rdpack", "DBI", "ROracle", 
              "RPostgreSQL", "testthat", "knitr", "rmarkdown", "stringr", 
              "devtools", "pkgdown", "roxygen2")

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

# update.packages
update.packages(lib.loc = lib)

# install the local package
require(devtools, lib.loc = lib)
devtools::document(".")
devtools::install(".", reload = FALSE, quick = TRUE, 
                  args = paste0("--library=", lib), quiet = verbose, 
                  dependencies = FALSE)

# exit
q("no")
