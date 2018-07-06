##################################################
# _install.R
#
# author: arnd.weber@bafg.de
# date:   06.07.2018
#
# purpose:
#   - install R packages required for the CI jobs
#   - install the repository version of hyd1d
#
##################################################

# configure output
verbose <- TRUE
quiet <- !verbose

# standard library path for the package install
R_version <- paste(sep = ".", R.Version()$major, R.Version()$minor)
lib <- paste0("~/R/", R_version, "/")
dir.create(lib, verbose, TRUE)

# install dependencies
# ROracle (>= 1.1-1) needs an Oracle (Instant)Client
packages <- c("RJSONIO", "RCurl", "plotrix", "Rdpack", "DBI", "ROracle", 
              "RPostgreSQL", "testthat", "knitr", "rmarkdown", "stringr", 
              "devtools", "pkgdown", "roxygen2", "revealjs")

for (a_package in packages) {
    if (! (a_package %in% installed.packages(lib.loc = lib)[, "Package"])) {
        install.packages(a_package, lib = lib, 
                         repos = "https://ftp.gwdg.de/pub/misc/cran/", 
                         dependencies = TRUE, quiet = quiet)
    }
}

# update.packages
#update.packages(lib.loc = lib, ask = FALSE)

# install the local package
require(devtools, lib.loc = lib)
devtools::install(".", reload = FALSE, quick = TRUE, 
                  args = paste0("--library=", lib), quiet = quiet, 
                  dependencies = FALSE)

# exit
q("no")
