##################################################
# _build.R
#
# author: arnd.weber@bafg.de
# date:   23.05.2018
#
# purpose: 
#   - build the repository version of hyd1d
#
##################################################

require(devtools)
require(DBI)
require(RPostgreSQL)

# source hyd1d-internal to obtain the credentials function
source("R/hyd1d-internal.R")

#####
# package the data, if necessary ...
# - reversed order, since some datasets are passed between individual 
#   sourced scripts
source("data-raw/data_date_gauging_data.R")
for (a_file in rev(list.files("data-raw", pattern = "data_df.*", 
                              full.names = TRUE))) {
    source(a_file)
}
rm(a_file)

# unload superfluous packages
detach("package:RPostgreSQL", unload = TRUE)
detach("package:DBI", unload = TRUE)

#####
# minimal devtools workflow
devtools::load_all(".")

#####
# build documentation
devtools::document(".")

# postprocess package documentation
today <- strftime(Sys.Date(), "%Y-%m-%d")

# date_gauging_data
x <- readLines("man/date_gauging_data.Rd")
y <- gsub('$RDO_DATE_GAUGING_DATA$', today, x, fixed = TRUE)
cat(y, file = "man/date_gauging_data.Rd", sep="\n")

# df.gauging_station_data
x <- readLines("man/df.gauging_station_data.Rd")
y <- gsub('$RDO_NROW_DF.GAUGING_STATION_DATA$', 
          RDO_NROW_DF.GAUGING_STATION_DATA, x, fixed = TRUE)
cat(y, file = "man/df.gauging_station_data.Rd", sep="\n")

# df.flys
x <- readLines("man/df.flys.Rd")
y <- gsub('$RDO_NROW_DF.FLYS$', RDO_NROW_DF.FLYS, x, 
          fixed = TRUE)
cat(y, file = "man/df.flys.Rd", sep="\n")

# clean up
rm(x, y, today, RDO_NROW_DF.GAUGING_STATION_DATA, RDO_NROW_DF.FLYS)

#####
# check the package source
devtools::check(".")

#####
# install the package from source
#devtools::install(".")

#####
# build the source package
devtools::build(".")

q("no")

