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

# standard library path for the package install
R_version <- paste(sep = ".", R.Version()$major, R.Version()$minor)
lib <- paste0("~/R/", R_version, "/")

# load the packages
require(devtools, lib.loc = lib)
require(DBI, lib.loc = lib)
require(RPostgreSQL, lib.loc = lib)

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
write("#####", stderr())
write(" load_all", stderr())
devtools::load_all(".")

#####
# build documentation
write("#####", stderr())
write(" document", stderr())
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
rm(x, y, today) #, RDO_NROW_DF.GAUGING_STATION_DATA, RDO_NROW_DF.FLYS)


#####
# build vignettes
write("#####", stderr())
write(" build vignettes", stderr())
devtools::build_vignettes(".")

#####
# check the package source
write("#####", stderr())
write(" check", stderr())
devtools::check(".", document = FALSE, manual = FALSE, 
                build_args = "--no-build-vignettes")

#####
# build the source package
write("#####", stderr())
write(" build", stderr())
devtools::build(".", vignettes = FALSE, manual = FALSE)

q("no")

