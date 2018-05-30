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

#####
# create public/downloads directory and copy hyd1d_*.tar.gz-files into it
from <- list.files(path = dirname(getwd()), 
                   pattern = "hyd1d\\_[:0-9:]\\.[:0-9:]\\.[:0-9:]\\.tar\\.gz",
                   full.names = TRUE)
to <- "public/downloads"
dir.create(to, FALSE, TRUE)
file.copy(from = from, to = to, overwrite = TRUE, copy.date = TRUE)

#####
# document
write("#####", stderr())
write(" document", stderr())

# render the README.md 
if (!(file.exists("README.md"))) {
    rmarkdown::render("README.Rmd", output_format = "github_document", 
                      output_file = "README.md", clean = TRUE)
    unlink("README.html", force = TRUE)
}

# render the package website
pkgdown::build_site(".", examples = TRUE, preview = FALSE)

# insert the BfG logo into the header
files <- list.files(path = "public", pattern = "*[.]html", 
                    all.files = TRUE, full.names = FALSE, recursive = TRUE,
                    ignore.case = FALSE, include.dirs = TRUE, no.. = FALSE)
for (a_file in files){
    x <- readLines(paste0("public/", a_file))
    if (grepl("/", a_file, fixed = TRUE)){
        print(a_file)
        y <- gsub('<a href="http://www.bafg.de">BfG</a>', 
                  '<a href="http://www.bafg.de"><img border="0" src="../bfg_logo.jpg" height="50px" width="114px"></a>', 
                  x)
    } else {
        y <- gsub('<a href="http://www.bafg.de">BfG</a>', 
                  '<a href="http://www.bafg.de"><img border="0" src="bfg_logo.jpg" height="50px" width="114px"></a>', 
                  x)
    }
    # remove the prefix "technical report" in references
    z <- gsub('Technical Report ', '', y)
    cat(z, file = paste0("public/", a_file), sep="\n")
}

# clean up
rm(a_file, files, x, y)

# copy logo
if (!(file.exists("public/bfg_logo.jpg"))){
    file.copy("pkgdown/bfg_logo.jpg", "public")
}

# user and nodename dependent syncs
if (Sys.info()["nodename"] == "hpc-service" & 
    Sys.info()["user"] == "WeberA") {
    system("cp -rp public/* /home/WeberA/public_html/hyd1d/")
}

q("no")

