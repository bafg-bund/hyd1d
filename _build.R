##################################################
# _build.R
#
# author: arnd.weber@bafg.de
# date:   30.05.2018
#
# purpose: 
#   - build the repository version of hyd1d
#
##################################################

# configure output
verbose <- TRUE
quiet <- !verbose

# standard library path for the package install
R_version <- paste(sep = ".", R.Version()$major, R.Version()$minor)
lib <- paste0("~/R/", R_version, "/")

# output paths
build <- paste0("build/", R_version)
dir.create(build, verbose, TRUE)
public <- paste0("public/", R_version)
dir.create(public, verbose, TRUE)
downloads <- paste0("public/", R_version, "/downloads")
dir.create(downloads, verbose, TRUE)

# load the packages
require(devtools, lib.loc = lib)
require(DBI, lib.loc = lib)
require(RPostgreSQL, lib.loc = lib)
require(knitr, lib.loc = lib)
require(rmarkdown, lib.loc = lib)
require(pkgdown, lib.loc = lib)

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
devtools::build(".", path = build, vignettes = FALSE, manual = FALSE)

#####
# create public/downloads directory and copy hyd1d_*.tar.gz-files into it
from <- list.files(path = build, 
                   pattern = "hyd1d\\_[:0-9:]\\.[:0-9:]\\.[:0-9:]\\.tar\\.gz",
                   full.names = TRUE)
file.copy(from = from, to = downloads, overwrite = TRUE, copy.date = TRUE)

#####
# install hyd1d from source
write("#####", stderr())
write(" install from source", stderr())

pkg_files <- list.files(path = build, 
                        pattern = paste0("hyd1d\\_[:0-9:]\\.[:0-9:]\\.[:0-9:]",
                                         "\\.tar\\.gz"))

for (a_file in pkg_files) {
    
    write(a_file, stderr())
    
    # seperate package name from version string
    package_name <- unlist(strsplit(a_file, "_"))[1]
    package_version <- gsub(".tar.gz", "", unlist(strsplit(a_file, "_"))[2])
    
    # check presently installed local packages
    pkgs <- as.data.frame(installed.packages(lib.loc = lib))
    if (package_name %in% pkgs$Package) {
        if (compareVersion(as.character(packageVersion(package_name, 
                                                       lib.loc = lib)), 
                           package_version) < 1) {
            install.packages(paste(build, a_file, sep = "/"), 
                             lib = lib, dependencies = TRUE, quiet = quiet)
        }
    } else {
        install.packages(paste(build, a_file, sep = "/"), 
                         lib = lib, dependencies = TRUE, quiet = quiet)
    }
}

#####
# export the documentation as pdf
write("#####", stderr())
write(" export the documentation as pdf", stderr())

system(paste0("R CMD Rd2pdf . --output=", downloads, "/hyd1d.pdf --no-preview ",
              "--force --RdMacros=Rdpack --encoding=UTF-8 --outputEncoding=UTF",
              "-8"))

if (R_version != "3.4.4") {
    q("no")
}

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

# # render the package website
# pkgdown::build_site(".", examples = TRUE, preview = FALSE, document = FALSE, 
#                     override = list(destination = public))
# 
# # insert the BfG logo into the header
# files <- list.files(path = public, pattern = "*[.]html", 
#                     all.files = TRUE, full.names = FALSE, recursive = TRUE,
#                     ignore.case = FALSE, include.dirs = TRUE, no.. = FALSE)
# for (a_file in files){
#     x <- readLines(paste0(public, "/", a_file))
#     if (grepl("/", a_file, fixed = TRUE)){
#         if (verbose) {
#             print(a_file)
#         }
#         y <- gsub('<a href="http://www.bafg.de">BfG</a>', 
#                   paste0('<a href="http://www.bafg.de"><img border="0" src="..',
#                          '/bfg_logo.jpg" height="50px" width="114px"></a>'), x)
#     } else {
#         y <- gsub('<a href="http://www.bafg.de">BfG</a>', 
#                   paste0('<a href="http://www.bafg.de"><img border="0" src="bf',
#                          'g_logo.jpg" height="50px" width="114px"></a>'), x)
#     }
#     # remove the prefix "technical report" in references
#     z <- gsub('Technical Report ', '', y)
#     cat(z, file = paste0(public, "/", a_file), sep="\n")
# }
# 
# # clean up
# rm(a_file, files, x, y)
# 
# # copy logo
# if (!(file.exists(paste0(public, "/bfg_logo.jpg")))){
#     file.copy("pkgdown/bfg_logo.jpg", public)
# }

# user, nodename and version dependent sync to web roots and install directories
if (Sys.info()["nodename"] == "hpc-service" & 
    Sys.info()["user"] == "WeberA") {
    system("cp -rp public/3.4.4/* /home/WeberA/public_html/hyd1d/")
    system(paste0("[ -d /home/WeberA/freigaben/AG/R/server/server_admin/packag",
                  "e_sources ] || cp -rp public/3.4.4/downloads/hyd1d_*.tar.gz",
                  " /home/WeberA/freigaben/AG/R/server/server_admin/package_so",
                  "urces"))
}

q("no")

