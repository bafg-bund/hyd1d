##################################################
# _build.R
#
# author: arnd.weber@bafg.de
# date:   17.08.2018
#
# purpose: 
#   - build the repository version of hyd1d
#
##################################################

#####
# assemble variables, create output directories and load packages
write("#####", stdout())
write(" determine output directories", stdout())

# version
R_version <- as.character(getRversion())

# output paths
build <- paste0("built/", R_version)
dir.create(build, FALSE, TRUE)
public <- paste0("docs/")
dir.create(public, FALSE, TRUE)

#####
# load the packages
write("#####", stdout())
write(" load packages", stdout())
require(devtools)
require(usethis)
require(DBI)
require(RPostgreSQL)
require(knitr)
require(rmarkdown)
require(pkgdown)
require(revealjs)

#####
# assemble variables and create output directories
write("#####", stdout())
write(" set en_US locale", stdout())
Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")
Sys.setlocale(category = "LC_PAPER", locale = "en_US.UTF-8")
Sys.setlocale(category = "LC_MEASUREMENT", locale = "en_US.UTF-8")
Sys.setlocale(category = "LC_MESSAGES", locale = "en_US.UTF-8")

#####
# assemble variables and create output directories
write("#####", stdout())
write(" sessionInfo", stdout())
sessionInfo()

#####
# package the data, if necessary ...
# - reversed order, since some datasets are passed between individual
#   sourced scripts
write("#####", stdout())
write(" data-raw", stdout())

# source hyd1d-internal to obtain the credentials function
source("R/hyd1d-internal.R")

# prepare data
for (a_file in rev(list.files("data-raw", pattern = "data_df.*",
                              full.names = TRUE))) {
    source(a_file)
}
rm(a_file)

# unload superfluous packages
detach("package:RPostgreSQL", unload = TRUE)
detach("package:DBI", unload = TRUE)
rm(list = c("credentials", "simpleCap", "readzrx"))

#####
# minimal devtools workflow
write("#####", stdout())
write(" load_all", stdout())
devtools::load_all(".")

#####
# build documentation
write("#####", stdout())
write(" document", stdout())
devtools::document(".")

#####
# build vignettes
write("#####", stdout())
write(" build vignettes", stdout())
devtools::build_vignettes(".", clean = FALSE)
tools::compactPDF(paths = "doc", gs_quality = "ebook")

#####
# check the package source
write("#####", stdout())
write(" check", stdout())
devtools::check(".", document = TRUE, manual = TRUE, error_on = "never",
                build_args = c('--compact-vignettes=both'))

#####
# build the source package
write("#####", stdout())
write(" build", stdout())
devtools::build(".", path = build, vignettes = TRUE, manual = TRUE,
                args = c("--compact-vignettes=both"))

#####
# install hyd1d from source
write("#####", stdout())
write(" install from source", stdout())

pkg_files <- list.files(path = build,
                        pattern = paste0("hyd1d\\_[:0-9:]\\.[:0-9:]\\.[:0-9:]",
                                         "\\.tar\\.gz"))

for (a_file in pkg_files) {

    write(a_file, stdout())

    # seperate package name from version string
    package_name <- unlist(strsplit(a_file, "_"))[1]
    package_version <- gsub(".tar.gz", "", unlist(strsplit(a_file, "_"))[2])

    # check presently installed local packages
    pkgs <- as.data.frame(installed.packages())
    if (package_name %in% pkgs$Package) {
        if (compareVersion(as.character(packageVersion(package_name)),
                           package_version) < 1) {
            install.packages(paste(build, a_file, sep = "/"),
                             dependencies = TRUE)
        }
    } else {
        install.packages(paste(build, a_file, sep = "/"),
                         dependencies = TRUE)
    }
}

#####
# document
write("#####", stdout())
write(" document git & website", stdout())

# render the README.md
if (!(file.exists("README.md"))) {
    rmarkdown::render("README.Rmd", output_format = "github_document",
                      output_file = "README.md", clean = TRUE)
    unlink("README.html", force = TRUE)
}

# render the package website 
#pkgdown::clean_site(".")
pkgdown::build_site(".", examples = TRUE, preview = FALSE, new_process = TRUE)

# insert the BfG logo into the header
files <- list.files(path = public, pattern = "*[.]html",
                    all.files = TRUE, full.names = FALSE, recursive = TRUE,
                    ignore.case = FALSE, include.dirs = TRUE, no.. = FALSE)
for (a_file in files){
    x <- readLines(paste0(public, a_file))
    if (grepl("/", a_file, fixed = TRUE)){
        write(a_file, stdout())
        y <- gsub('href="https://www.bafg.de">BfG</a>',
                  paste0('href="https://www.bafg.de"><img border="0" src="..',
                         '/bfg_logo.jpg" height="50px" width="114px"></a>'), x)
    } else {
        y <- gsub('href="https://www.bafg.de">BfG</a>',
                  paste0('href="https://www.bafg.de"><img border="0" src="bf',
                         'g_logo.jpg" height="50px" width="114px"></a>'), x)
    }
    # edit footer
    y <- gsub('Developed by Arnd Weber, Marcus Hatz.',
              paste0('Developed by Arnd Weber, Marcus Hatz. <a href="https',
                     '://www.bafg.de/EN/Service/Imprint/imprint_node.html"',
                     '>Imprint</a>.'),
              y)
    # remove the prefix "technical report" in references
    z <- gsub('Technical Report ', '', y)
    cat(z, file = paste0(public, a_file), sep="\n")
}

# clean up
rm(a_file, files, x, y)

# copy logo
if (!(file.exists(paste0(public, "bfg_logo.jpg")))){
    file.copy("pkgdown/bfg_logo.jpg", public)
}

#####
# create public/downloads directory and copy hyd1d_*.tar.gz-files into it
downloads <- paste0(public, "downloads")
dir.create(downloads, FALSE, TRUE)
from <- list.files(path = build,
                   pattern = "hyd1d\\_[:0-9:]\\.[:0-9:]\\.[:0-9:]\\.tar\\.gz",
                   full.names = TRUE)
file.copy(from = from, to = downloads, overwrite = TRUE, copy.date = TRUE)

#####
# export the documentation as pdf
write("#####", stdout())
write(" export the documentation as pdf", stdout())

system(paste0("R CMD Rd2pdf . --output=", downloads, "/hyd1d.pdf --no-preview ",
              "--force --RdMacros=Rdpack --encoding=UTF-8 --outputEncoding=UTF",
              "-8"))

#####
# presentation
if (!file.exists("presentation/presentation_DE.html")) {
    system("R -e 'rmarkdown::render(\"presentation/presentation_DE.Rmd\")'",
           intern = FALSE, wait = TRUE)
}

# copy external image and video files
from <- "presentation/presentation_DE.html"
from <- append(from, list.files(path = "presentation", pattern = "*\\.png",
                                full.names = TRUE))
from <- append(from, list.files(path = "presentation", pattern = "*\\.css",
                                full.names = TRUE))
from <- append(from, list.files(path = "presentation", pattern = "*\\.mp4",
                                full.names = TRUE))
file.copy(from = from,
          to = gsub("presentation/", paste0(public, "articles/"), from),
          overwrite = TRUE, copy.date = TRUE)

#####
# document
# user, nodename and version dependent sync to web roots and install directories
write("#####", stdout())
write(" web", stdout())

host <- Sys.info()["nodename"]
user <- Sys.info()["user"]
if (host == "pvil-r" & user == "WeberA" & R_version == "4.2.2") {
    # copy html output to ~/public_html
    system(paste0("cp -rp ", public, "* /home/", user, "/public_html/hyd1d/"))
    system("permissions_html")
    
    # copy shinyapps to ~/ShinyApps
    system(paste0("cp -rp shinyapps/gauging_data/* /home/", user, "/ShinyApps/",
                  "02-gauging_data"))
    system(paste0("cp -rp shinyapps/waterLevel/* /home/", user, "/ShinyApps/",
                  "05-waterlevel"))
    system(paste0("cp -rp shinyapps/waterLevelPegelonline/* /home/", user,
                  "/ShinyApps/06-waterlevelpegelonline"))
    system("permissions_shiny")
    
    # copy package source to r.bafg.de
    system(paste0("[ -d /home/", user, "/freigaben_r/_packages/package_sources",
                  " ] && cp -rp ", public, "downloads/hyd1d_*.tar.gz /home/",
                  user, "/freigaben_r/_packages/package_sources"))
    
}

q("no")

