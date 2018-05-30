
print(.libPaths())

# standard library path for the package install
R_version <- paste(sep = ".", R.Version()$major, R.Version()$minor)
lib <- paste0("~/R/", R_version, "/")

# load the packages
require(knitr, lib.loc = lib)
require(rmarkdown, lib.loc = lib)
require(pkgdown, lib.loc = lib)

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

# articles/hyd1d.html
x <- readLines("public/articles/hyd1d.html")
y <- gsub('../../../../../srv/cifs-mounts/WeberA_home/WeberA/hyd1d/hyd1d/vignettes/', 
          '', x)
cat(y, file = "public/articles/hyd1d.html", sep="\n")
rm(a_file, files, x, y)

# copy logo
if (!(file.exists("public/bfg_logo.jpg"))){
    file.copy("pkgdown/bfg_logo.jpg", "public")
}

# exit
q("no")

