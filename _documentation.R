
require(rmarkdown)
require(pkgdown)

# render the README.md 
if (!(file.exists("README.md"))) {
    rmarkdown::render("README.Rmd", output_format = "github_document", output_file = "README.md", clean = TRUE)
    unlink("README.html", force = TRUE)
}

# render the package website
pkgdown::build_site(".", examples = TRUE, preview = FALSE)

# exit
q("no")

