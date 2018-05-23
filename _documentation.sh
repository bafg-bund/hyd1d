#!/usr/bin/env 
Rscript -e 'rmarkdown::render("README.Rmd", "github_document", "README.md")'
Rscript -e 'pkgdown::build_site(".", examples = TRUE, preview = FALSE)'
