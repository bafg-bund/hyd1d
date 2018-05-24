#!/usr/bin/env 
Rscript -e 'rmarkdown::render("README.Rmd", output_format = "github_document", output_file = "README.md", clean = TRUE)'
Rscript -e 'unlink("README.html", force = TRUE)'
Rscript -e 'pkgdown::build_site(".", examples = TRUE, preview = FALSE)'
