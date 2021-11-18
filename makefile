SHELL:=/bin/bash

install:
	. /usr/share/Modules/init/bash; \
	module purge; \
	module load i4/R/latest; \
	module list; \
	source /opt/rh/devtoolset-8/enable; \
	Rscript _install.R

pkg:
	. /usr/share/Modules/init/bash; \
	module purge; \
	module load i4/R/latest; \
	module list; \
	Rscript _build.R


aq:
	sed -i -e 's#http://r.bafg.de/~WeberA#https://www.aqualogy.de/R/packages#g' DESCRIPTION
	sed -i -e 's#http://gitlab.lan.bafg.de/auenoekologie#https://git.aqualogy.de/arnd#g' DESCRIPTION
	sed -i -e 's#gitlab.lan.bafg.de/auenoekologie#git.aqualogy.de/arnd#g' README.Rmd
	sed -i -e 's#gitlab.lan.bafg.de/auenoekologie#git.aqualogy.de/arnd#g' README.md
	sed -i -e 's#gitlab.lan.bafg.de/auenoekologie#git.aqualogy.de/arnd#g' vignettes/hyd1d.Rmd
	sed -i -e 's#http://r.bafg.de/~WeberA#https://www.aqualogy.de/R/packages#g' vignettes/hyd1d.Rmd
	sed -i -e 's#gitlab.lan.bafg.de/auenoekologie#git.aqualogy.de/arnd#g' vignettes/vignette_DE.Rmd
	sed -i -e 's#http://r.bafg.de/~WeberA#https://www.aqualogy.de/R/packages#g' vignettes/vignette_DE.Rmd
	sed -i -e 's#http://r.bafg.de/shiny/WeberA/02-#https://www.aqualogy.de/shiny/#g' vignettes/vignette_DE.Rmd
	sed -i -e 's#http://r.bafg.de/shiny/WeberA/05-#https://www.aqualogy.de/shiny/#g' vignettes/vignette_DE.Rmd
	sed -i -e 's#http://r.bafg.de/shiny/WeberA/06-#https://www.aqualogy.de/shiny/#g' vignettes/vignette_DE.Rmd
	sed -i -e 's#http://r.bafg.de/~WeberA#https://www.aqualogy.de/R/packages#g' pkgdown/_pkgdown.yml
	sed -i -e 's#http://r.bafg.de/shiny/WeberA/02-#https://www.aqualogy.de/shiny/#g' pkgdown/_pkgdown.yml
	sed -i -e 's#http://r.bafg.de/shiny/WeberA/05-#https://www.aqualogy.de/shiny/#g' pkgdown/_pkgdown.yml
	sed -i -e 's#http://r.bafg.de/shiny/WeberA/06-#https://www.aqualogy.de/shiny/#g' pkgdown/_pkgdown.yml
	sed -i -e 's#http://r.bafg.de/~WeberA#https://www.aqualogy.de/R/packages#g' inst/CITATION
	sed -i -e 's#http://r.bafg.de/~WeberA#https://www.aqualogy.de/R/packages#g' inst/REFERENCES.bib
