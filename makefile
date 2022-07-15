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

www:
	rsync -auv --delete /srv/cifs-mounts/WeberA_home/WeberA/hyd1d/docs/ /srv/cifs-mounts/WeberA/U/U3/Auengruppe_INFORM/Weber_etal_2022_hyd1d_hydflood/www/hyd1d
	find /srv/cifs-mounts/WeberA/U/U3/Auengruppe_INFORM/Weber_etal_2022_hyd1d_hydflood/www/hyd1d/. -iname "*.html" -exec sed -i -e 's#<a class="external-link dropdown-item" href="http://r.bafg.de/shiny/WeberA/07-flood3/">flood3()</a># #g' {} \;
	find /srv/cifs-mounts/WeberA/U/U3/Auengruppe_INFORM/Weber_etal_2022_hyd1d_hydflood/www/hyd1d/. -iname "*.html" -exec sed -i -e 's#http://r.bafg.de/shiny/WeberA/08-flood3wms/#https://hydflood.bafg.de/apps/flood3wms/#g' {} \;
	find /srv/cifs-mounts/WeberA/U/U3/Auengruppe_INFORM/Weber_etal_2022_hyd1d_hydflood/www/hyd1d/. -iname "*.html" -exec sed -i -e 's#http://r.bafg.de/shiny/WeberA/10-flood3daily/#https://hydflood.bafg.de/apps/flood3daily/#g' {} \;

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
