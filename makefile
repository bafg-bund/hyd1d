SHELL:=/bin/bash

install:
	. /usr/share/Modules/init/bash; \
	module purge; \
	module load R/latest; \
	source /opt/rh/gcc-toolset-13/enable; \
	Rscript _install.R

pkg:
	. /usr/share/Modules/init/bash; \
	module purge; \
	module load R/latest; \
	Rscript _build.R

dev:
	. /usr/share/Modules/init/bash; \
	module purge; \
	module load R/devel; \
	Rscript -e 'devtools::check(".", document = TRUE, manual = TRUE, error_on = "never", build_args = c("--compact-vignettes=both"))'

pdfde:
	. /usr/share/Modules/init/bash; \
	module purge; \
	module load R/latest; \
	R -e 'rmarkdown::render("vignettes/vignette_DE.Rmd", output_format = "pdf_document")'

www:
	rsync -auv --delete /srv/cifs-mounts/WeberA_home/WeberA/hyd1d/docs/ /srv/cifs-mounts/WeberA/U/U3/Auengruppe_INFORM/Weber_etal_2022_hyd1d_hydflood/www/hyd1d
