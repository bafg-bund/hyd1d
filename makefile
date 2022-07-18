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
