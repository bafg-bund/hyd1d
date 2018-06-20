#!/bin/bash
cd /home/WeberA/hyd1d/
git pull
Rscript _install.R
Rscript _build.R
Rscript data-raw/daily_pegelonline2gauging_data.R
Rscript data-raw/daily_df.gauging_data.R
chown -R WeberA:WeberA /home/WeberA/hyd1d/

# sync hyd1d website
export OPTS="-v --recursive --delete --times --no-implied-dirs --iconv=utf8"
rsync $OPTS /home/WeberA/hyd1d/public/3.5.0/ /var/www/vhosts/lvps46-163-72-150.dedicated.hosteurope.de/aqualogy-cloud.de/hyd1d
chown -R apache:apache /var/www/vhosts/lvps46-163-72-150.dedicated.hosteurope.de/aqualogy-cloud.de/hyd1d

# exit
exit 0
