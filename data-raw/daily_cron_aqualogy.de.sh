#!/bin/bash
cd /home/WeberA/BfG/hyd1d/
git pull
Rscript _install.R
Rscript _build.R
Rscript data-raw/daily_pegelonline2gauging_data.R
Rscript data-raw/daily_df.gauging_data.R
chown -R WeberA:WeberA /home/WeberA/BfG/hyd1d/

# sync hyd1d website
export OPTS="-v --recursive --delete --times --no-implied-dirs --iconv=utf8"
rsync $OPTS /home/WeberA/BfG/hyd1d/public/3.4.4/ /var/www/hyd1d
chown -R www-data:www-data /var/www/hyd1d
chown root:www-data /var/www/hyd1d

# exit
exit 0
