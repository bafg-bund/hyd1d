#!/bin/bash
# download present package repository
cd $hyd1d
git pull

# use R 3.5.0 from the i4 module environment
source /etc/profile.d/modules.sh
module add i4/applications/R-3.5.0

# run the daily scripts
#Rscript _install.R
#Rscript _build.R
Rscript data-raw/daily_pegelonline2gauging_data.R
Rscript data-raw/daily_df.gauging_data.R
chown -R arnd:arnd $hyd1d

# sync hyd1d website
#export OPTS="-v --recursive --delete --times --no-implied-dirs --iconv=utf8"
#rsync $OPTS $hyd1d/public/3.5.0/ /var/www/hyd1d
#chown -R www-data:www-data /var/www/hyd1d
#chown root:www-data /var/www/hyd1d

# exit
exit 0
