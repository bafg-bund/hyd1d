#!/bin/bash
# download present package repository
cd $hyd1d
git pull

# use R $R_VERSION from the i4 module environment
source /etc/profile.d/modules.sh
module add i4/applications/R-$R_VERSION

# run the daily scripts
Rscript _install.R
Rscript _build.R
Rscript data-raw/daily_pegelonline2gauging_data.R
Rscript data-raw/daily_df.gauging_data.R
chown -R arnd:arnd $hyd1d

# check user, then sync
if [ "$USER" == "root" ]
  then
    # sync hyd1d website
    export FROM=$hyd1d/public/$R_VERSION
    export TO=/var/www/hyd1d
    export OPTS="-v --recursive --delete --times --no-implied-dirs --iconv=utf8"
    rsync $OPTS $FROM $TO
    chown -R www-data:www-data $TO
    chown root:www-data $TO
    
    # set permissions
    chown -R www-data:www-data $TO
    chown root:www-data $TO
    find ${TO}/ -type f -print0 | xargs -0 chmod 0644
    find ${TO}/ -type d -print0 | xargs -0 chmod 0750
    
    # sync df.gauging_data_latest.RDS
    export PUBLIC=/var/www/wordpress/wp-content/uploads/bfg
    export GD=$PUBLIC/df.gauging_data_latest.RDS
    export GD_v2=$PUBLIC/df.gauging_data_latest_v2.RDS
    cp $FROM/downloads/df.gauging_data_latest.RDS $GD
    cp $FROM/downloads/df.gauging_data_latest_v2.RDS $GD_v2
    chown www-data:www-data $GD
    chown www-data:www-data $GD_v2
    chmod 640 $GD
    chmod 640 $GD_v2
fi

# exit
exit 0
