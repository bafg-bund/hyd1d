#!/bin/bash
# use R $R_VERSION from the i4 module environment
source /etc/profile.d/modules.sh
module add i4/applications/R-$R_VERSION

# download present package repository
cd $hyd1d

# compare local repository with remote 'origin'
git fetch origin
UPSTREAM=${1:-'@{u}'}
LOCAL=$(git rev-parse @)
REMOTE=$(git rev-parse "$UPSTREAM")
BASE=$(git merge-base @ "$UPSTREAM")

if [ $LOCAL = $REMOTE ]; then
    echo "Up-to-date"
elif [ $LOCAL = $BASE ]; then
    echo "Need to pull"
    git pull
    Rscript _install.R
    Rscript _build.R
elif [ $REMOTE = $BASE ]; then
    echo "Need to push"
    git push
    Rscript _install.R
    Rscript _build.R
else
    echo "Diverged"
fi

# run the daily scripts
Rscript data-raw/daily_pegelonline2gauging_data.R
Rscript data-raw/daily_df.gauging_data.R
chown -R arnd:arnd $hyd1d

# check user, then sync
if [ "$USER" == "root" ]
  then
    # sync hyd1d website
    export FROM=$hyd1d/public/$R_VERSION/
    export TO=/var/www/hyd1d
    export OPTS="-v --recursive --delete --times --no-implied-dirs --iconv=utf8"
    rsync $OPTS --exclude 'downloads' $FROM $TO
    rsync $OPTS $hyd1d/public/downloads $TO
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
    cp $hyd1d/public/downloads/df.gauging_data_latest.RDS $GD
    cp $hyd1d/public/downloads/df.gauging_data_latest_v2.RDS $GD_v2
    chown www-data:www-data $GD
    chown www-data:www-data $GD_v2
    chmod 640 $GD
    chmod 640 $GD_v2
fi

# exit
exit 0
