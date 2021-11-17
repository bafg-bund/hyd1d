#!/bin/bash
# use R $R_VERSION from the i4 module environment
source /etc/profile.d/modules.sh
module add R/OS

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
    cp build/$R_VERSION/*.tar.gz /home/arnd/BfG/r.bafg.de/_packages/package_sources
    git rev-parse HEAD > .commit
elif [ $REMOTE = $BASE ]; then
    echo "Need to push"
    git push
    Rscript _install.R
    Rscript _build.R
    cp build/$R_VERSION/*.tar.gz /home/arnd/BfG/r.bafg.de/_packages/package_sources
    git rev-parse HEAD > .commit
else
    echo "Diverged"
    exit 0
fi

# rebuild, if the present commit has not been build
if [ -f .commit ]; then
    export commit_processed=$(cat .commit)
else
    export commit_processed=
fi
export commit_present=$(git rev-parse HEAD)
if [ "$commit_processed" != "$commit_present" ]; then
    echo "The present commit has not been built!"
    Rscript _install.R
    Rscript _build.R
    cp build/$R_VERSION/*.tar.gz /home/arnd/BfG/r.bafg.de/_packages/package_sources
    git rev-parse HEAD > .commit
fi

# run the daily scripts
Rscript data-raw/daily_pegelonline2gauging_data.R
Rscript data-raw/daily_df.gauging_data.R
chown -R arnd:arnd $hyd1d

# check user, then sync
if [ "$USER" == "root" ]; then
    # sync hyd1d website
    export FROM=$hyd1d/public/$R_VERSION/
    export TO=/var/www/R/packages/hyd1d
    mkdir -p $TO
    export OPTS="-v --recursive --times --no-implied-dirs --iconv=utf8"
    rsync $OPTS --delete --exclude 'downloads' $FROM $TO
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
    cp -u $hyd1d/public/downloads/df.gauging_data_latest.RDS $GD
    cp -u $hyd1d/public/downloads/df.gauging_data_latest_v2.RDS $GD_v2
    chown www-data:www-data $GD
    chown www-data:www-data $GD_v2
    chmod 640 $GD
    chmod 640 $GD_v2
fi

# exit
exit 0
