#!/usr/bin/bash
cd /srv/cifs-mounts/WeberA_home/WeberA/hyd1d

# compare local repository with remote 'origin'
git fetch origin
UPSTREAM=${1:-'@{u}'}
LOCAL=$(git rev-parse @{0})
REMOTE=$(git rev-parse "$UPSTREAM")
BASE=$(git merge-base @{0} "$UPSTREAM")

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
#Rscript data-raw/daily_waterLevels.R
chown -R WeberA:users /srv/cifs-mounts/WeberA_home/WeberA/hyd1d

# sync hyd1d website
#export OPTS="-v --recursive --delete --times --no-implied-dirs --iconv=utf8"
#rsync $OPTS /srv/cifs-mounts/WeberA_home/WeberA/hyd1d/public/$R_VERSION/ /home/WeberA/public_html/hyd1d
#chown -R WeberA:users /home/WeberA/public_html/hyd1d
#find /home/WeberA/public_html/hyd1d/ -type f -print0 | xargs -0 chmod 0644
#find /home/WeberA/public_html/hyd1d/ -type d -print0 | xargs -0 chmod 0755

# copy data
cp -u /srv/cifs-mounts/WeberA_home/WeberA/hyd1d/public/downloads/*.RDS /home/WeberA/public_html/hyd1d/downloads/
chmod 644 /home/WeberA/public_html/hyd1d/downloads/*.RDS

# exit
exit 0
