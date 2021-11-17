#!/usr/bin/bash
cd /srv/cifs-mounts/WeberA_home/WeberA/hyd1d

# load R-OS
source /etc/profile.d/modules.sh
module purge
module load i4/R/latest
module list

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
    cp -u build/$R_VERSION/*.tar.gz /home/WeberA/freigaben_r/_packages/package_sources
    git rev-parse HEAD > .commit
elif [ $REMOTE = $BASE ]; then
    echo "Need to push"
    git push
    Rscript _install.R
    Rscript _build.R
    cp -u build/$R_VERSION/*.tar.gz /home/WeberA/freigaben_r/_packages/package_sources
    git rev-parse HEAD > .commit
else
    echo "Diverged"
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
    cp build/$R_VERSION/*.tar.gz /home/WeberA/freigaben_r/_packages/package_sources
    git rev-parse HEAD > .commit
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
