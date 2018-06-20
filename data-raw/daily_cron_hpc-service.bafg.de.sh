#!/usr/bin/bash
cd /srv/cifs-mounts/WeberA_home/WeberA/hyd1d
git pull
Rscript _install.R
Rscript _build.R
Rscript data-raw/daily_pegelonline2gauging_data.R
Rscript data-raw/daily_df.gauging_data.R
chown -R WeberA:users /srv/cifs-mounts/WeberA_home/WeberA/hyd1d

# sync hyd1d website
export OPTS="-v --recursive --delete --times --no-implied-dirs --iconv=utf8"
rsync $OPTS /srv/cifs-mounts/WeberA_home/WeberA/hyd1d/public/3.5.0/ /home/WeberA/public_html/hyd1d
chown -R WeberA:users /home/WeberA/public_html/hyd1d

# exit
exit 0
