#!/bin/bash

# sync hyd1d website
#export OPTS="-v --recursive --delete --times --no-implied-dirs --iconv=utf8"
#rsync $OPTS /home/gitlab-runner/builds/755538b8/0/arnd/hyd1d/public/ /var/www/vhosts/lvps46-163-72-150.dedicated.hosteurope.de/aqualogy-cloud.de/hyd1d
#chown apache:apache /var/www/vhosts/lvps46-163-72-150.dedicated.hosteurope.de/aqualogy-cloud.de/hyd1d

# export data at the 6:10 run
H=$(date "+%k")
if (( 6 <= H && H < 7 )); then
  echo "Export of df.gauging_data"
  cp -p /home/gitlab-runner/builds/755538b8/0/arnd/hyd1d/public/downloads/df.gauging_data_latest.rda /var/www/vhosts/lvps46-163-72-150.dedicated.hosteurope.de/aqualogy-cloud.de/downloads/df.gauging_data_latest.rda
  chown arnd.weber:psacln /var/www/vhosts/lvps46-163-72-150.dedicated.hosteurope.de/aqualogy-cloud.de/downloads/df.gauging_data_latest.rda
else
  echo "No export of df.gauging_data"
fi

# exit
exit 0
