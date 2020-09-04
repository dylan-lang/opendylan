#!/bin/bash -xe

# This script is run from cron.

logfile=/var/log/update-opendylan.org.`date +%Y%m%d%H%M`.log
exec > $logfile 2>&1

dest_dir=/var/www/opendylan.org
repo_dir=/root

./update-website.sh "${dest_dir}" "${repo_dir}"


echo "Done updating opendylan.org"
bzip2 $logfile
# Keep 10 days of logs.
find /var/log -name 'update-opendylan.org.*' -mtime +10 -print -exec rm {} \;
