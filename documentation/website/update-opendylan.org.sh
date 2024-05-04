#!/bin/bash -xe

# This script is intended to be run from cron.

logfile=/var/log/update-opendylan.org.`date +%Y%m%d%H%M`.log
exec > $logfile 2>&1

exe_dir="$(realpath $(dirname $0))"

dest_dir=/var/www/opendylan.org

# Update the repo first so we get any changes to the update.sh script,
# dependencies, etc.
git pull --rebase origin master

${exe_dir}/update.sh "${dest_dir}"

echo "Done updating opendylan.org in ${dest_DIR}."
bzip2 $logfile
# Keep 10 days of logs.
find /var/log -name 'update-opendylan.org.*' -mtime +10 -print -exec rm {} \;
