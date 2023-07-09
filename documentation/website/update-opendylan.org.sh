#!/bin/bash -xe

# This script is intended to be run from cron.

logfile=/var/log/update-opendylan.org.`date +%Y%m%d%H%M`.log
exec > $logfile 2>&1

dest_dir=/var/www/opendylan.org
opendylan_dir=/root/deploy-opendylan.org/opendylan
gendoc_exe=/root/deploy-opendylan.org/gendoc/_build/bin/gendoc

exe_dir="$(realpath $(dirname $0))"

${exe_dir}/update.sh "${dest_dir}" "${opendylan_dir}" "${gendoc_exe}"

echo "Done updating opendylan.org"
bzip2 $logfile
# Keep 10 days of logs.
find /var/log -name 'update-opendylan.org.*' -mtime +10 -print -exec rm {} \;
