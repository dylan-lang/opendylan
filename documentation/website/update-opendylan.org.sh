#!/bin/bash -xe

# This script is intended to be run from cron.

logfile=/var/log/update-opendylan.org.`date +%Y%m%d%H%M`.log
exec > $logfile 2>&1

exe_dir="$(realpath $(dirname $0))"

repo_dir=/root/deploy-opendylan.org
gendoc_exe=${repo_dir}/gendoc/_build/bin/gendoc
dest_dir=/var/www/opendylan.org

# Update opendylan and website submodule first so we get any changes
# to the update.sh script.
cd ${repo_dir}/opendylan
git pull --rebase origin master
git submodule update --init --recursive

${exe_dir}/update.sh "${dest_dir}" "${repo_dir}" "${gendoc_exe}"

echo "Done updating opendylan.org"
bzip2 $logfile
# Keep 10 days of logs.
find /var/log -name 'update-opendylan.org.*' -mtime +10 -print -exec rm {} \;
