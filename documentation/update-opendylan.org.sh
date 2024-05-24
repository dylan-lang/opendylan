#!/bin/bash -xe

# This script is intended to be run from a systemd timer.  We use our own log
# file because there can be a lot of output and it seems better not to spam the
# systemd journal.

logfile=/var/log/update-opendylan.org.`date +%Y%m%d%H%M`.log
exec > $logfile 2>&1

export PATH=/root/dylan/bin:/opt/opendylan/bin:/opt/python3-venv/bin:${PATH}

exe_dir="$(realpath $(dirname $0))"
opendylan_dir="$(dirname ${exe_dir})"
dest_dir=/var/www/opendylan.org

# Pull changes to update.sh and the docs.
cd "${opendylan_dir}"
git pull --rebase --tags origin master
git submodule update --init --recursive

${exe_dir}/update.sh "${dest_dir}"

echo "Done updating opendylan.org"
bzip2 $logfile
# Keep 10 days of logs.
find /var/log -name 'update-opendylan.org.*' -mtime +10 -print -exec rm {} \;
