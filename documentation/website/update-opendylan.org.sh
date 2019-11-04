#!/bin/bash -xe

# This script is run from cron.

# TODO:
# * Build/install /root/dylan-programming-book into
#   /var/www/opendylan.org/books/dpg.
# * Build/install /root/dylan-reference-manual into
#   /var/www/opendylan.org/books/drm.

# For completeness, note that /var/www/opendylan.org/downloads is
# handled specially. As far as I (cgay) know, new versions of Open
# Dylan must be installed there by hand and then download/index.html
# must be updated in the website repo.

logfile=/var/log/update-opendylan.org.`date +%Y%m%d%H%M`.log
exec > $logfile 2>&1

site_root=/var/www/opendylan.org
opendylan=/root/opendylan
website=/root/website

# Update the main web site pages.
cd ${website}
git pull --rebase origin master
git submodule update --recursive --init
make html
rsync -avz build/html/ ${site_root}


# Update the docs contained in the opendylan repository.

subdirs="building-with-duim duim-reference getting-started-cli getting-started-ide"
subdirs="${subdirs} hacker-guide intro-dylan library-reference man-pages"
subdirs="${subdirs} project-notebook release-notes style-guide"

cd ${opendylan}
git pull --rebase origin master
git submodule update --recursive --init
export PYTHONPATH=${opendylan}/documentation/sphinx-extensions

for subdir in ${subdirs}; do
    cd ${opendylan}/documentation/${subdir}
    make html && rsync -avz build/html/ ${site_root}/documentation/${subdir}/
    make epub && cp build/epub/*.epub ${site_root}/documentation/${subdir}/
    make latexpdf && cp build/latex/*.pdf ${site_root}/documentation/${subdir}/
done

# Update library docs.
# For now these are one-offs, until we decide on a standard.

for lib in binary-data concurrency http melange objc-dylan statistics testworks tracing; do
    cd /root/${lib}
    git pull --rebase origin master
    git submodule update --recursive --init
    cd /root/${lib}/documentation
    if [[ "${lib}" == "testworks" ]]; then
	cd users-guide
    fi
    make html && rsync -avz build/html/ ${site_root}/documentation/${lib}/
    make epub && cp build/epub/*.epub ${site_root}/documentation/${lib}/
    make latexpdf && cp build/latex/*.pdf ${site_root}/documentation/${lib}/
done


# Finish up.
echo "Done updating opendylan.org"
bzip2 $logfile
# Keep 10 days of logs.
find /var/log -name 'update-opendylan.org.*' -mtime +10 -print -exec rm {} \;
