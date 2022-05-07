#!/bin/bash -xe

# This script assumes that all repos (website, opendylan, and various
# libraries) are checked out into the same directory. To test, clone all
# necessary repositories (including this one) into a single directory and then:
#
#   mkdir /tmp/www; ./website/update.sh /tmp/www .

# /var/www/opendylan.org/downloads is handled specially. New versions of Open
# Dylan must be installed there by hand and then download/index.html must be
# updated in the website repo.

# TODO:
# * Fix latexpdf vs python3 issues. Disabled PDF for now.
# * Link to GitHub releases for the download pages instead of maintaining
#   downloads by hand.
# * Clone repos if not already present. This should use the package manager
#   since they're spread across different GitHub orgs and we don't want to
#   maintain that info in multiple places.
# * Build/install dylan-programming-book into ${dest_dir}/books/dpg.
# * Build/install dylan-reference-manual into ${dest_dir}/books/drm.


if [[ $# -ne 2 ]]; then
    echo "Usage: `basename $0` dest_dir repo_dir"
    echo "  dest_dir:  directory in which to build the website. e.g. /var/www/opendylan.org"
    echo "  repo_dir:  directory containing all necessary git repos"
    exit 2
fi

dest_dir=$(realpath "$1")
repo_dir=$(realpath "$2")       # so we can cd with wild abandon below

# Update the main web site pages.
cd ${repo_dir}/website
git pull --rebase origin master
git submodule update --recursive --init
make html
rsync -avz build/html/ ${dest_dir}


# Update the docs contained in the opendylan repository.

subdirs="building-with-duim duim-reference getting-started-cli getting-started-ide"
subdirs="${subdirs} hacker-guide intro-dylan library-reference man-pages"
subdirs="${subdirs} project-notebook release-notes style-guide corba-guide"

cd ${repo_dir}/opendylan
git pull --rebase origin master
git submodule update --recursive --init
export PYTHONPATH=${repo_dir}/opendylan/documentation/sphinx-extensions

for subdir in ${subdirs}; do
    cd ${repo_dir}/opendylan/documentation/${subdir}
    make html && rsync -avz build/html/ ${dest_dir}/documentation/${subdir}/
    make epub && cp build/epub/*.epub ${dest_dir}/documentation/${subdir}/
    # Disable PDF for now. For some reason it blows out the tex stack in python3.
    # make latexpdf && cp build/latex/*.pdf ${dest_dir}/documentation/${subdir}/
done

# Update library docs.
# For now these are one-offs, until we decide on a standard.

libraries="binary-data concurrency dylan-tool http melange objc-dylan statistics testworks tracing"

for lib in ${libraries}; do
    echo
    echo "Building documentation for library ${lib}..."
    cd ${repo_dir}/${lib}
    git pull --rebase origin master
    git submodule update --recursive --init
    cd ${repo_dir}/${lib}/documentation
    if [[ "${lib}" == "testworks" ]]; then
	cd users-guide
    fi
    make html && rsync -avz build/html/ ${dest_dir}/documentation/${lib}/
    make epub && cp build/epub/*.epub ${dest_dir}/documentation/${lib}/
    # Disable PDF for now.
    # make latexpdf && cp build/latex/*.pdf ${dest_dir}/documentation/${lib}/
done
