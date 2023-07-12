#!/bin/bash -xe

# This script builds the opendylan.org website. It does a clean build
# each time and copies files to ${dest_dir}, meaning that there could
# be cruft left around in ${dest_dir}. Might want to fix that some day.

# /var/www/opendylan.org/downloads is handled specially. New versions of Open
# Dylan should be copied there by hand, or ... we could stop doing that.

if [[ $# -ne 3 ]]; then
    echo "Usage: `basename $0` dest_dir repo_dir gendoc_exe"
    echo "  dest_dir:    Directory in which to place the built website documents."
    echo "               Example: /var/www/opendylan.org"
    echo "  repo_dir:    Directory in which opendylan, drm, and dpg repos live."
    echo "  gendoc_exe:  Path to the gendoc executable."
    exit 2
fi

dest_dir=$(realpath "$1")
repo_dir=$(realpath "$2")
gendoc_exe=$(realpath "$3")

# Not sure if necessary yet...
#mkdir -p ${dest_dir}/_plantuml

echo "Generating package docs..."
gendoc_work_dir=$(mktemp -d gendoc-XXXX) # git clones go here
gendoc_output_dir=${repo_dir}/opendylan/documentation/website/source
cd ${gendoc_work_dir}
rm -rf ${gendoc_output_dir}/package
${gendoc_exe} -o ${gendoc_output_dir}

echo "Building website..."
cd ${repo_dir}/opendylan/documentation/website
make clean
make html
rsync -avz build/html/ ${dest_dir}

echo "Updating DRM files..."
cd ${repo_dir}/dylan-reference-manual
git pull --rebase origin master
rsync -avz source/  /var/www/opendylan.org/books/drm
