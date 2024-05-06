#!/bin/bash -xe

# This script builds the opendylan.org website. It does a clean build of the
# docs each time and copies files to ${dest_dir}, meaning that there could be
# cruft left around in ${dest_dir}. Might want to fix that some day.

# /var/www/opendylan.org/downloads is handled specially. New versions of Open
# Dylan should be copied there by hand, or ... we could stop doing that.

# Note the -e in the #! line above causes this script to abort on any command
# failure.

if [[ $# -ne 1 ]]; then
    echo "Usage: `basename $0` dest_dir"
    echo "  dest_dir:    Directory in which to place the built website documents."
    echo "               Example: /var/www/opendylan.org"
    exit 2
fi

# Get latest gendoc, DRM, DPG, etc, as specified in dylan-package.json.
echo "Updating the Dylan workspace to get latest packages..."
dylan update
dylan build gendoc

website_dir=$(realpath $(dirname "$0"))

dest_dir=$(realpath "$1")
if [[ ! -d "$dest_dir" ]]; then
    echo "dest_dir ${dest_dir} doesn't exist, aborting."
    exit 2
fi

gendoc_exe=${website_dir}/_build/bin/gendoc

# Packages are downloaded here.
gendoc_work_dir=$(mktemp -d gendoc-XXXX)

# Doc for each package is generated in
# ${gendoc_output_dir}/package/<package-name>
gendoc_output_dir=${website_dir}/source
echo "Generating package docs in ${gendoc_output_dir}..."

rm -rf ${gendoc_output_dir}/package
${gendoc_exe} -o ${gendoc_output_dir}

echo "Building website..."
make html
rsync -avz build/html/ ${dest_dir}

echo "Updating DRM files..."
mkdir -p ${dest_dir}/books/drm
rsync -avz _packages/dylan-reference-manual/current/src/source/  ${dest_dir}/books/drm

echo "Updating DPG files..."
mkdir -p ${dest_dir}/books/dpg
rsync -avz _packages/dylan-programming-book/current/src/source/  ${dest_dir}/books/dpg
