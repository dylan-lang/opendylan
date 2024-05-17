#!/bin/bash

# This script builds the opendylan.org website. It does a clean build of the
# docs each time and copies files to ${dest_dir}, meaning that there could be
# cruft left around in ${dest_dir}. Might want to fix that some day.

# Three directories in ${dest_dir} are special:
#   1. /downloads   # copy files there by hand
#   2. /books/drm   # the DRM is .html files, copied here separately
#   3. /package     # package docs are generated here

echo ""

if [[ $# -ne 1 ]]; then
    echo "Usage: `basename $0` dest-dir"
    echo
    echo "  DEST-DIR: Directory into which the website content should be copied."
    echo
    echo "  Example: update.sh /var/www/opendylan.org"
    exit 2
fi

dest_dir=$(realpath "$1")

if [[ ! -d "$dest_dir" ]]; then
    echo "${dest_dir} doesn't exist; aborting."
    exit 2
fi

set -e   # die on any error

opendylan_dir=$(dirname $(dirname $(realpath "$0")))

# Get latest gendoc, DRM, etc, as specified in dylan-package.json.
echo "Updating the Dylan workspace to get latest package dependencies..."
cd "${opendylan_dir}/documentation"
dylan update
dylan build gendoc


# Build and install main docs

echo "Building Open Dylan docs in ${opendylan_dir}/documentation/_build/html/ ..."
make --directory ${opendylan_dir}/documentation html

echo "Copying main docs to ${dest_dir} ..."
rsync -avz ${opendylan_dir}/documentation/_build/html/ ${dest_dir}


# Build and install package docs.
#
# Copy the gendoc package first since we're about to modify its docs/source dir.

work_dir=$(mktemp -d)
cp -rp ${opendylan_dir}/_packages/gendoc/current/src ${work_dir}/gendoc
cd ${work_dir}/gendoc
dylan update        # Install sphinx-extensions in the right relative location.

package_dir="${work_dir}/gendoc/docs/source/"
echo "Generating package docs in ${package_dir}..."
${opendylan_dir}/_build/bin/gendoc ${package_dir}/index.rst
make --directory ${work_dir}/gendoc/docs html

echo "Copying package docs to ${dest_dir}/package ..."
rsync -avz ${work_dir}/gendoc/docs/_build/html/ ${dest_dir}/package


# Install the DRM docs.
#
# Note that the DRM docs depend on rewrite rules setup in nginx config in order
# to redirect Foo to Foo.html or Foo.png etc.

echo "Copying DRM docs to ${dest_dir}/books/drm ..."
mkdir -p ${dest_dir}/books/drm
rsync -avz ${opendylan_dir}/_packages/dylan-reference-manual/current/src/source/  ${dest_dir}/books/drm
