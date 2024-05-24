#!/bin/bash

# This script builds the opendylan.org website. It does a clean build of the
# docs each time and copies files to ${dest_dir}, meaning that there could be
# cruft left around in ${dest_dir}. Might want to fix that some day.

# Three directories in ${dest_dir} are special:
#   1. /downloads   # copy files there by hand
#   2. /books/drm   # the DRM is .html files, copied here separately

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
set -x   # debug output

opendylan_dir=$(dirname $(dirname $(realpath "$0")))

# Get latest gendoc, DRM, etc, as specified in dylan-package.json.
echo "Updating the Dylan workspace to get latest package dependencies..."
cd "${opendylan_dir}/documentation"
dylan update

# Build and install main docs

echo "Building Open Dylan docs in ${opendylan_dir}/documentation/_build/html/ ..."
make --directory ${opendylan_dir}/documentation html

echo "Copying main docs to ${dest_dir} ..."
rsync -a ${opendylan_dir}/documentation/_build/html/ ${dest_dir}


# Install the DRM docs.
#
# Note that the DRM docs depend on rewrite rules setup in nginx config in order
# to redirect Foo to Foo.html or Foo.png etc., so testing is difficult. But it
# also rarely changes so is unlikely to break.

echo "Copying DRM docs to ${dest_dir}/books/drm ..."
mkdir -p ${dest_dir}/books/drm
rsync -a ${opendylan_dir}/_packages/dylan-reference-manual/current/src/source/  ${dest_dir}/books/drm
