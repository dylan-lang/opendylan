#!/bin/bash

# This script builds the opendylan.org website. It does a clean build of the
# docs each time and copies files to ${dest_dir}, meaning that there could be
# cruft left around in ${dest_dir}. Might want to fix that some day.

# /var/www/opendylan.org/downloads is handled specially. New versions of Open
# Dylan should be copied there by hand, or ... we could stop doing that.

echo ""

if [[ $# -ne 2 ]]; then
    echo "Usage: `basename $0` index-file dest"
    echo "  INDEX-FILE: Pathname to the documentation/source/package/index.rst file"
    echo "    in the opendylan repository checkout.  This file will be modified to"
    echo "    contain the package docs."
    echo "  DEST: Directory into which the website content should be copied."
    echo "    Example: /var/www/opendylan.org"
    exit 2
fi

index_file="$1"
dest_dir="$2"

if [[ ! -r "${index_file}" ]]; then
    echo "${index_file} doesn't exist; aborting."
    exit 2
fi

if [[ ! -d "$dest_dir" ]]; then
    echo "${dest_dir} doesn't exist, aborting."
    exit 2
fi

set -e   # die on any error

opendylan_dir=$(realpath "${index_file}/../../../..")
gendoc_exe="${opendylan_dir}/_build/bin/gendoc"

# Get latest gendoc, DRM, etc, as specified in dylan-package.json.
echo "Updating the Dylan workspace to get latest package dependencies..."
cd "${opendylan_dir}/documentation"
dylan update
dylan build gendoc

# Doc for each package is generated in ${package_dir}/<package-name>
package_dir=${opendylan_dir}/documentation/source/package
echo "Generating package docs in ${package_dir}..."

for file in ${package_dir}/*; do
    if [[ "$(basename "$file")" != "index.rst" ]]; then
        echo "rm -rf ${file}"
        rm -rf "$file"
    fi
done
${gendoc_exe} ${index_file}

echo "Building opendylan.org ..."
make --directory ${opendylan_dir}/documentation html
rsync -avz ${opendylan_dir}/documentation/_build/html/ ${dest_dir}

echo "Updating DRM files..."
mkdir -p ${dest_dir}/books/drm
rsync -avz ${opendylan_dir}/_packages/dylan-reference-manual/current/src/source/  ${dest_dir}/books/drm
