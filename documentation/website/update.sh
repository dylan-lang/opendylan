#!/bin/bash -xe

# This script builds the opendylan.org website. A summary of what this script
# actually does:
#
# $ git clone --recursive https://github.com/dylan-lang/opendylan
# $ git clone --recursive https://github.com/dylan-lang/gendoc
# $ cd gendoc
# $ make docs
# $ cp -rp output/package ../opendylan/documentation/website/source/
# $ cd ../opendylan/documentation/website
# $ make html
# $ cp -rp build/html/* /var/www/opendylan.org

# /var/www/opendylan.org/downloads is handled specially. New versions of Open
# Dylan must be copied there by hand.

# TODO:
# * Fix latexpdf vs python3 issues. Disabled PDF for now.
# * Link to GitHub releases for the download pages instead of maintaining
#   downloads by hand.
# * Build/install dylan-programming-book into ${dest_dir}/books/dpg.
# * Build/install dylan-reference-manual into ${dest_dir}/books/drm.

if [[ $# -ne 3 ]]; then
    echo "Usage: `basename $0` dest_dir opendylan_dir gendoc_exe"
    echo "  dest_dir:      Directory in which to place the built website documents."
    echo "                 Example: /var/www/opendylan.org"
    echo "  opendylan_dir: The opendylan repo directory."
    echo "  gendoc_exe:    Path to the gendoc executable."
    exit 2
fi

dest_dir=$(realpath "$1")
opendylan_dir=$(realpath "$2")
gendoc_exe=$(realpath "$3")

# Generate package docs
gendoc_work_dir=$(mktemp -d gendoc-XXXX) # git clones go here
gendoc_output_dir=${opendylan_dir}/documentation/website/source
cd ${gendoc_work_dir}
rm -rf ${package_doc_dir}
${gendoc_exe} -o ${gendoc_output_dir}

# Build website
cd ${opendylan_dir}/documentation/website
git pull --rebase origin master
git submodule update --recursive --init
make clean
make html

# Deploy
rsync -avz build/html/ ${dest_dir}
