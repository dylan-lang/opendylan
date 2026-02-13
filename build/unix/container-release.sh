#!/bin/sh
set -e

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.
top_srcdir=$(realpath $srcdir/../..)

dylandir=$(realpath $(dirname $(which dylan-compiler))/..)

#CONTAINERTOOL=docker
CONTAINERTOOL=podman

$CONTAINERTOOL build -t opendylan.builder -f $srcdir/Containerfile

$CONTAINERTOOL run --rm -it \
               --volume $dylandir:/opt/opendylan \
               --volume $top_srcdir:$top_srcdir \
               --volume $(pwd):$(pwd) --workdir $(pwd) \
               opendylan.builder $srcdir/release-with-batteries.sh "$@"
