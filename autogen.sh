#!/bin/sh
# Run this to generate all the initial makefiles, etc.

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

DIE=false

(autoconf --version && autoheader --version) < /dev/null > /dev/null 2>&1 || {
    echo
    echo "You must have autoconf installed to compile Open Dylan."
    echo "Download the appropriate package for your distribution,"
    echo "or get the source tarball at ftp://ftp.gnu.org/pub/gnu/autoconf"
    DIE=true
}

(aclocal --version) < /dev/null > /dev/null 2>&1 || {
    echo
    echo "You must have automake installed to compile Open Dylan."
    echo "Download the appropriate package for your distribution,"
    echo "or get the source tarball at ftp://ftp.gnu.org/pub/gnu/automake"
    DIE=true
}

$DIE && exit 1

echo "*** Hello, there! ***"
echo "*** Please ignore errors and warnings from automake. ***"
echo "*** Thanks! ***"

( cd $srcdir
  mkdir -p build-aux
  aclocal
  automake --foreign --add-missing
  autoconf )
