#!/bin/sh
# Run this to generate all the initial makefiles, etc.

srcdir=`dirname $0`
test -z "$srcdir" && srcdir=.

DIE=false

LIBTOOL=libtool
LIBTOOLIZE=libtoolize

if test `uname` = "Darwin";
then LIBTOOL=glibtool;
LIBTOOLIZE=glibtoolize
fi

(autoconf --version && autoheader --version) < /dev/null > /dev/null 2>&1 || {
    echo
    echo "You must have autoconf installed to compile Functional Developer."
    echo "Download the appropriate package for your distribution,"
    echo "or get the source tarball at ftp://ftp.gnu.org/pub/gnu/autoconf"
    DIE=true
}

(aclocal --version) < /dev/null > /dev/null 2>&1 || {
    echo
    echo "You must have automake installed to compile Functional Developer."
    echo "Download the appropriate package for your distribution,"
    echo "or get the source tarball at ftp://ftp.gnu.org/pub/gnu/automake"
    DIE=true
}

($LIBTOOLIZE --version) < /dev/null > /dev/null 2>&1 || {
    echo
    echo "You must have libtool installed to compile Functional Developer."
    echo "Download the appropriate package for your distribution,"
    echo "or get the source tarball at ftp://ftp.gnu.org/pub/gnu/libtool"
    DIE=true
}

$DIE && exit 1

echo "processing (ignore errors and warnings from automake and libtoolize)..."

( cd $srcdir
  libtoolize --force --copy
  automake --foreign --add-missing
  autoconf )
