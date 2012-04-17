#!/bin/sh

OD_VERSION=`grep 'define constant \$release-version' sources/lib/release-info/common-info.dylan | cut -d '"' -f 2`
OD_ARCH=x86
OD_SYSTEM=`uname -s | tr A-Z a-z`
OD_SOURCES=`pwd`

OD_PLATFORM=$OD_ARCH-$OD_SYSTEM

cd $OD_SOURCES
./configure --prefix=/opt/opendylan-$OD_VERSION
make 3-stage-bootstrap
sudo make install

cd /opt/
chown -R root opendylan-$OD_VERSION
chgrp -R root opendylan-$OD_VERSION
tar cjf opendylan-$OD_VERSION-$OD_PLATFORM-debug-info.tar.bz2 opendylan-$OD_VERSION/build
cd opendylan-$OD_VERSION
rm -rf profiles build
cp $OD_SOURCES/License.txt .
cp $OD_SOURCES/packages/unix/README .
if [ -f $OD_SOURCES/packages/unix/README-$OD_SYSTEM.diff ]; then
  patch < $OD_SOURCES/packages/unix/README-$OD_SYSTEM.diff # (only on FreeBSD and MacOSX)
fi
cd ..
tar cjf opendylan-$OD_VERSION-$OD_PLATFORM.tar.bz2 opendylan-$OD_VERSION