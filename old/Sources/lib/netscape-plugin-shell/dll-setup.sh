#!/bin/sh -f

if [ $# -ne 2 ]; then
  echo "usage dll-setup.sh destination-dir target-name"
  exit 1
fi

dir=$1
target=$2

cp netscape-plugin.mak $dir/$target.mak
chmod ug+w $dir/$target.mak
echo  "TARGET = $target" > $dir/target.mak
chmod ug+w $dir/target.mak
cp npwin.cpp $dir/npwin.cpp
chmod ug+w $dir/npwin.cpp
if [ \! \( -f $dir/$target.rc \) ]; then
  cp plugin.rc $dir/$target.rc
  chmod ug+w $dir/$target.rc
  echo "Don't forget to update $dir/$target.rc with your resources."
fi
cp plugin.rc2 $dir
chmod ug+w $dir/plugin.rc2
cp resource.h $dir
chmod ug+w $dir/resource.h

cat > $dir/$target.def <<SCRIPT
LIBRARY   $target

CODE	  PRELOAD MOVEABLE DISCARDABLE
DATA	  PRELOAD SINGLE

EXPORTS
	NP_GetEntryPoints   @1
	NP_Initialize       @2
	NP_Shutdown         @3
SCRIPT
chmod ug+w $dir/$target.def
cp wsock.c wsock.h $dir
chmod ug+w $dir/wsock.c $dir/wsock.h
if [ -f $dir/libs.lnk ]; then
  cp fix-libs.sh $dir
  ( cd $dir; ./fix-libs.sh )
else
  echo "Don't forget to run fix-libs.sh in $dir after compiling $target to create libs.mak"
fi
if [ \! \( -d $dir/objs \) ]; then
  mkdir $dir/objs
fi
cp -r include $dir
(cd $dir/include; chmod ug+w `ls -a`)
