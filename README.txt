Welcome to the Functional Developer open sources!

This file is intended to give you a basic hint as to how to start hacking on
Functional Developer and what is and is not included in the Open Source
version.


WHAT WAS OPEN SOURCED

Everything except for the files necessary to build Windows installers has been
Open Sourced.  Functional Objects plans to continue to build pre-packaged
versions of Functional Developer for the Windows platform and make them
available for a small fee to anyone who prefers not to build it themselves.

Functional Developer uses the Memory Pool System (MPS) from Ravenbrook, Limited
to do its memory management.  The MPS is available from Ravenbrook at
http://www.ravenbrook.com/project/mps/ and must be downloaded and built
separately.


HOW TO BUILD ON WINDOWS

First build the garbage collector.  See the section on BUILDING THE MPS.

The main build script for FunDev on Windows is
fundev/admin/builds/build-release.bat.  See that file for additional
notes and a list of prerequisites.  Notably, to do the initial build
you will need the "hacker" edition of Functional Developer installed.
(This should be available from www.functionalobjects.com by the time
you read this.)  You will also need VC++ 6.0.

[TODO: verify details here]



HOW TO BUILD ON LINUX

--- 2004.03.18 ---
These directions are likely to change soon, but here's how to build as
of now:

Install the FD Linux Alpha and disable its expiration date with this
command:

echo "001ce9c0: 00 00 00 00" | sudo xxd -r - /usr/local/lib/functional-developer/lib/libdylan.so

cvs co fundev
export SRCDIR=`pwd`/fundev        # must be absolute path!
export BUILDDIR=<your build dir>
cd $SRCDIR
autoconf
automake -a
libtoolize -c
cd $BUILDDIR
$SRCDIR/configure
make

The goal was ultimately to make it possible to build FunDev in the FunDev IDE,
but unfortunately we never quite made that happen.  Some work may need to be
done on the "project manager" first, at a minimum.


BUILDING THE MPS

The FunDev garbage collector is the Memory Pool System (MPS), from Ravenbrook,
Ltd.  Download the MPS from Ravenbrook at
http://www.ravenbrook.com/project/mps/ and extract it to some directory.  cd to
the 'code' subdirectory in the MPS sources and build the mmdw.lib target.

  Windows:  nmake /k /f w3i3mv.nmk mmdw.lib
  Linux:    make -f lii4gc.gmk mmdw.a

The actual makefile you use may differ depending on your platform.  The main
point to notice here is that you don't just build the default target, as
described in the MPS documentation.  You must build mmdw.lib instead.

Copy mmdw.lib [TODO: which version?] into the FunDev source tree in
fundev/Sources/lib/run-time/pentium-win32/.  This will be picked up by the
FunDev build scripts.

  [TODO: verify that this is sufficient.]

