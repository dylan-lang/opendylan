Welcome to Open Dylan!

Open Dylan is the open source releas of Functional Developer.

This file is intended to give you a basic hint as to how to start
hacking on Open Dylan and what is and is not included in the Open
Source version.


WHAT WAS OPEN SOURCED

Everything except for the files necessary to build Windows installers
has been Open Sourced.  Functional Objects plans to continue to build
pre-packaged versions of Functional Developer for the Windows platform
and make them available for a small fee to anyone who prefers not to
build it themselves.

Open Dylan uses the Memory Pool System (MPS) from Ravenbrook, Limited
to do its memory management.  The MPS is available from Ravenbrook at
http://www.ravenbrook.com/project/mps/ and must be downloaded and
built separately.


HOW TO BUILD ON WINDOWS

First build the garbage collector.  See the section on BUILDING THE
MPS.

The main build script for Open Dylan on Windows is
fundev/admin/builds/build-release.bat.  See that file for additional
notes and a list of prerequisites.  Notably, to do the initial build
you will need Open Dylan installed.  (This should be available from
www.opendylan.org by the time you read this.)  You will also need VC++
6.0.

[TODO: verify details here]



HOW TO BUILD ON LINUX

Install the latest binary release of Open Dylan from
http://www.opendylan.org/downloading.phtml

svn co svn://anonsvn.gwydiondylan.org/scm/svn/dylan/trunk/fundev fundev
export SRCDIR=`pwd`/fundev        # must be absolute path!
export BUILDDIR=<your build dir>

Build the MPS and copy it into the Open Dylan source tree (see below).

cd $SRCDIR
./autogen.sh

cd $BUILDDIR
$SRCDIR/configure --with-mps=/path/to/mps-header-files
              # you must call configure with absolute path!
make

The goal was ultimately to make it possible to build Open Dylan in the
Open Dylan IDE, but unfortunately we never quite made that happen.
Some work may need to be done on the "project manager" first, at a
minimum.

If you encounter problems during the build, please refer to the log
files that are written in the 'logs' directory under each bootstrap
stage build directory.



BUILDING THE MPS

The Open Dylan garbage collector is the Memory Pool System (MPS), from
Ravenbrook, Ltd.  Download version 1.100.1 (or greater) of the MPS
from Ravenbrook at http://www.ravenbrook.com/project/mps/ and extract
it to some directory.  cd to the 'code' subdirectory in the MPS
sources and build the mmdw.lib target.

  Windows:  nmake /k /f w3i3mv.nmk mmdw.lib
	    copy *.h+w3i3mv\ci\mmdw.lib %FUNDEV%\sources\lib\run-time\pentium-win32

  Linux:    make -f lii4gc.gmk mmdw.a mpsplan.a

The actual makefile you use may differ depending on your platform.
The main point to notice here is that you don't just build the default
target, as described in the MPS documentation.  You must build
mmdw.lib or mmdw.a instead.

Copy mmdw.lib into the Open Dylan source tree in
fundev/sources/lib/run-time/pentium-win32/ respectively.  This will be
picked up by the Open Dylan build scripts.

There are two caveats here.  Number one: the above instructions make
you use the ci, "cool internal" variant of the MPS on Windows. On
Linux, MPS_VARIANT=hi is specified in
fundev/sources/lib/run-time/pentium-linux/Makefile.in The ci variant
does quite a lot of sanity checks all over, which heavily impacts
performance, up to a factor of three in total application runtime.  If
you're looking for performance figures, use the hi, "hot internal", or
even wi, "white-hot internal" releases.

Number two: glibc >=2.3 and linux kernel >= 2.6 required
