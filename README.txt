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

Build the MPS and copy it into the fundev source tree (see below).

cd $SRCDIR
./autogen.sh

sudo cp Sources/lib/run-time/pentium-linux/dylan-elf-*.script /usr/local/lib/functional-developer/lib

cd $BUILDDIR
$SRCDIR/configure            # you must call configure with absolute path!
make

The goal was ultimately to make it possible to build FunDev in the FunDev IDE,
but unfortunately we never quite made that happen.  Some work may need to be
done on the "project manager" first, at a minimum.

If you encounter problems during the build, please refer to the log files
that are written in the 'logs' directory under each bootstrap stage build
directory.



BUILDING THE MPS

The FunDev garbage collector is the Memory Pool System (MPS), from Ravenbrook,
Ltd.  Download version 1.100.1 (or greater) of the MPS from Ravenbrook at
http://www.ravenbrook.com/project/mps/ and extract it to some directory.  cd to
the 'code' subdirectory in the MPS sources and build the mmdw.lib target.

  Windows:  nmake /k /f w3i3mv.nmk mmdw.lib
            copy *.h w3i3mv/ci/mmdw.lib %FUNDEV%/Sources/lib/run-time/pentium-win32/
  Linux:    make -f lii4gc.gmk mmdw.a mpsplan.a
            cp *.h lii4gc/ci/mmdw.a lii4gc/ci/mpsplan.a $FUNDEV/Sources/lib/run-time/pentium-linux/
            [if you don't have a lii4gc/ci directory, choose a different ?i directory.]

The actual makefile you use may differ depending on your platform.
The main point to notice here is that you don't just build the default
target, as described in the MPS documentation.  You must build
mmdw.lib or mmdw.a instead.

Copy mmdw.lib or mmdw.a and mpsplan.a into the FunDev source tree in
fundev/Sources/lib/run-time/pentium-win32/ or pentium-linux,
respectively.  This will be picked up by the FunDev build scripts.

There are to caveats here.  Number one: the above instructions make
you use the ci, "cool internal" variant of the MPS.  This one does
quite a lot of sanity checks all over, which heavily impacts
performance, up to a factor of three in total application runtime.  If
you're looking for performance figures, use the hi, "hot internal", or
even wi, "white-hot internal" releases.

Caveat number two is that the Linux port is still a little weak.  In
particular, there are problems with NPTL, the new generation POSIX
thread support.  If you see any crashes when trying to use threads
(try app/gctest to make sure), you can force a switch to the old
threading model with

  export LD_ASSUME_KERNEL=2.4.1

