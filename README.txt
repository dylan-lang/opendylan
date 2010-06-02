Welcome to Open Dylan!

Open Dylan is the open source release of Functional Developer.

This file is intended to give you a basic hint as to how to start
hacking on Open Dylan and what is and is not included in the Open
Source version.



WHAT WAS OPEN SOURCED

Everything except for the files necessary to build Windows installers
has been Open Sourced.

Open Dylan uses the Memory Pool System (MPS) from Ravenbrook, Limited
to do its memory management.  The MPS is available from Ravenbrook at
http://www.ravenbrook.com/project/mps/ and must be downloaded and
built separately.



HOW TO BUILD ON LINUX

Install the latest binary release of Open Dylan from
http://www.opendylan.org/downloading.phtml

  svn co svn://anonsvn.gwydiondylan.org/scm/svn/dylan/trunk/fundev fundev
  export SRCDIR=`pwd`/fundev        # must be absolute path!
  export BUILDDIR=<your build dir>

Build the MPS (see below).

  cd $SRCDIR
  ./autogen.sh

  cd $BUILDDIR
  $SRCDIR/configure --with-mps=/path/to/mps-kit                   [1][2]
              # you must call configure with absolute path!
  make

Your new Dylan compiler will be in
$BUILDDIR/Bootstrap.2/bin/minimal-console-compiler


[1] Note that the directory --with-mps wants here is the one containing the
    "code" directory.  If you downloaded the MPS kit from Ravenbrook this
    would be, for example, "--with-mps=/path/to/mps-kit-1.106.2".

[2] Ubuntu note: In Ubuntu 6.06 I had to install the XML::Parser module via
    "cpan -i XML::Parser", which blew up because expat.h was not found, so
    first "sudo apt-get install libexpat-dev".  --cgay 20061104



HOW TO BUILD ON WINDOWS

First build the garbage collector.  See the section on BUILDING THE
MPS.

The main build script for Open Dylan on Windows is
fundev/admin/builds/build-release.bat.  See that file for additional
notes and a list of prerequisites.  Notably, to do the initial build
you will need Open Dylan installed.  You will also need VC++ 6.0.

[TODO: verify details here and fill this out]



HOW TO BUILD ON DARWIN OR MAC OS X

Install the latest binary release of Open Dylan from
http://www.opendylan.org/downloads/opendylan. Ensure the release's bin
directory is in your path.

Install the expat library, followed by the XML::Parser Perl module. For the
expat library, you can use a package manager like MacPorts or Fink. For the
XML::Parser module, use CPAN:

  sudo cpan XML::Parser

Install the Boehm garbage collection library. On the Mac, Open Dylan uses that
instead of the MPS library. You can use a package manager or download, make,
and install it directly. I have used the following configure settings:

  ./configure --enable-parallel-mark --enable-threads=posix
    --enable-large-config --enable-gc-debug USE_I686_PREFETCH=1

Download the Open Dylan source tree, containing this file and the "sources"
directory, from SVN as described on http://www.opendylan.org/repository.phtml.

In the directory of the downloaded source tree, run these commands (set the
prefix to whatever you want, and set the correct path to the Boehm GC
libraries):

  ./autogen.sh
  ./configure --prefix=/usr/local/opendylan --with-gc=/usr/local
  make
  sudo make install

Ignore any errors printed by autogen.sh. The build process may freeze; if
several minutes pass without any progress, cancel the build with ^C and re-run
the last command. After installation is complete, don't forget to remove the
binary release's bin directory from your path.

To avoid installing the release in the prefix location, replace the "sudo make
install" command with "make bootstrap-stage-3". The final compiler will be put
in the Bootstrap.3 directory in your source tree.

BUILDING FOR 64 BITS

You can build Open Dylan for 64 bits, but it is not functional yet. The 64-bit
compiler will crash when run. That said, to build the 64-bit compiler, ensure
you have installed a compatible Boehm garbage collection library, include the
--with-arch option in the configure command, and make the 2-stage-bootstrap or
3-stage-bootstrap target:

  ./autogen.sh
  ./configure --prefix=/usr/local/opendylan --with-gc=/usr/local --with-arch=x86_64
  make 3-stage-bootstrap

In addition, you will need to include the -arch x86_64 option whenever you build
a Dylan library:

  minimal-console-compiler -build -arch x86_64 my-project.lid



GENERAL BUILD INSTRUCTIONS

If you encounter problems during the build, please refer to the log
files that are written in the 'logs' directory under each bootstrap
stage build directory.

Note that when building Open Dylan 1.0 beta 5 or later using the 1.0 beta 4
binary release, you must NOT make the 2-stage-bootstrap or 3-stage-bootstrap
targets. They will fail with a "bogus float" or some other error. Make the
default target as described above (the 1-stage-bootstrap target).



BUILDING THE MPS

The Open Dylan garbage collector is the Memory Pool System (MPS), from
Ravenbrook, Ltd.  Download version 1.100.1 (or greater) of the MPS
from Ravenbrook at http://www.ravenbrook.com/project/mps/ and extract
it to some directory.  cd to the 'code' subdirectory in the MPS
sources and build the mmdw target:

Windows:

  nmake /k /f w3i3mv.nmk mmdw.lib
  copy *.h+w3i3mv\ci\mmdw.lib %FUNDEV%\sources\lib\run-time\pentium-win32

Linux:

  make -f lii4gc.gmk mmdw.a mpsplan.a

  The actual makefile you use may differ depending on your platform.
  See the readme.txt file in the MPS distribution for a list.

  The build products will be picked up by
  fundev/sources/lib/run-time/pentium-linux/Makefile.in
  assuming you pass the appropriate value for --with-mps.

  glibc >=2.3 and linux kernel >= 2.6 required

  For MPS 1.106.2 I (cgay) encountered this problem on Ubuntu 7.10:
  http://www.ravenbrook.com/project/mps/issue/job001637/ The solution
  was to replace

     #define _POSIX_C_SOURCE 1

  with

    #define _XOPEN_SOURCE 500

  in both prmcli.h and pthrdext.c.  The comments in the above URL
  weren't explicit about that.

The main point to notice here is that you don't just build the default
target, as described in the MPS documentation.  You must build
mmdw.lib or mmdw.a instead.

The above instructions use the ci, "cool internal" variant of the MPS
on Windows. On Linux, MPS_VARIANT=hi is specified in
fundev/sources/lib/run-time/pentium-linux/Makefile.in.  The ci variant
does quite a lot of sanity checks all over, which heavily impacts
performance, up to a factor of three in total application runtime.  If
you're looking for performance figures, use the hi, "hot internal", or
even wi, "white-hot internal" releases.

