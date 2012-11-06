AC_INIT([Open Dylan], [2012.1])
AC_PREREQ(2.50)

# Directory for config.guess etc.
AC_CONFIG_AUX_DIR(build-aux)

#---------------------
# Make srcdir absolute
#---------------------

srcdir=`(cd $srcdir; pwd)`

SUPPORT_GC_MPS=no
SUPPORT_GC_BOEHM=yes

AC_CANONICAL_TARGET
case $target in
  i*86-*-linux*)
      OPEN_DYLAN_PLATFORM_NAME=x86-linux;
      RUNTIME_SOURCES=sources/lib/run-time;
      SUPPORT_GC_MPS=yes;
      SUPPORT_GC_BOEHM=no
    ;;
  i*86-*-freebsd*)
      OPEN_DYLAN_PLATFORM_NAME=x86-freebsd;
      RUNTIME_SOURCES=sources/lib/run-time;
      SUPPORT_GC_MPS=yes;
      SUPPORT_GC_BOEHM=no
    ;;
  i*86-*-darwin*)
      OPEN_DYLAN_PLATFORM_NAME=x86-darwin;
      RUNTIME_SOURCES=sources/dfmc/c-run-time
    ;;
  x86_64-*-darwin*)
      OPEN_DYLAN_PLATFORM_NAME=x86-darwin;
      RUNTIME_SOURCES=sources/dfmc/c-run-time
    ;;
  powerpc-*-darwin*)
      OPEN_DYLAN_PLATFORM_NAME=ppc-darwin;
      RUNTIME_SOURCES=sources/dfmc/c-run-time
    ;;
  amd64-*-freebsd*)
      OPEN_DYLAN_PLATFORM_NAME=amd64-freebsd;
      RUNTIME_SOURCES=sources/dfmc/c-run-time
    ;;
  x86_64-*-linux*)
      OPEN_DYLAN_PLATFORM_NAME=x86_64-linux;
      RUNTIME_SOURCES=sources/dfmc/c-run-time
    ;;
  *)
      AC_MSG_ERROR([The $target platform is not supported.])
    ;;
esac
AC_SUBST(OPEN_DYLAN_PLATFORM_NAME)
AC_SUBST(RUNTIME_SOURCES)

AM_INIT_AUTOMAKE

AC_PROG_INSTALL
AC_PROG_LN_S

AC_PATH_PROG(PERL, perl, no)
test "$PERL" = no && AC_MSG_ERROR([
perl is required to build Open Dylan])
AC_SUBST(PERL)

PERL_MODULES="File::Spec Getopt::Long"
for module in $PERL_MODULES; do
  AC_MSG_CHECKING(for Perl module $module)
  $PERL -M$module -e 'exit 0' || AC_MSG_ERROR([
$module is required to build Open Dylan])
  AC_MSG_RESULT(yes)
done

AC_CHECK_PROGS(DYLANCOMPILER, dylan-compiler opendylan minimal-console-compiler, no)
test "$DYLANCOMPILER" = no && AC_MSG_ERROR([
dylan-compiler, opendylan or minimal-console-compiler is required to bootstrap Open Dylan. Please download and install a build from http://opendylan.org/download/index.html])
test "$DYLANCOMPILER" = dylan-compiler && DYLANCOMPILER="$DYLANCOMPILER -build"
test "$DYLANCOMPILER" = opendylan && DYLANCOMPILER="$DYLANCOMPILER -build"
test "$DYLANCOMPILER" = minimal-console-compiler && DYLANCOMPILER="$DYLANCOMPILER -build"
AC_SUBST(DYLANCOMPILER)

# TODO: figure out the bootstrap type given the fdcompile version
bootstrap_target=1-stage-bootstrap
AC_SUBST(bootstrap_target)

AC_PROG_CC([clang gcc cc])

AC_CHECK_PROGS([MAKE],[gnumake gmake make],[AC_MSG_ERROR([Unable to find a make program.])])
if ! $MAKE --version | grep -q "GNU" 2> /dev/null; then
  AC_MSG_ERROR([OpenDylan requires GNU make to build.])
fi

AC_ARG_WITH(mps,
            AS_HELP_STRING([--with-mps=PATH],
                           [Path to MPS distribution and build.]))

AC_ARG_WITH(gc,
            AS_HELP_STRING([--with-gc=PATH],
                           [Path of Boehm GC installation.]))

AS_IF([test x$with_mps != x],
      [
        AS_IF([test x${SUPPORT_GC_MPS} == xno],
          [
            AC_MSG_ERROR([The platform that you are building for requires the Boehm GC.])
          ]
        )
        GC_CFLAGS="-DGC_USE_MPS -I${with_mps}/code"
        GC_CHOICE="mps"
        old_CPPFLAGS=$CPPFLAGS
        CPPFLAGS="-I${with_mps}/code"
        AC_CHECK_HEADER([mps.h],[],[AC_MSG_ERROR([Incorrect path to MPS specified. Please make sure you specified an absolute path to the root MPS directory.])])
        CPPFLAGS=$old_CPPFLAGS
        # This is to help catch people who haven't updated to the new MPS yet.
        AC_CHECK_FILE([${with_mps}/code/lii4gc.gmk],[AC_MSG_ERROR([You have an old version of MPS and need to update.])],[])
      ]
)
AS_IF([test x$with_gc != x],
      [
        AS_IF([test x${SUPPORT_GC_BOEHM} == xno],
          [
            AC_MSG_ERROR([The platform that you are building for requires the MPS GC.])
          ]
        )
        GC_CFLAGS="-DGC_USE_BOEHM -DGC_THREADS -I${with_gc}/include"
        GC_LFLAGS="-L${with_gc}/lib -lgc"
        GC_CHOICE="boehm"
      ]
)
AS_IF([test x${with_gc}x${with_mps} == xx],
      [
        AS_IF([test x${SUPPORT_GC_BOEHM} == xno],
          [
            AC_MSG_ERROR([The platform that you are building for requires the MPS GC.])
          ]
        )
        GC_CFLAGS="-DGC_USE_BOEHM -DGC_THREADS"
        GC_LFLAGS="-lgc"
        GC_CHOICE="boehm"
      ]
)
AC_SUBST(GC_CFLAGS)
AC_SUBST(GC_LFLAGS)
AC_SUBST(GC_CHOICE)

# for now, replace mps only in pentium-linux
# gc is also replaced in sources/dfmc/c-run-time
AC_CONFIG_FILES(Makefile
	        sources/jamfiles/Makefile
                sources/jamfiles/config.jam
                sources/lib/run-time/Makefile
                sources/dfmc/c-run-time/Makefile)

AC_OUTPUT
