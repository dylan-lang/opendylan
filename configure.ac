AC_INIT([Open Dylan], [2013.2])
AC_PREREQ(2.50)

# Directory for config.guess etc.
AC_CONFIG_AUX_DIR(build-aux)

#---------------------
# Make srcdir absolute
#---------------------

srcdir=`(cd $srcdir; pwd)`

SUPPORT_GC_MPS=no
SUPPORT_GC_BOEHM=yes
GC_USE_STATIC_BOEHM=false
GC_LIB=-lgc
ADDITIONAL_RUNTIME_TARGET_PLATFORMS=

AC_CANONICAL_TARGET

MULTIARCH=$host
case $host in
  i*86-*-linux*)
      OPEN_DYLAN_TARGET_PLATFORM=x86-linux;
      SUPPORT_GC_MPS=yes;
      SUPPORT_GC_BOEHM=no
      MULTIARCH=i386-linux-gnu
    ;;
  i*86-*-freebsd*)
      OPEN_DYLAN_TARGET_PLATFORM=x86-freebsd;
      SUPPORT_GC_MPS=yes;
      SUPPORT_GC_BOEHM=no
    ;;
  i*86-*-darwin*)
      OPEN_DYLAN_TARGET_PLATFORM=x86-darwin;
      ADDITIONAL_RUNTIME_TARGET_PLATFORMS="x86_64-darwin";
      GC_USE_STATIC_BOEHM=true
    ;;
  x86_64-*-darwin*)
      OPEN_DYLAN_TARGET_PLATFORM=x86-darwin;
      ADDITIONAL_RUNTIME_TARGET_PLATFORMS="x86_64-darwin";
      GC_USE_STATIC_BOEHM=true
    ;;
  powerpc-*-darwin*)
      OPEN_DYLAN_TARGET_PLATFORM=ppc-darwin;
    ;;
  amd64-*-freebsd* | x86_64-*-freebsd*)
      OPEN_DYLAN_TARGET_PLATFORM=x86_64-freebsd;
      GC_LIB=-lgc-threaded
    ;;
  x86_64-*-linux*)
      OPEN_DYLAN_TARGET_PLATFORM=x86_64-linux;
      MULTIARCH=x86_64-linux-gnu
    ;;
  arm-*-linux*)
      OPEN_DYLAN_TARGET_PLATFORM=arm-linux;
    ;;
  *)
      AC_MSG_ERROR([The $host platform is not supported.])
    ;;
esac
AC_SUBST(OPEN_DYLAN_TARGET_PLATFORM)
AC_SUBST(ADDITIONAL_RUNTIME_TARGET_PLATFORMS)

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

if test "$cross_compiling" != "yes"; then
    AS_IF([test x${GC_USE_STATIC_BOEHM} == xtrue],
          [
            GC_LIB_NAME=libgc.a
          ],
          [
            AS_IF([test x${OPEN_DYLAN_TARGET_PLATFORM} == xx86-darwin],
                  [
                    GC_LIB_NAME=libgc.dylib
                  ],
                  [
                    GC_LIB_NAME=libgc.so
                  ])
          ]
    )

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
            GC_LFLAGS="-L${with_gc}/lib ${GC_LIB}"
            AC_CHECK_FILE([${with_gc}/lib/${GC_LIB_NAME}],
                          [
                            GC_LIBRARY="${with_gc}/lib/${GC_LIB_NAME}"
                          ],
                          [])
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
            GC_LFLAGS="${GC_LIB}"
            GC_CHOICE="boehm"
            AC_CHECK_FILE([/usr/lib/${GC_LIB_NAME}],
                          [
                            GC_LIBRARY="/usr/lib/${GC_LIB_NAME}"
                          ],
                          [
                            AC_CHECK_FILE([/usr/local/lib/${GC_LIB_NAME}],
                                          [
                                            GC_LIBRARY="/usr/local/lib/${GC_LIB_NAME}"
                                          ],
                                          [
                                            AC_CHECK_FILE([/usr/lib/${MULTIARCH}/${GC_LIB_NAME}],
                                                          [
                                                            GC_LIBRARY="/usr/lib/${MULTIARCH}/${GC_LIB_NAME}"
                                                          ],
                                                          [])
                                          ])
                          ])
          ]
    )
fi

AS_IF([test x${GC_CHOICE}x${GC_LIBRARY} == xboehmx],
      [
        AC_MSG_ERROR([Could not find ${GC_LIB_NAME}. Please specify the path to the Boehm GC installation via --with-gc])
      ]
)
AC_SUBST(GC_CFLAGS)
AC_SUBST(GC_LFLAGS)
AC_SUBST(GC_LIBRARY)
AC_SUBST(GC_CHOICE)
AC_SUBST(GC_USE_STATIC_BOEHM)

# We don't want to build generated code with warnings enabled usually.
# The exception to this is if the person running configure has passed
# in CFLAGS with -W options ...
AC_MSG_CHECKING(if we should disable C compiler warnings for generated code)
AS_IF([echo "$CFLAGS" | grep -q -- "-W" 2> /dev/null],
      [
        DISABLE_WARNINGS_CFLAGS=
        AC_MSG_RESULT(no)
      ],[
        DISABLE_WARNINGS_CFLAGS=-w
        AC_MSG_RESULT(yes)
      ]
)
AC_SUBST(DISABLE_WARNINGS_CFLAGS)

AC_CONFIG_FILES(Makefile
                sources/jamfiles/Makefile
                sources/jamfiles/config.jam
                sources/lib/run-time/Makefile)

AC_OUTPUT
