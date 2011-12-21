AC_INIT([Open Dylan], [1.0 beta 5])
AC_PREREQ(2.50)

# Directory for config.guess etc.
AC_CONFIG_AUX_DIR(build-aux)

#---------------------
# Make srcdir absolute
#---------------------

srcdir=`(cd $srcdir; pwd)`

AC_DEFUN([FD_PLATFORM],[
case $1 in
i*86-*-linux*) $2=x86-linux; $3=sources/lib/run-time/pentium-linux ;;
i*86-*-freebsd*) $2=x86-freebsd; $3=sources/lib/run-time/pentium-freebsd ;;
i*86-*-darwin*) $2=x86-darwin; $3=sources/dfmc/c-run-time; $4="-arch i386" ;;
x86_64-apple-darwin*) $2=x86-darwin; $3=sources/dfmc/c-run-time; $4="-arch i386" ;;
powerpc-*-darwin*) $2=ppc-darwin; $3=sources/dfmc/c-run-time; $4="-arch ppc" ;;
powerpc-*-linux-gnu*) $2=ppc-linux; $3=sources/dfmc/c-run-time ;;
amd64-*-freebsd*) $2=amd64-freebsd; $3=sources/dfmc/c-run-time ;;
x86_64-*-linux*) $2=x86_64-linux; $3=sources/dfmc/c-run-time ;;
*) AC_MSG_ERROR([The $1 platform is not supported.]) ;;
esac])
 
AC_CANONICAL_BUILD
FD_PLATFORM($build,fd_build,rt_build,arch_build)
AC_SUBST(fd_build)
AC_SUBST(rt_build)
AC_SUBST(arch_build)

AC_CANONICAL_HOST
FD_PLATFORM($host,fd_host,rt_host,arch_host)
AC_SUBST(fd_host)
AC_SUBST(rt_host)
AC_SUBST(arch_host)

AC_CANONICAL_TARGET
FD_PLATFORM($target,fd_target,rt_target,arch_target)
AC_SUBST(fd_target)
AC_SUBST(rt_target)
AC_SUBST(arch_target)

AM_INIT_AUTOMAKE

AC_PROG_INSTALL
AC_PROG_LN_S

AC_PATH_PROG(PERL, perl, no)
test "$PERL" = no && AC_MSG_ERROR([
perl is required to build Open Dylan])
AC_SUBST(PERL)

PERL_MODULES="File::Spec Getopt::Long XML::Parser"
for module in $PERL_MODULES; do
  AC_MSG_CHECKING(for Perl module $module)
  $PERL -M$module -e 'exit 0' || AC_MSG_ERROR([
$module is required to build Open Dylan])
  AC_MSG_RESULT(yes)
done

AC_CHECK_PROGS(DYLANCOMPILER, dylan-compiler opendylan minimal-console-compiler, no)
test "$DYLANCOMPILER" = no && AC_MSG_ERROR([
dylan-compiler, opendylan or minimal-console-compiler is required to bootstrap Open Dylan])
test "$DYLANCOMPILER" = dylan-compiler && DYLANCOMPILER="$DYLANCOMPILER -build"
test "$DYLANCOMPILER" = opendylan && DYLANCOMPILER="$DYLANCOMPILER -build"
test "$DYLANCOMPILER" = minimal-console-compiler && DYLANCOMPILER="$DYLANCOMPILER -build"
AC_SUBST(DYLANCOMPILER)

# TODO: figure out the bootstrap type given the fdcompile version
bootstrap_target=1-stage-bootstrap
AC_SUBST(bootstrap_target)

AC_ARG_WITH(arch,
            AC_HELP_STRING([--with-arch=ARCH],
                           [Runtime architecture, e.g., i386 or x86_64.]),
            [arch_flag="-arch $with_arch"],
	    [arch_flag=$arch_target])
AC_SUBST(arch_flag)

AC_ARG_WITH(mps,
            AC_HELP_STRING([--with-mps=PATH],
                           [Path to MPS distribution and build.]))
AC_SUBST(with_mps)

AC_ARG_WITH(gc,
            AC_HELP_STRING([--with-gc=PATH],
			   [Path of Boehm GC installation.]))
AC_SUBST(with_gc)

AC_ARG_WITH(library-source,
            AC_HELP_STRING([--with-library-source=PATH],
                           [Path to Dylan contributed libraries source.]))
AC_SUBST(with_library_source)

AC_MSG_CHECKING(for Library Packs to include)
LIBRARY_PACKS=Core
AC_ARG_ENABLE(network,
	      AC_HELP_STRING([--disable-network],
	                     [Do not build the basic networking library]),
	      ,LIBRARY_PACKS="$LIBRARY_PACKS Network")
AC_ARG_ENABLE(testworks,
	      AC_HELP_STRING([--enable-testworks],
	                     [Build the unit test framework library]),
	      LIBRARY_PACKS="$LIBRARY_PACKS TestWorks")
AC_ARG_ENABLE(dood,
	      AC_HELP_STRING([--enable-dood],
	                     [Build the DOOD persistent storage library]),
	      LIBRARY_PACKS="$LIBRARY_PACKS DOOD")
AC_MSG_RESULT($LIBRARY_PACKS)
AC_SUBST(LIBRARY_PACKS)

# for now, replace mps only in pentium-linux
# gc is replaced in sources/dfmc/c-run-time
AC_CONFIG_FILES(Makefile sources/lib/run-time/pentium-linux/Makefile sources/lib/run-time/pentium-freebsd/Makefile sources/dfmc/c-run-time/Makefile)

AC_OUTPUT
