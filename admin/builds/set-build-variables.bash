#!/usr/local/bin/bash
#      Script: set-build-variables.bash
#      Author: Shri Amit(amit), Andy Armstrong(andrewa)
use="   Usage: set-build-variables.bash -r <Root> -p <Personal Root> -l <Platform> -s"
# Opts & Flgs: -r: Root of sources, default: 
#                    unix and pc c back end: /u/dylan/releases/kan
#                          pentium back end: /u/dylan/releases/pentium-kan
#              -p: Personal Root, default: $root
#              -l: Platform target for cross compilation, default: none
#              -s: Enables static linking, default: dynamic (except on
#                  sparc-sunos4 on which only static linking is supported)
#    Synopsis: . Sets and exports all the environment variables used 
#                by the dfmc compiler.
######################################################################
## Application exit status legend ##
##  0: Env setup successful
##  1: Unsupported host platform
##  2: Invalid command line option
##  3: Unsupported target platform

## Parse the command line and check incorrect usage ##
##
dylanroot="/u/dylan"
scriptsdir="$dylanroot/admin/scripts"
root=""
personal_root=""
target_platform=""
sources_directory=""
install_directory=""
build_directory=""
linking_type=""
while [ "$#" -ne 0 ]; do
  case $1 in
   -r | -root) shift
               root="$1" ;;
     -p | -pr) shift
	       personal_root="$1" ;;
    -sp | -ps) shift
	       personal_root="$1"
	       root="$1" ;;
     -l | -pl) shift
	       target_platform="$1" ;;
     -sources) shift
	       sources_directory="$1" ;;
     -install) shift
	       install_directory="$1" ;;
       -build) shift
	       build_directory="$1" ;;
 -s | -static) linking_type="static" ;;
            *) echo set-build-variables.bash: Invalid command line option -  $option
               echo $use 
               exit 2;;
  esac
  shift
done

## Setup lispworks licensing stuff ##
##
source $dylanroot/admin/builds/lw-license-variables

## Ensure that platform is supported ##
##
platform=`$scriptsdir/dylan-platform`
case $platform in
    sparc-solaris2 | sparc-sunos4 | alpha-osf3) ;;
    *) echo "$0 - Error - unsupported host platform $platform"
       exit 1 ;;
esac

## Ensure that target platform is supported and set ##
## the pentium root if applicable                   ##
##
target_platform=${target_platform:-$platform}
platform_name=$target_platform
case $target_platform in
    x86-win32 | sparc-solaris2 | sparc-sunos4 | alpha-osf3)
	     root=${root:-/u/dylan/releases/kan} ;;
    pentium) root=${root:-/u/dylan/releases/pentium-kan} 
	     platform_name=x86-win32 ;;
    *) echo "$0 - Error - unsupported target platform $target_platform"
       exit 3 ;;
esac

## Defaults for command line options ##
##
personal_root=${personal_root:-$root}
linking_type=${linking_type:-dynamic}

## Export the Emulator environment variables. Strictly ##
## speaking these don't belong here, but some users    ##
## find it easier to use the same image for the dfmc   ##
## as well as emulator.                                ##
##
export DYLAN_ROOT=$root
export TRUNK_SYSTEM_ROOT="$root/"

## Export the release variables
##

export OPEN_DYLAN_PLATFORM_NAME=$platform_name
export OPEN_DYLAN_RELEASE="$root/"
export OPEN_DYLAN_RELEASE_REGISTRIES="$OPEN_DYLAN_RELEASE/sources/registry/"
case $target_platform in
    x86-win32 | sparc-solaris2 | sparc-sunos4 | alpha-osf3)
        export OPEN_DYLAN_RELEASE_INSTALL=$OPEN_DYLAN_RELEASE/platforms/$platform_name ;;
    pentium)
        export OPEN_DYLAN_RELEASE_INSTALL=$OPEN_DYLAN_RELEASE ;;
    *) echo "$0 - Error - unsupported target platform $target_platform"
       exit 3 ;;
esac

## Export the user variables
##
sources_directory=${sources_directory:-$personal_root/sources}
case $target_platform in
    x86-win32 | sparc-solaris2 | sparc-sunos4 | alpha-osf3)
	install_directory=${install_directory:-$personal_root/platforms/$platform_name}
	build_directory=${build_directory:-$install_directory/build} ;;
    pentium)
	install_directory=${install_directory:-$personal_root}
	build_directory=${build_directory:-$personal_root/build} ;;
    *) echo "$0 - Error - unsupported target platform $target_platform"
       exit 3 ;;
esac
export OPEN_DYLAN_USER_ROOT="$personal_root/"
export OPEN_DYLAN_USER_REGISTRIES=$sources_directory/registry
export OPEN_DYLAN_USER_BUILD=$build_directory
export OPEN_DYLAN_USER_INSTALL=$install_directory

## Other build settings
##
export OPEN_DYLAN_C_FLAGS="-g -O"
LD_LIBRARY_PATH=$OPEN_DYLAN_USER_INSTALL/lib:$OPEN_DYLAN_RELEASE_INSTALL/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH
export TMPDIR=/usr/tmp

## Setup vars for linking ##
##
case $linking_type in
 dynamic) case $OPEN_DYLAN_PLATFORM_NAME in
	   alpha-osf3) export OPEN_DYLAN_LIB_SUFFIX="so"
		       export OPEN_DYLAN_LD="ld -shared"
		       export OPEN_DYLAN_LINKLIB_FLAGS="-expect_unresolved '*'" ;;
           sparc-solaris2) export OPEN_DYLAN_LIB_SUFFIX="so"
                           export OPEN_DYLAN_LD="ld -G" ;;
           *) export OPEN_DYLAN_LIB_SUFFIX="a"
              export OPEN_DYLAN_LD="ld -r" ;;
	  esac ;;
 static) export OPEN_DYLAN_LIB_SUFFIX="a"
         export OPEN_DYLAN_LD="ld -r" ;;
esac

## *** Remove these old names when the project manager is updated
export OPEN_DYLAN_PROJECTS_BUILD=$OPEN_DYLAN_USER_BUILD
export OPEN_DYLAN_PROJECTS_INSTALL=$OPEN_DYLAN_USER_INSTALL

echo "-----------------------------------------------------------------------"
echo "                         Environment Settings                          "
echo "-----------------------------------------------------------------------"
set | grep -e ^FUNCTIONAL
set | grep -e ^DYLAN
echo "-----------------------------------------------------------------------"

# eof
