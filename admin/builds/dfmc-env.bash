#!/usr/local/bin/bash
#      Script: dfmc-env.bash
#      Author: Shri Amit(amit)
use="   Usage: dfmc-env.bash -r <Root> -p <Personal Root> -l <Platform> -s"
# Opts & Flgs: -r: Root of sources, default: 
#                    unix and pc c back end: /u/dylan
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

## Setup lispworks licensing stuff ##
##
source /u/dylan/admin/builds/env.bash

## Ensure that platform is supported ##
##
case $platform in
    sparc-solaris2 | sparc-sunos4 | alpha-osf3) ;;
    *) echo "$0 - Error - unsupported host platform"
       exit 1 ;;
esac

## Parse the command line and check incorrect usage ##
##
while [ "$#" -ne 0 ]; do
  case $1 in
   -r | -root) shift
               root="$1" ;;
     -p | -pr) shift
	       personal_root="$1" ;;
     -l | -pl) shift
	       target_platform="$1" ;;
 -s | -static) linking_type="static" ;;
            *) echo dfmc-env.bash: Invalid command line option -  $option
               echo $use 
               exit 2;;
  esac
  shift
done

## Ensure that target platform is supported and set ##
## the pentium root if applicable                   ##
##
target_platform=${target_platform:-$platform}
case $target_platform in
    x86-win32 | sparc-solaris2 | sparc-sunos4 | alpha-osf3)
	     root=${root:-/u/dylan} ;;
    *) echo "$0 - Error - unsupported target platform"
       exit 3 ;;
esac

## Defaults for command line options ##
##
personal_root=${personal_root:-$root}
linking_type=${linking_type:-dynamic}

## Export the DFMC environment variables ##
##
export WEBSTER_SYSTEM_ROOT=$root
export WEBSTER_PLATFORM_NAME=$target_platform
export WEBSTER_PERSONAL_ROOT="$personal_root/"
install=$WEBSTER_SYSTEM_ROOT/install/$WEBSTER_PLATFORM_NAME
export WEBSTER_SYSTEM_BIN=$install/bin
export WEBSTER_SYSTEM_LIB=$install/lib
export WEBSTER_SYSTEM_INCLUDE=$install/include
pinstall=$WEBSTER_PERSONAL_ROOT/install/$WEBSTER_PLATFORM_NAME
export WEBSTER_PERSONAL_BIN=$pinstall/bin
export WEBSTER_PERSONAL_LIB=$pinstall/lib
export WEBSTER_PERSONAL_INCLUDE=$pinstall/include
export WEBSTER_PERSONAL_BUILD=$WEBSTER_PERSONAL_ROOT/build/$WEBSTER_PLATFORM_NAME
export WEBSTER_SYSTEM_BUILD=$WEBSTER_SYSTEM_ROOT/build/$WEBSTER_PLATFORM_NAME
export WEBSTER_C_FLAGS="-g -O"
LD_LIBRARY_PATH=$WEBSTER_PERSONAL_LIB:$WEBSTER_SYSTEM_LIB:$LD_LIBRARY_PATH  
export LD_LIBRARY_PATH

## Setup vars for linking ##
##
case $linking_type in
 dynamic) case $WEBSTER_PLATFORM_NAME in
	   alpha-osf3) export WEBSTER_LIB_SUFFIX="so"
		       export WEBSTER_LD="ld -shared"
		       export WEBSTER_LINKLIB_FLAGS="-expect_unresolved '*'" ;;
           sparc-solaris2) export WEBSTER_LIB_SUFFIX="so"
                           export WEBSTER_LD="ld -G" ;;
           *) export WEBSTER_LIB_SUFFIX="a"
              export WEBSTER_LD="ld -r" ;;
	  esac ;;
 static) export WEBSTER_LIB_SUFFIX="a"
         export WEBSTER_LD="ld -r" ;;
esac

# eof
