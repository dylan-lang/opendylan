#!/usr/local/bin/bash
#      Script: env.bash <Root>
#      Author: Shri Amit(amit)
usage=" Usage: source env.bash <Root>"
#    Synopsis: The following script sets a Bourne or Bash shell
#              environment used by the kan release scripts under
#              the given root.
#######################################################################
## Application exit status legend ##
##  1: Root not specified

## To maintain the identity of each release w.r.t the pathnames ##
## seen by the innards of the build and the compiler etc.       ##
##
dylanroot="/u/dylan"
scriptsdir="$dylanroot/admin/scripts"
root=$1
case $root in
    "") echo "$0: Error - Root must be specified"
        echo $usage
        exit 1;;
     *) ;;
esac        

## Set the paths ##
##
PATH="/bin:/usr/bin:/usr/local/bin:/usr/ucb:$PATH"; export PATH
umask 000

## The file utility on sparc-solaris2 platforms is broken,  ##
## it claims symlinks are directories! The following kludge ##
## has been incorporated for our sparc-solaris2 machines    ##
## 
site=`domainname | cut -f1 -d "."`;     export site
platform=`$scriptsdir/dylan-platform`;  export platform
case $platform in
    sparc-solaris2) case $site in
		     ed | long) solaris_prefix="rsh casper.long -n" ;;
		           1cc) solaris_prefix="rsh banshee -n" ;;
		           cam) solaris_prefix="rsh dorsal -n" ;;
		         menlo) solaris_prefix="rsh meteor -n" ;;
		       seattle) solaris_prefix="rsh skookum -n" ;;
		       *) echo "No alternate machine specified to get around"
		          echo "the file bug at this site." ;;			  
		    esac ;;
		 *) ;;
esac
echo the solaris prefix is --$solaris_prefix--
resolved_root=$root
if test -L $root; then
    root_dir=`dirname $root`
    resolved_root=`$solaris_prefix file $root | cut -f4 -d " "`
    echo the resolved root is --$resolved_root--
    if test ! -d $resolved_root; then
	resolved_root=`echo $resolved_root | cut -f2 -d "/"`
	echo the resolved root is --$resolved_root--
	resolved_root="$root_dir/$resolved_root"
    fi
fi
echo "The resolved root is: --$resolved_root--"

## All commonly used variables ##
##
root=$resolved_root;                    export root
localdir=$root/local;                   export localdir
admindir=$root/admin;                   export admindir
logsdir=$admindir/logs;                 export logsdir
qadir=$root/qa;                         export qadir
installdir=$root/install;               export installdir
patchdir=$root/patches;                 export patchdir
bindir=$installdir/$platform/bin;       export bindir
host=`$admindir/get-host-name`;         export host
suffix=`date '+%y%m%d'`;                export suffix
time=`date +%H%M%S`;                    export time

## Setup lispworks variables ##
##
source $dylanroot/admin/builds/lw-license-variables

## The mailing lists for various release phases        ##
##
## rel_eng: A list of the people maintain the builds   ##
##          and wish to get the reports from all sites ##
## <site>:  Site specific lists that are concatenated  ##
##          to the rel_eng list before mails are sent  ## 
##          out. This allows people to get reports for ##
##          their own site.
## dfmc_hackers: List of compiler developers interested##
##               in dfmc builds                        ##
## env_hackers: List of development env developers     ##
##              who should be informed about env bugs  ##
## clim_hackers: List of people interesting in the     ##
##               clim images                           ##
## emu_hackers: List of people responsible for bugs in ##
##              the emulator                           ##
##
rel_eng="dylan-admin";  export rel_eng
onecc="";               export onecc
seattle="";             export seattle
menlo="";               export menlo
long="";                export long
ed="";                  export ed
cam="";                 export cam
case $site in
     1cc) rel_eng="$rel_eng,$onecc"   ;;
 seattle) rel_eng="$rel_eng,$seattle" ;;
   menlo) rel_eng="$rel_eng,$menlo"   ;;
    long) rel_eng="$rel_eng,$long"    ;;
      ed) rel_eng="$rel_eng,$ed"      ;;
     cam) rel_eng="$rel_eng,$cam"     ;;
    *) ;;
esac
env_hackers="$rel_eng";                      export env_hackers
clim_hackers="$rel_eng";                     export clim_hackers
emu_hackers="$rel_eng";                      export emu_hackers
dfmc_hackers="$rel_eng";                     export dfmc_hackers
#dfmc_hackers=`cat $scriptsdir/dfmc-hackers` 
#emu_hackers="$rel_eng,jonathan,keith"
#env_hackers="$rel_eng,andrewa,swm"
#clim_hackers="$rel_eng,swm,sgr"

## Function: send-mail ##
## Usage:    send-mail <message> <usernames> <logfile> <subject> <error string>  ##
## Synopsis: sends <message> to <usernames> and uses fabs to find <error string> ##
##           and return 10 lines above and below the first occurance thereof     ##
##
function send-mail() {
  the_message=$1
  the_hackers=$2
  the_log=$3
  the_subject=$4
  the_error_string=$5
  case $the_error_string in
    "") the_error_string="Caught build error:" ;;
     *) ;;
  esac

  ## Platform-dependent mailing commands.
  ##
  case $platform in
    sparc-sunos4 | sparc-solaris2) mailer="/usr/ucb/mail";;
    alpha-osf3)                    mailer="mailx";;
    *) echo "send-mail: error - No mailer specified for $platform platform."; 
       exit 1;;
  esac

  ## Echo summary info into a mail pipe
  ##
  if test -f $the_log; then
    echo "Synopsis:"
    echo "   $the_message"
    echo ""
    echo "The first occurance of \"$the_error_string\" in:"
    echo "   $the_log:"
    echo ""
    $admindir/fabs "$the_log" "$the_error_string" -a 10 -b 10 -t
  else
    echo "Log file $the_log not found. Something is seriouly wrong!"
  fi | "$mailer" -s "$the_subject" "$the_hackers"
}
export -f send-mail

# eof
