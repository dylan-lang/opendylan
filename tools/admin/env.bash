#!/usr/local/bin/bash
#     Script: env.bash
#     Author: Shri Amit(amit)
#      Usage: source env.bash
#   Synopsis: The following script sets a Bourne or Bash shell
#             environment used by the dylan admin scripts
#######################################################################

## All commonly used variables ##
##
dylandir=/u/dylan;                     export dylandir
localdir=$dylandir/local;              export localdir
toolsdir=$dylandir/tools;              export toolsdir
admindir=$toolsdir/admin;              export admindir
logsdir=$admindir/logs;                export logsdir
qadir=$dylandir/qa;                    export qadir
scriptsdir=$toolsdir/scripts;          export scriptsdir
releasedir=$dylandir/releases;         export releasedir
platform=`$scriptsdir/dylan-platform`; export platform
site=`domainname | cut -f1 -d "."`;    export site
host=`$admindir/get-host-name`;        export host
suffix=`date '+%y%m%d'`;               export suffix
imagestokeep=2;                        export imagestokeep
logstokeep=2;                          export logstokeep

## Setup the lispworks variables ##
##
source $admindir/lw-license-variables

## The mailing lists for various release phases        ##
##
## rel_eng: A list of the people maintain the builds   ##
##          and wish to get the reports from all sites ##
## send_report: The list of people who wish to see the ##
##          build-report from all sites
## <site>:  Site specific lists that are concatenated  ##
##          to the send_report list for build report   ## 
##          This allows people to get reports for their##
##          own site.
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
send_report="$rel_eng"; export send_report
onecc="andrewa,jwl";    export onecc
seattle="roman";        export seattle
menlo="";               export menlo
long="";                export long
ed="hughg";             export ed
cam="";                 export cam
case $site in
     1cc) send_report="$send_report,$onecc"   ;;
 seattle) send_report="$send_report,$seattle" ;;
   menlo) send_report="$send_report,$menlo"   ;;
    long) send_report="$send_report,$long"    ;;
      ed) send_report="$send_report,$ed"      ;;
     cam) send_report="$send_report,$cam"     ;;
    *) ;;
esac
env_hackers="$rel_eng";                                 export env_hackers
clim_hackers="$rel_eng";                                export clim_hackers
emu_hackers="$rel_eng";                                 export emu_hackers
dfmc_hackers="$rel_eng,`cat $scriptsdir/dfmc-hackers`"; export dfmc_hackers
#emu_hackers="$rel_eng,jonathan,keith"
#env_hackers="$rel_eng,andrewa,swm"
#clim_hackers="$rel_eng,swm,sgr"

## Path to remove the need for full path names all the time ##
##
PATH="/usr/bin:$admindir:$scriptsdir:/usr/ucb:$PATH"; export PATH
umask 000

## Function: send-mail ##
## Usage:    send-mail <message> <usernames> <logfile> <subject> <error string>  ##
## Synopsis: sends <message> to <usernames> and uses fabs to find <error string> ##
##           and return 10 lines above and below the first occurrence thereof     ##
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
    echo "The first occurrence of \"$the_error_string\" in:"
    echo "   $the_log:"
    echo ""
    $admindir/fabs "$the_log" "$the_error_string" -a 10 -b 10 -t
  else
    echo "Log file $the_log not found. Something is seriouly wrong!"
  fi | "$mailer" -s "$the_subject" "$the_hackers"
}
export -f send-mail

# eof
