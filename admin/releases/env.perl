#!/usr/local/bin/perl
#   Script: env.perl
#   Author: Shri Amit(amit)
#    Usage: env.perl
# Synopsis: The following script defines the environment, common
#           variables and functions used by all perl release scripts.
#           Other perl script can just "require" this script
####################################################################
## Application exit codes ##
## 0: successful
## 1: send-mail error - unidentified platform

## All commonly used variables ##
##
$dylanroot    = "/u/dylan";
$scriptsdir   = "$dylanroot/admin/scripts";
$buildsdir    = "$dylanroot/admin/builds";
$rel_admindir = "/u/dylan/releases/kan/admin";
$platform     = `$scriptsdir/dylan-platform`;   chop($platform);
$host         = `$buildsdir/get-host-name`;     chop($host);
$suffix       = `date '+%y%m%d'`;               chop($suffix);
$time         = `date '+%H%M%S'`;               chop($time);
$site         = `domainname | cut -f1 -d "."`;  chop($site);

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
$rel_eng="dylan-admin";

## Subroutine: send_mail ##
##      Usage: &send-mail(<message> <usernames> <subject>)   ##
##   Synopsis: sends <message> to <usernames> with <subject> ##
##
## Note that this mail function is different from the one in ##
## env.bash because perl buffers its logfiles and so it is   ##
## not possible to have the same program send mail from its  ##
## logfile as the buffer is not saved until the script quits ##
##
sub send_mail {
    local($the_message) = $_[0];
    local($the_hackers) = $_[1];
    local($the_subject) = $_[2];
    local($mailer);

    ## Platform-dependent mailing commands.
    ##
    if ($platform eq "sparc-sunos4" 
	|| $platform eq "sparc-solaris2") {
	$mailer="/usr/ucb/mail";
    } elsif ($platform eq "alpha-osf3") {
	$mailer="mailx";
    } else {
	&output("send_mail: error - No mailer specified for $platform platform.");
       exit(1);
    }
    `echo "$the_message" | "$mailer" -s "$the_subject" "$the_hackers"`;
}

## The following variable determines, the type of ##
## output provided by the subroutine &output. It  ##
## has 3 recognized values:                       ##
##   "both": both stdout and to THELOG            ##
## "stdout": only to stdout                       ##
##    "log": only to THELOG                       ##
## Note that the latter two options can also be   ##
## acheived directly by using the perl print cmd  ##
## but the subroutines &application_error and     ##
## &execute use &output to direct their output.   ##
## This also assumes that the logfile has a file  ##
## handle called THELOG                           ##
##
$OUTPUT_TYPE = "stdout";

## Sends output to file descriptor described by ##
## $OUTPUT_TYPE                                 ##
##
sub output {
    foreach $arg (@_) {
	if ($OUTPUT_TYPE eq "both") {
	    print "\n$arg";
	    print THELOG "\n$arg";
	} elsif ($OUTPUT_TYPE eq "stdout") {
	    print "\n$arg";
	} elsif ($OUTPUT_TYPE eq "log") {
	    print THELOG "\n$arg";
	}
    }
}

## Subroutine: application_error ##
##      Usage: &application_error(<error_name>, <exit_value>,  ## 
##                                <app_name>, <app_usage>)     ##
##   Synopsis: Exits the application with <error_name> message ##
##             and <exit_value> error code                     ##
##
sub application_error {
    local($error_name, $error_value, $app_name, $app_usage) = @_;
    &output("$app_name: Error - $error_name",
	    "$app_usage\n\n");
    exit($error_value);
}

## Subroutine: execute ##
##      Usage: &execute(<arg>)                               ##
##   Synopsis: Executes <arg> in the shell and collects text ##
##             the output and sends it to &output            ##
##
sub execute {
    local($command) = $_[0];
    local($i);
    @exec_output = `$command`;
    foreach $i (@exec_output) {
	chop($i);
	&output("$i");
    }
}

# eof
