#!/u/gts/perl5/bin/perl

# usage: perl find-warnings.pl files
# Author: Greg Sullivan
#
# command line options:
#   -n num  number of lines to print starting with a matched line.  
#           Default = 0.
#   -b      both -- print warnings _and_ serious warnings.
#   -s      file summaries:  for each file, prints total numbers
#                            of warnings and serious warnings.
#   -g      grand summary:  print grand totals of warnings and 
#                           serious warnings.
#   -e exp  regular expression -- overrides defaults.  Note that 
#             summary counts will be for matches on this regexp.
#   -v      verbose:  print filenames and line numbers always; suitable 
#           for processing by emacs' compilation mode.
#   -o      print lines starting with match until hit blank line.
#   -t      throw away nitpicks.  A nitpick is found by examining the 
#           line _after_ a match for the $nitpick_regexp.
#   -p arch platform:  defaults to "MSDOS".  Others include "UNIX", "VMS".
#   -d file details filename.  If given, details will be printed to 
#             this filename.  Grand summary info will still be printed 
#             to STDOUT also.
#
# Examples:  (assuming "find-warnings" invokes perl with this file)
#   find-warnings -g compile*.log  -- grand total of serious warnings.
#   find-warnings -bg compile*.log -- grand total of serious and non-serious.
#   find-warnings -sg compile*.log -- by-file and grand totals of serious warnings.
#   find-warnings -sgn 9 compile*.log -- same as above plus 9 lines per match.
#   find-warnings -sgvn1 compile*.log -- summaries plus one-liners suitable for
#      emacs' compilation mode (so you might do M-x compile and give it this command).
#   find-warnings -n 1 -e "Invalid type for" compile*.log  -- matches for the given string

# default regular expressions for finding warnings:

$serious_regexp = "^Serious warning";
$warn_regexp = "^Warning";

# use FileHandle;
use File::Basename;
use File::DosGlob 'glob';

use Getopt::Std;

getopts("n:be:stgvopd:");

sub summarize_file;

$numlines = ($opt_n) ? $opt_n : 0;
$both_p = $opt_b;
if ($opt_e) {
    $regexp = $opt_e;
} elsif ($both_p) {
    $regexp = "$serious_regexp|$warn_regexp";
} else {
    $regexp = "$serious_regexp";
};
$summary_p = $opt_s;
$grand_p = $opt_g;
$verbose_p = $opt_v;
$paragraph_mode_p = $opt_o;
$platform = ($opt_p) ? $opt_p : "MSDOS";
$details_file = $opt_d;
$nitpick_p = $opt_t;

if ($nitpick_p) {
    $nitpick_regexp = "defined but not referenced or exported";
} else {
    $nitpick_regexp = "";
}

fileparse_set_fstype($platform);

# print "numlines = $numlines, both? $both_p, summary? $summary_p.\n";
# print "opt_n=[$opt_n], regexp = $regexp.\n";
# exit;

if ($details_file) {
    open(OUTF, ">$details_file");
    $outname = \*OUTF;
} else {
    $outname = \*STDOUT;
}

($echo_p, $count, $numfiles) = (0, 0, 0);
($file_nits, $file_warnings, $file_serious, $grand_warnings, $grand_serious, $grand_nits) = (0,0,0,0,0,0);
($file_matches, $grand_matches) = (0, 0);
%warning_summaries = ();
($printed_filename, $prev_line) = ("","");

@args = @ARGV;
@ARGV = ();
foreach $arg (@args) { @ARGV = (@ARGV, glob $arg) }

##  the prev_line/line buffer is sloppy about end of file
##  -- won't print a match on last line of input, for example.
$prev_line = <ARGV>;            # better have at least one line to go on.
while ($line = <>) {
    $basename = basename($ARGV,".log");
    if ($prev_line =~ $regexp) {	# found a match
	if ($nitpick_regexp ne "" && 
	    ($line =~ $nitpick_regexp || $prev_line =~ $nitpick_regexp)) {
	    $file_nits++;
	    $grand_nits++;
	}
	else {
	    $file_matches++;
	    $grand_matches++;
	    $count = 1;
	    $echo_p = 1;
	}
    }
    if ($echo_p && $prev_line =~ $warn_regexp) { 
	$file_warnings++;
	$grand_warnings++;
    }
    if ($echo_p && $prev_line =~ $serious_regexp) {
	$file_serious++;
	$grand_serious++;
    }
    if ($echo_p) {
	if ($paragraph_mode_p) {
	    if ($prev_line =~ /^\s*$/) {
		print $outname "\n";
		$echo_p = 0;
	    }
	}
	elsif ($count > $numlines) {
	    $echo_p = 0;
	}
    }
    if ($echo_p) {		# we're going to print this line
	if ($verbose_p) {
	    print $outname "$ARGV:$.:$prev_line";
	}
	else {			# if not verbose, print filename only once
	    if ($basename ne $printed_filename) {
		print $outname "\n========== $basename:\n";
		$printed_filename = $basename;
	    }
	    print $outname $prev_line;
	}
	$count++;
    }

    if (eof(ARGV)) {
	$numfiles++;
	if ($summary_p) {	# print summaries for the last file
	    summarize_file;
	}
	if ($grand_p) {
	    $warning_summaries{$basename} = [$file_warnings, $file_serious, $file_nits];
	}
	close ARGV;		# reset $.
	($file_nits, $file_warnings, $file_serious, $file_matches) = (0, 0, 0, 0);
    }
    $prev_line = $line;
}

format gs_top =

Serious                               Non-serious
Warnings  Filename                    Warnings            Nitpicks
-------------------------------------------------------------------
.

format gs =
@>>>>     @<<<<<<<<<<<<<<<<<<<<<<<<     @<<<<<<<          @>>>>>>>
$sss,       $file,                       $www,            $nnn
.

if ($details_file) {
    select OUTF;
    $~ = "gs";
    $^ = "gs_top";
}

select STDOUT;
$~ = "gs";
$^ = "gs_top";

if ($grand_p) { 
    foreach $file (sort keys %warning_summaries) {
	$www = $warning_summaries{$file}->[0];
	$sss = $warning_summaries{$file}->[1];
	$nnn = $warning_summaries{$file}->[2];
	if (($sss > 0) ||
	    ($both_p && ($www > 0 || $sss > 0))) {
	    write;
	    write OUTF if $details_file;
	}
    }
    if ($opt_e) {
	print "\n** Grand total: $grand_matches matches.\n";
	print OUTF "\n** Grand total: $grand_matches matches.\n" if $details_file;
    }
    else {
	print "-----\n$grand_serious Serious warnings";
	print " and $grand_warnings Warnings";
	print " and $grand_nits Nitpicks";
	print " in $numfiles files.\n";
	if ($details_file) {
	    print OUTF "-----\n$grand_serious Serious warnings";
	    print OUTF " and $grand_warnings Warnings";
	    print OUTF " and $grand_nits Nitpicks";
	    print OUTF " in $numfiles files.\n";
	}
    }
}

if ($details_file) {
    close OUTF;
}

sub summarize_file {
    if ($opt_e && $file_matches > 0) {
	print $outname "* $basename: $file_matches matches.\n";
    }
    elsif ($both_p && ($file_warnings > 0 || $file_serious > 0)) {
	print $outname "* $basename: $file_warnings Warnings and ";
	print $outname "* $basename: $file_nits Nitpicks and ";
	print $outname "$file_serious Serious Warnings.\n";
    }
    elsif ($file_serious > 0) {
	print $outname "* $basename: $file_serious Serious Warnings.\n";
    }
}
