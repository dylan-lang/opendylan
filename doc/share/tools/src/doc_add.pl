#! /usr/local/bin/perl

# Script to add doc units or compounds to HOPE.
#
# Arguments are generally the same as those to the HOPE add command.
# The script parses the arguments.
#
# Differences between the script's arguments and the HOPE command's
# arguments:
#  (a) Because of the script's heuristics for unit names, the user can
#      give arguments derived from file names.  Thus, you can use file
#      globbing -- e.g., you can say "doc_add -comp foo *.doc".
#      You don't have to put -and between unit names.
#
# Environment variables can specify the commands that the script
# uses internally.  The values can have more than one word; for example,
# you could set HOPE_CMD to "hope verbose" instead of "hope".  Following
# are the environment variables that specify commands:
#   HOPE_CMD     default "hope"
#
# This script does not attempt to compress files before adding them.
# If you want to add compressed files, you must compress them yourself
# (usually via "gzip") before using this script.  If the files to be
# added are compressed, use the -binary option.  Adding support for
# automatic compression is left as an exercise for the reader.  See
# the sobering doc_checkin script before you try.

# Main routine.  Sequence of events:
#   Initialize variables
#   Parse argument list
#   Execute HOPE add command
#
# Some global variables bound here, in addition to those set from
# environment variables:
#   @HOPE_ARGS     Array of arguments to HOPE add command
#
sub main {
    local (@HOPE_ARGS);
    local (@HOPE_CMD) = split (' ', ($ENV{"HOPE_CMD"} || "hope"));
    $|=1;
    &process_arglist;
    &print_hope_command ("add");
    system (@HOPE_CMD, "add", @HOPE_ARGS);
}

# Parse the command argument list.  Arguments are the same as those to
# the HOPE add command.  Unfortunately, the list of arguments is
# guaranteed to get out of date as HOPE changes.
#
# We first scan the arglist looking for a -help option.  If found, we
# return right away, because the command is essentially a no-op.
#
# Arguments we don't care about are accumulated in the @HOPE_ARGS array.
# Arguments we do care about:
#   -compound                             Set $COMPOUND to next arg
#   -subcompound                          Set $SUBCOMP to next arg
#   -unit (or arg not beginning with -)   Push onto @UNITS
#   -filename                             Set $FILENAME to next arg
#
# At the end of the arglist, or if we find -and followed by more args,
# we process the current object (compound plus subcompound or units).
#
# Some other global variables set or bound here:
#   %ARG_ARRAY          Assoc array of command options and information
#   %GLOBAL_ARGS_SEEN   Assoc array to keep track of global options seen
#   %LOCAL_ARGS_SEEN    Assoc array to keep track of local options seen
#   %CONJ_ARGS_SEEN     Assoc array to keep track of conjunctions seen
#   (various)           Constants with integer values
#
sub process_arglist {
    local ($COMPOUND, $SUBCOMP, $FILENAME, @UNITS);
    local ($ARG, $NEXT_ARG, $FULL_ARG, $NEXT_ACTION);
    local (%ARG_ARRAY, %GLOBAL_ARGS_SEEN, %LOCAL_ARGS_SEEN, %CONJ_ARGS_SEEN);
    local ($NO_ARG) = 0;
    local ($TAKES_ARG) = 1;
    local ($LOCAL_ARG) = 0;
    local ($GLOBAL_ARG) = 2;
    local ($CONJ_ARG) = 4;
    local ($ARG_NOT_SEEN) = 0;
    local ($ARG_SEEN) = 1;
    &fill_arg_array (*ARG_ARRAY);
    &fill_global_args_seen (*GLOBAL_ARGS_SEEN);
    &fill_local_args_seen (*LOCAL_ARGS_SEEN);
    &fill_conj_args_seen (*CONJ_ARGS_SEEN);
    if (&scan_arglist_for_help) { return; }
    while (@ARGV) {
	$ARG = shift (@ARGV);
	($FULL_ARG, $NEXT_ACTION) =
	    &match_arg ($ARG, *ARG_ARRAY, *GLOBAL_ARGS_SEEN,
			*LOCAL_ARGS_SEEN, *CONJ_ARGS_SEEN);
	if ($FULL_ARG eq '-compound') {
	    if (defined ($NEXT_ARG = &next_arg)) { $COMPOUND = $NEXT_ARG; }
	    else {
		&argument_error ("No argument given for option ${FULL_ARG}.",
				 *ARG_ARRAY);
	    }
	} elsif ($FULL_ARG eq '-subcompound') {
	    if (defined ($NEXT_ARG = &next_arg)) { $SUBCOMP = $NEXT_ARG; }
	    else {
		&argument_error ("No argument given for option ${FULL_ARG}.",
				 *ARG_ARRAY);
	    }
	} elsif ($FULL_ARG eq '-unit') {
	    if (defined ($NEXT_ARG = &next_arg)) { push (@UNITS, $NEXT_ARG); }
	    else {
		&argument_error ("No argument given for option ${FULL_ARG}.",
				 *ARG_ARRAY);
	    }
	} elsif ($FULL_ARG eq '-filename') {
	    if (defined ($NEXT_ARG = &next_arg)) { $FILENAME = $NEXT_ARG; }
	    else {
		&argument_error ("No argument given for option ${FULL_ARG}.",
				 *ARG_ARRAY);
	    }
	} elsif ($FULL_ARG eq '-and') {
	    &process_object ($COMPOUND, $SUBCOMP, $FILENAME, @UNITS);
	    undef ($SUBCOMP, $FILENAME, @UNITS);
	    &mark_array_not_seen (*LOCAL_ARGS_SEEN);
	    push (@HOPE_ARGS, $FULL_ARG);
	} elsif ($NEXT_ACTION == $TAKES_ARG) {
	    push (@HOPE_ARGS, $FULL_ARG);
	    if (defined ($NEXT_ARG = &next_arg)) { push (@HOPE_ARGS, $NEXT_ARG);}
	    else {
		&argument_error ("No argument given for option ${FULL_ARG}.",
				 *ARG_ARRAY);
	    }
	} else {
	    push (@HOPE_ARGS, $FULL_ARG);
	}
    }
    &process_object ($COMPOUND, $SUBCOMP, $FILENAME, @UNITS);
}

# Construct an assoc array whose keys are the legitimate options for
# this command.  The value for each key is a string with three parts
# and an optional fourth, separated by whitespace:
#   1. Full HOPE option name
#   2. Integer that indicates whether the option takes an argument
#   3. Integer indicating scope of the option (global, local, -and)
#   4. (Optional) Category that this option belongs to
#
# The fourth part is used for mutually exclusive options, like
# -recursive and -not-recursive.  An arglist can contain only one
# option from such a set.
#
# When the options to the HOPE checkout command change, this array must
# be updated.
#
sub fill_arg_array {
    local (*ARG_ARRAY) = @_;
    %ARG_ARRAY =
	(
	 '-&',                 "-and $NO_ARG $CONJ_ARG",
	 '-a',                 "-and $NO_ARG $CONJ_ARG",
	 '-and',               "-and $NO_ARG $CONJ_ARG",
	 '-apple-single',      "-apple-single $NO_ARG $LOCAL_ARG",
	 '-b',                 "-branch $TAKES_ARG $LOCAL_ARG",
	 '-binary',            "-binary $NO_ARG $LOCAL_ARG",
	 '-branch',            "-branch $TAKES_ARG $LOCAL_ARG",
	 '-bug-number',        "-bug-number $TAKES_ARG $LOCAL_ARG",
	 '-c',                 "-compound $TAKES_ARG $LOCAL_ARG",
	 '-comment',           "-comment $TAKES_ARG $LOCAL_ARG",
 	 '-compound',          "-compound $TAKES_ARG $LOCAL_ARG",
	 '-directory',         "-directory $TAKES_ARG $LOCAL_ARG",
	 '-filedate',          "-filedate $NO_ARG $LOCAL_ARG",
	 '-filename',          "-filename $TAKES_ARG $LOCAL_ARG",
	 '-fn',                "-filename $TAKES_ARG $LOCAL_ARG",
	 '-help',              "-help $TAKES_ARG $GLOBAL_ARG",
	 '-mac-creator',       "-mac-creator $TAKES_ARG $LOCAL_ARG",
	 '-mac-type',          "-mac-type $TAKES_ARG $LOCAL_ARG",
	 '-mode',              "-mode $TAKES_ARG $LOCAL_ARG",
	 '-not-apple-single',  "-not-apple-single $NO_ARG $LOCAL_ARG -apple-single",
	 '-not-binary',        "-not-binary $NO_ARG $LOCAL_ARG -binary",
	 '-not-filedate',      "-not-filedate $NO_ARG $LOCAL_ARG -filedate",
	 '-not-versioned',     "-not-versioned $NO_ARG $GLOBAL_ARG -versioned",
	 '-prompt',            "-prompt $NO_ARG $GLOBAL_ARG",   # not HOPE arg
	 '-rcs',               "-rcs $NO_ARG $GLOBAL_ARG",
	 '-reason',            "-reason $TAKES_ARG $LOCAL_ARG",
	 '-subbranch',         "-subbranch $TAKES_ARG $LOCAL_ARG",
	 '-subcompound',       "-subcompound $TAKES_ARG $LOCAL_ARG",
	 '-subversion',        "-subbranch $TAKES_ARG $LOCAL_ARG",
	 '-tip',               "-tip $NO_ARG $GLOBAL_ARG",
	 '-u',                 "-unit $TAKES_ARG $LOCAL_ARG",
	 '-unit',              "-unit $TAKES_ARG $LOCAL_ARG",
	 '-v',                 "-branch $TAKES_ARG $LOCAL_ARG",
	 '-version',           "-branch $TAKES_ARG $LOCAL_ARG",
	 '-versioned',         "-versioned $NO_ARG $GLOBAL_ARG",
	 );
}

# Initialize an assoc array used to check for duplicate options.  Each
# key is the full name of a global option.  Each value is an integer
# indicating whether or not this option has already appeared in the
# argument list.  We use this to check for duplicate options.  Each key
# is actually an option "category", so mutually exclusive options appear
# only once.  For example, we record appearances of both -recursive and
# -non-recursive under the same category, since only one of these is
# allowed.  The category is the fourth part of the value of %ARG_ARRAY
# if provided, or else the full option name.
#
sub fill_global_args_seen {
    local (*GLOBAL_ARGS_SEEN) = @_;
    %GLOBAL_ARGS_SEEN =
	(
	 '-help',              $ARG_NOT_SEEN,
	 '-prompt',            $ARG_NOT_SEEN,   # not HOPE arg
	 '-rcs',               $ARG_NOT_SEEN,
	 '-tip',               $ARG_NOT_SEEN,
	 '-versioned',         $ARG_NOT_SEEN,
	 );
}

# Initialize an assoc array used to check for duplicate options.  Each
# key is the full name of a local option.  Each value is an integer
# indicating whether or not this option has already appeared in the
# argument list.  We use this to check for duplicate options.  Each key
# is actually an option "category", so mutually exclusive options appear
# only once.  For example, we record appearances of both -recursive and
# -non-recursive under the same category, since only one of these is
# allowed.  The category is the fourth part of the value of %ARG_ARRAY
# if provided, or else the full option name.
#
sub fill_local_args_seen {
    local (*LOCAL_ARGS_SEEN) = @_;
    %LOCAL_ARGS_SEEN =
	(
	 '-apple-single',      $ARG_NOT_SEEN,
	 '-binary',            $ARG_NOT_SEEN,
	 '-branch',            $ARG_NOT_SEEN,
	 '-bug-number',        $ARG_NOT_SEEN,
	 '-comment',           $ARG_NOT_SEEN,
	 '-compound',          $ARG_NOT_SEEN,
	 '-directory',         $ARG_NOT_SEEN,
	 '-filedate',          $ARG_NOT_SEEN,
	 '-filename',          $ARG_NOT_SEEN,
	 '-mac-creator',       $ARG_NOT_SEEN,
	 '-mac-type',          $ARG_NOT_SEEN,
	 '-mode',              $ARG_NOT_SEEN,
	 '-reason',            $ARG_NOT_SEEN,
	 '-subbranch',         $ARG_NOT_SEEN,
	 '-subcompound',       $ARG_NOT_SEEN,
	 '-unit',              $ARG_NOT_SEEN,
	 );
}

# Initialize an assoc array used to check for duplicate options.  Each
# key is the full name of a conjunction.  Each value is an integer
# indicating whether or not this option has already appeared in the
# argument list.  We use this to check for duplicate options.  Each key
# is actually an option "category", so mutually exclusive options appear
# only once.  For example, we record appearances of both -recursive and
# -non-recursive under the same category, since only one of these is
# allowed.  The category is the fourth part of the value of %ARG_ARRAY
# if provided, or else the full option name.
#
# Note that the initial value indicates that the option has already
# appeared in the argument list.  This is because HOPE does not allow an
# argument list to begin with -and.  If the first argument is anything
# else, we reset the value to indicate "not seen".
#
sub fill_conj_args_seen {
    local (*CONJ_ARGS_SEEN) = @_;
    %CONJ_ARGS_SEEN =
	(
	 '-and',               $ARG_SEEN,
	 );
}

# Given an assoc array whose keys are option names, set the value for
# each key to indicate that the option has not yet appeared in the
# argument list.  We call this function to "reset" an array used to
# check for duplicate options.  For instance, if we encounter -and, we
# expect to start a new "object" (compound-unit) clause, so we reset the
# array of local options.
#
sub mark_array_not_seen {
    local (*ARRAY) = @_;
    foreach $KEY (keys (%ARRAY)) {
	$ARRAY{$KEY} = $ARG_NOT_SEEN;
    }
}

# Scan the argument list looking for a -help option.  If we find one, we
# just shove all the arguments into @HOPE_ARGS and skip further
# processing of the arguments, because the command is essentially a
# no-op.  Note that -help cannot be abbreviated in HOPE.
#
# This routine returns an integer that indicates whether or not -help
# appears in the command argument list.
#
sub scan_arglist_for_help {
    local ($HELP_SEEN) = 0;
    scan: foreach $ARG (@ARGV) {
	if ($ARG eq '-help') {
	    $HELP_SEEN = 1;
	    last scan;
	}
    }
    if ($HELP_SEEN) { @HOPE_ARGS = @ARGV; }
    $HELP_SEEN;
}

# Match an option supplied on the command line with the list of
# legitimate options for the command.  If the command-line arg
# matches a recognized option exactly, use that option.
# If not, check whether the command-line arg is an initial substring for
# a recognized option.  There are three possibilities:
#
#   The arg matches exactly one option: use that option.
#   The arg matches no options: die.
#   The arg matches more than one option: show matches and die.
#
# This routine also checks for duplicate options.  If it finds a
# duplicate, it prints an error message and dies.  To match HOPE, we do
# not allow initial, final, or repeated uses of -and.  Note that, unlike
# HOPE, we allow multiple occurrences of -unit.
#
# This routine returns two values: the full name of the matching option,
# and an integer indicating whether or not the option itself takes an
# argument.
#
sub match_arg {
    local ($ARG, *ARG_ARRAY, *GLOBAL_ARGS_SEEN, *LOCAL_ARGS_SEEN,
           *CONJ_ARGS_SEEN) = @_;
    local ($FULL_ARG, $NEXT_ACTION, $ARG_SCOPE, $ARG_CATEGORY);
    local ($ARG_PAT);
    local (@MATCHES, $MATCH, $i);
    if ($ARG !~ /^-/) {
	$MATCH = '-unit';
	unshift (@ARGV, $ARG);
    } else {
	($ARG_PAT = $ARG) =~ s/(\W)/\\$1/g;
	$ARG_PAT = "^${ARG_PAT}";
      argmatch: foreach $KEY (keys (%ARG_ARRAY)) {
	  if ($KEY =~ $ARG_PAT) {
	      if ($KEY eq $ARG) {
		  $MATCH = $KEY;
		  last argmatch;
	      }
	      else { push (@MATCHES, $KEY); }
	  }
      }
    }
    if (! $MATCH) {
	if (! @MATCHES) {
	    &argument_error ("Unrecognized option ${ARG}.", *ARG_ARRAY);
	} elsif (@MATCHES == 1) { $MATCH = $MATCHES[0]; }
	else {
	    $ERR_STRING = "Multiple completions for option ${ARG}:";
	    @MATCHES = sort (@MATCHES);
	    for ($i = 0; $i < @MATCHES; $i++) {
		$ERR_STRING = "$ERR_STRING $MATCHES[$i]";
		if ((@MATCHES > 2) && ($i < (@MATCHES - 1))) {
		    $ERR_STRING = "${ERR_STRING},";
		}
		if ($i == (@MATCHES - 2)) { $ERR_STRING = "$ERR_STRING and"; }
	    }
	    $ERR_STRING = "${ERR_STRING}.";
	    &argument_error ($ERR_STRING, *ARG_ARRAY);
	}
    }
    ($FULL_ARG, $NEXT_ACTION, $ARG_SCOPE, $ARG_CATEGORY) = 
	split (' ', $ARG_ARRAY{$MATCH});
    $ARG_CATEGORY = $ARG_CATEGORY || $FULL_ARG;
    if ($ARG_SCOPE == $GLOBAL_ARG) {
	if ($GLOBAL_ARGS_SEEN{$ARG_CATEGORY} == $ARG_SEEN) {
	    &argument_error ("Multiple use of global option $ARG_CATEGORY.",
			     *ARG_ARRAY);
	} else {
	    $GLOBAL_ARGS_SEEN{$ARG_CATEGORY} = $ARG_SEEN;
	    &mark_array_not_seen (*CONJ_ARGS_SEEN);
	}
    } elsif ($ARG_SCOPE == $LOCAL_ARG) {
	if (($LOCAL_ARGS_SEEN{$ARG_CATEGORY} == $ARG_SEEN) &&
	    (! ($FULL_ARG eq '-unit'))) {
	    &argument_error ("Multiple use of option $ARG_CATEGORY.",
			     *ARG_ARRAY);
	} else {
	    $LOCAL_ARGS_SEEN{$ARG_CATEGORY} = $ARG_SEEN;
	    &mark_array_not_seen (*CONJ_ARGS_SEEN);
	}
    } elsif ($ARG_SCOPE == $CONJ_ARG) {
	if (($CONJ_ARGS_SEEN{$ARG_CATEGORY} == $ARG_SEEN) || (! @ARGV)) {
	    &argument_error
		("Repeated, initial, or final use of option $ARG_CATEGORY.",
		 *ARG_ARRAY);
	} else { $CONJ_ARGS_SEEN{$ARG_CATEGORY} = $ARG_SEEN; }
    }
    ($FULL_ARG, $NEXT_ACTION);
}

# Return next argument in the arglist if it exists and does not begin
# with "-".  Otherwise, return undef.
#
sub next_arg {
    local ($NEXT_ARG);
    if (@ARGV) {
	$NEXT_ARG = shift (@ARGV);
	if ($NEXT_ARG =~ /^-/) {
	    unshift (@ARGV, $NEXT_ARG);
	    undef;
	} else { $NEXT_ARG; }
    } else { undef; }
}

# Handle an error in the argument list.
# Print error message, show valid options, and die.
#
sub argument_error {
    local ($ERR_STRING, *ARG_ARRAY) = @_;
    &print_error ($ERR_STRING);
    &show_options (*ARG_ARRAY);
    exit (1);
}

# Show valid options for this command.
#
sub show_options {
    local (*ARG_ARRAY) = @_;
    local (@OPTIONS) = sort (keys (%ARG_ARRAY));
    local ($FULL_ARG, $NEXT_ACTION);
    if (@OPTIONS) {
	print "Recognized options for this command:\n";
	foreach $OPTION (@OPTIONS) {
	    ($FULL_ARG, $NEXT_ACTION) = split (' ', $ARG_ARRAY{$OPTION});
	    print " ${OPTION}";
	    if ($NEXT_ACTION == $TAKES_ARG) { print " <argument>"; }
	    print "\n";
	}
    } else { print "No recognized options for this command.\n"; }
}

# Process an object (compound plus subcompound or units).
#   Add to @HOPE_ARGS.
#
sub process_object {
    local ($COMPOUND, $SUBCOMP, $FILENAME, @UNITS) = @_;
    &add_object_to_hope_args ($COMPOUND, $SUBCOMP, $FILENAME, @UNITS);
    $COMPOUND;
}

# Add the current object spec (compound plus optional units plus
# optional filename) to @HOPE_ARGS.
#
# Each HOPE clause can have either -subcompound or units, but not
# both.  If the user has mixed a -subcompound with units, we
# put -subcompound into @HOPE_ARGS ahead of the units.
#
# If there is a filename and there are multiple units, we repeat the
# filename for each unit, since -filename in HOPE does not persist
# across -and.  We are making a big assumption here that that was the
# user's intention.
#
# Note that -subcompound and -subbranch also do not persist across
# -and, but: (a) We allow only one -subcompound for each object;
# (b) if there is a -subcompound, we put it into @HOPE_ARGS first,
# before any units; (c) -subbranch makes sense only with
# -subcompound; (d) if there is a -subbranch, it has already been
# added to @HOPE_ARGS before this routine is called.  Thus,
# -subcompound and -subbranch (if any) always appear in the same
# HOPE clause, and we do not have to repeat either.
#
sub add_object_to_hope_args {
    local ($COMPOUND, $SUBCOMP, $FILENAME, @UNITS) = @_;
    local ($i);
    if ($COMPOUND)   { push (@HOPE_ARGS, "-compound", $COMPOUND); }
    if ($SUBCOMP)    { push (@HOPE_ARGS, "-subcompound", $SUBCOMP); }
    if ($FILENAME)   { push (@HOPE_ARGS, "-filename", $FILENAME); }
    if ($SUBCOMP && @UNITS) {
	push (@HOPE_ARGS, "-and");
	if ($FILENAME) { push (@HOPE_ARGS, "-filename", $FILENAME); }
    }
    for ($i = 0; $i < @UNITS; $i++) {
	$UNIT = $UNITS[$i];
	push (@HOPE_ARGS, "-unit", $UNIT);
	if ($i < (@UNITS - 1)) {
	    push (@HOPE_ARGS, "-and");
	    if ($FILENAME) { push (@HOPE_ARGS, "-filename", $FILENAME); }
	}
    }
}

# Print the command line for the HOPE command that we are about to
# execute.
#
sub print_hope_command {
    local ($CMD) = @_;
    print "Executing the following HOPE command:\n";
    print "@HOPE_CMD $CMD";
    foreach $ARG (@HOPE_ARGS) {
	print ' ';
	if ($ARG =~ /\s/) { print "'", $ARG, "'"; }
	else { print $ARG; }
    }
    print "\n";
}

# Print an error message to STDERR.
#
sub print_error {
    local ($ERR_STRING) = @_;
    print STDERR "Error: ${ERR_STRING}\n";
}

# Print a warning message to STDERR.
#
sub print_warning {
    local ($ERR_STRING) = @_;
    print STDERR "Warning: ${ERR_STRING}\n";
}

# Do it!
#
&main;
