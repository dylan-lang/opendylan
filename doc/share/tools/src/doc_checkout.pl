#! /usr/local/bin/perl

# Script to check doc files out of HOPE, uncompressing checked-out files
# when necessary.  The idea is to let the user have all the power of the
# HOPE checkout command and also to be smart about when to uncompress or
# not to uncompress a checked-out file.
#
# Arguments are generally the same as those to the HOPE checkout command.
# The script parses the arguments.  When it detects the end of an object
# specification -- i.e., it encounters an -and or the end of the arglist
# -- it records the directory in which the checkout will happen.  If any
# units are specified on the command line and if any of these does not
# end with a suffix known to represent a compressed file, the script
# tries to figure out whether the unit argument corresponds to an actual
# unit name that does represent a compressed file -- for instance, if
# the argument is "foo.doc" and the actual unit is "foo.doc.gz".  In
# this case the script arranges for HOPE to check out foo.doc.gz.  After
# the checkout is complete, the script searches for checked-out files
# that might need to be uncompressed, and it uncompresses them.
#
# Two differences between the script's arguments and the HOPE command's
# arguments:
#  (a) Because of the script's heuristics for unit names, the user can
#      give arguments derived from file names.  Thus, you can use file
#      globbing -- e.g., you can say "doc_checkout -comp foo *.doc".
#      You don't have to put -and between unit names.
#  (b) The script has a -prompt option.  If you give this, the script
#      prompts before uncompressing any files and before overwriting a
#      read-only file when uncompressing.
#      Note: in the absence of -prompt, the script DOES overwrite a
#      read-only file when uncompressing.  It always prompts before
#      overwriting a writable file.
#
# The script allows recursive checkout of compounds and uncompresses the
# necessary files in subcompounds.
#
# If the user checks out an entire compound and gives a -filename
# argument, that argument is used as the directory in which the checkout
# takes place.
#
# Suppose the user has said -unit foo.doc and there exist both foo.doc
# and foo.doc.gz units?  In this case, the script checks out foo.doc.
# This situation is pathological.
#
# The script generally uses HOPE's heuristics for finding the compound
# to use if none is specified.  However, the environment variable
# DEFAULT_COMPOUND can contain the name of a compound to use if none is
# given on the command line.
#
# The script actually can deal with file types other than ".gz" for
# compressed files.  The environment variable ZIP_TYPES contains a
# colon-separated list of compressed file types to look for.  Note that
# when a file is uncompressed, the file name is usually formed by
# removing the suffix.  Thus, ".gz" is different from "gz": If the
# suffix is ".gz", "foo.doc.gz" yields "foo.doc"; but if the suffix is
# "gz", "foo.doc.gz" yields "foo.doc." (note trailing period).
#
# To get around this limitation, any of the colon-separated entries in
# ZIP_TYPES can be of the form "<zip-type>=<unzip-type>" -- for
# instance, ".doc.gz=.doc" or ".dgz=.doc".  The presence of "="
# indicates that the given zip-type is mapped to the given unzip-type.
# Suppose the entry ".dgz=.doc" appears in ZIP_TYPES.  If the
# nonexistent unit foo.doc is specified on the command line, the script
# looks for the unit foo.dgz to check out.  After the checkout, the
# script looks for newly checked-out files ending in .dgz to uncompress
# (yielding, for instance, the uncompressed file foo.doc from foo.dgz).
#
# One limitation on ZIP_TYPES: each compressed-file suffix can appear
# only once in the list.  One suffix can be a terminal substring of
# another; for instance, it's OK to list both ".gz" and "gz".  The
# script takes care of ordering these to search for the most specific
# suffix first.  It is also OK to map more than one compressed-file
# suffix to the same uncompressed-file suffix; for instance, you could
# have ".doc.gz=.doc:.dgz=.doc".
#
# Other environment variables can specify the commands that the script
# uses internally.  The values can have more than one word; for example,
# you could set GUNZIP_CMD to "gzip -d" instead of "gunzip" or set
# HOPE_CMD to "hope verbose" instead of "hope".  Following are the
# environment variables that specify commands:
#   HOPE_CMD     default "hope"
#   GUNZIP_CMD   default "gunzip"
#   TOUCH_CMD    default "touch"
#   FIND_CMD     default "find"
#   RM_CMD       default "rm"
#   MV_CMD       default "mv"
#   PWD_CMD      default "pwd"

# Main routine.  Sequence of events:
#   Initialize variables
#   Parse argument list
#   Touch a timestamp file
#   Execute HOPE checkout command
#   Uncompress files, if any
#
# Some global variables bound here, in addition to those set from
# environment variables:
#   @HOPE_ARGS     Array of arguments to HOPE checkout command
#   %DIRS          Assoc array of directories and timestamp files
#   $PROMPT        Flag for prompting when uncompressing files 
#   @ZIP_TYPES     Array of suffixes for compressed files
#   %ZIP_TYPES     Assoc array of zip suffixes and unzip suffixes
#   $ZIP_TYPE_PAT  Search pattern for finding compressed-file suffixes
#   $TMP_DIR       Name of /tmp directory
#   $TS_FILE       Name of timestamp file
#
sub main {
    local (@HOPE_ARGS, %DIRS, $PROMPT);
    local (@ZIP_TYPES, %ZIP_TYPES, $ZIP_TYPE_PAT);
    local ($TMP_DIR) = "/tmp";
    local ($TS_FILE) = ".co_$$";
    local ($DEFAULT_COMPOUND) = $ENV{"DEFAULT_COMPOUND"};
    local (@HOPE_CMD) = split (' ', ($ENV{"HOPE_CMD"} || "hope"));
    local (@GUNZIP_CMD) = split (' ', ($ENV{"GUNZIP_CMD"} || "gunzip"));
    local (@TOUCH_CMD) = split (' ', ($ENV{"TOUCH_CMD"} || "touch"));
    local (@FIND_CMD) = split (' ', ($ENV{"FIND_CMD"} || "find"));
    local (@RM_CMD) = split (' ', ($ENV{"RM_CMD"} || "rm"));
    local (@MV_CMD) = split (' ', ($ENV{"MV_CMD"} || "mv"));
    local (@PWD_CMD) = split (' ', ($ENV{"PWD_CMD"} || "pwd"));
    local ($ZIP_TYPES) = $ENV{"ZIP_TYPES"} ||
	".gz:.dgz=.doc:.bgz=.bk:.xgz=.xwd:.fgz=.tif:.cgz=.pcx:.pgz=.ps";
    $|=1;
    &process_zip_types;
    &process_arglist;
    &start_checkout;
    &print_hope_command ("checkout");
    system (@HOPE_CMD, "checkout", @HOPE_ARGS);
    &end_checkout;
}

# Start with the colon-separated list of compressed-file suffixes in
# $ZIP_TYPES, split these into the @ZIP_TYPES array, and generate the
# search pattern $ZIP_TYPE_PAT from the array.  We order the array so
# that more specific suffixes precede the less specific.  For example,
# we want ".gz" to precede "gz".  Otherwise, we would generate incorrect
# file names when uncompressing -- "foo.doc.gz" would yield "foo.doc."
# (with trailing period) instead of "foo.doc".
# Also, add each compressed-file suffix as a key in the %ZIP_TYPES assoc
# array.  The value is the associated uncompressed-file suffix, if any,
# which follows an equals sign ("=") in the $ZIP_TYPES entry.
#
sub process_zip_types {
    local (@Z_TYPES, $ZI_PAT, $TEMP, $i, $j);
    @Z_TYPES = split (':', $ZIP_TYPES);
    foreach $TYPE (@Z_TYPES) {
	$TYPE =~ /\s*([^=\s]*)\s*=*\s*([^=]*)\s*/;
	if ($1) {
	    push (@ZIP_TYPES, $1);
	    $ZIP_TYPES{$1} = $2;
	}
    }
    for ($i = 0; $i < @ZIP_TYPES - 1; $i++) {
	for ($j = $i + 1; $j < @ZIP_TYPES; $j++) {
	    if ($ZIP_TYPES[$i] eq $ZIP_TYPES[$j]) {
		splice (@ZIP_TYPES, $j, 1);
	    } else {
		($ZI_PAT = $ZIP_TYPES[$i]) =~ s/(\W)/\\$1/g;
		if ($ZIP_TYPES[$j] =~ /$ZI_PAT$/) {
		    $TEMP = $ZIP_TYPES[$i];
		    $ZIP_TYPES[$i] = $ZIP_TYPES[$j];
		    $ZIP_TYPES[$j] = $TEMP;
		}
	    }
	}
    }
    undef (@Z_TYPES);
    @Z_TYPES = @ZIP_TYPES;
    for ($i = 0; $i < @Z_TYPES; $i++) {
	$Z_TYPES[$i] =~ s/(\W)/\\$1/g;
	$Z_TYPES[$i] = "$Z_TYPES[$i]\$";
    }
    $ZIP_TYPE_PAT = join ('|', @Z_TYPES);
}

# Parse the command argument list.  Arguments are the same as those to
# the HOPE checkout command, with the addition of -prompt, meaning to
# prompt before uncompressing files.  Unfortunately, the list of
# arguments is guaranteed to get out of date as HOPE changes.
#
# We first scan the arglist looking for a -help option.  If found, we
# return right away, because the command is essentially a no-op.
#
# Arguments we don't care about are accumulated in the @HOPE_ARGS array.
# Arguments we do care about:
#   -compound                             Set $COMPOUND to next arg
#   -unit (or arg not beginning with -)   Push onto @UNITS
#   -filename                             Set $FILENAME to next arg
#   -until-date                           Set $UNTIL_DATE to next arg
#   -until-version                        Set $UNTIL_VERSION to next arg
#   -ci-date                              Warn about lack of automatic
#                                         uncompression
#
# At the end of the arglist, or if we find -and followed by more args,
# we process the current object (compound plus units).
#
# Some other global variables set or bound here:
#   $SELECTED_COMPOUND  Currently selected compound (in HOPE)
#   %COMPOUND_DIRS      Assoc array of compounds and directories
#   %COMPOUND_UNITS     Assoc array of compounds and units
#   %ARG_ARRAY          Assoc array of command options and information
#   %GLOBAL_ARGS_SEEN   Assoc array to keep track of global options seen
#   %LOCAL_ARGS_SEEN    Assoc array to keep track of local options seen
#   %CONJ_ARGS_SEEN     Assoc array to keep track of conjunctions seen
#   (various)           Constants with integer values
#
sub process_arglist {
    local ($COMPOUND, $FILENAME, $UNTIL_DATE, $UNTIL_VERSION, @UNITS);
    local ($SELECTED_COMPOUND);
    local (%COMPOUND_DIRS, %COMPOUND_UNITS);
    local ($ARG, $NEXT_ARG, $FULL_ARG, $NEXT_ACTION);
    local (%ARG_ARRAY, %GLOBAL_ARGS_SEEN, %LOCAL_ARGS_SEEN, %CONJ_ARGS_SEEN);
    local ($NO_ARG) = 0;
    local ($TAKES_ARG) = 1;
    local ($LOCAL_ARG) = 0;
    local ($GLOBAL_ARG) = 2;
    local ($CONJ_ARG) = 4;
    local ($ARG_NOT_SEEN) = 0;
    local ($ARG_SEEN) = 1;
    local ($SPEC_IS_COMPOUND) = 0;
    local ($SPEC_IS_UNIT) = 1;
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
	} elsif ($FULL_ARG eq '-until-date') {
	    if (defined ($NEXT_ARG = &next_arg)) { $UNTIL_DATE = $NEXT_ARG; }
	    else {
		&argument_error ("No argument given for option ${FULL_ARG}.",
				 *ARG_ARRAY);
	    }
	} elsif ($FULL_ARG eq '-until-version') {
	    if (defined ($NEXT_ARG = &next_arg)) { $UNTIL_VERSION = $NEXT_ARG; }
	    else {
		&argument_error ("No argument given for option ${FULL_ARG}.",
				 *ARG_ARRAY);
	    }
	} elsif ($FULL_ARG eq '-ci-date') {
	    &print_warning ("The -ci-date option is likely to prevent automatic uncompression of files.  You might have to uncompress checked-out files yourself.");
	    push (@HOPE_ARGS, $FULL_ARG);
	} elsif ($FULL_ARG eq '-prompt') {
	    $PROMPT = 1;
	} elsif ($FULL_ARG eq '-and') {
	    &process_object ($COMPOUND, $FILENAME, $UNTIL_DATE, $UNTIL_VERSION,
			     @UNITS);
	    undef ($FILENAME, $UNTIL_DATE, $UNTIL_VERSION, @UNITS);
	    &mark_array_not_seen (*LOCAL_ARGS_SEEN);
	    push (@HOPE_ARGS, $FULL_ARG);
	} elsif ($NEXT_ACTION == $TAKES_ARG) {
	    push (@HOPE_ARGS, $FULL_ARG);
	    if (defined ($NEXT_ARG = &next_arg)) {
		push (@HOPE_ARGS, $NEXT_ARG);
	    } else {
		&argument_error ("No argument given for option ${FULL_ARG}.",
				 *ARG_ARRAY);
	    }
	} else {
	    push (@HOPE_ARGS, $FULL_ARG);
	}
    }
    &process_object ($COMPOUND, $FILENAME, $UNTIL_DATE, $UNTIL_VERSION,
		     @UNITS);
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
	 '-b',                 "-branch $TAKES_ARG $LOCAL_ARG",
	 '-branch',            "-branch $TAKES_ARG $LOCAL_ARG",
	 '-bug-number',        "-bug-number $TAKES_ARG $LOCAL_ARG",
	 '-c',                 "-compound $TAKES_ARG $LOCAL_ARG",
	 '-ci-date',           "-ci-date $NO_ARG $LOCAL_ARG",
	 '-claim',             "-claim $TAKES_ARG $LOCAL_ARG",
	 '-compound',          "-compound $TAKES_ARG $LOCAL_ARG",
	 '-create',            "-create $NO_ARG $GLOBAL_ARG",
	 '-date',              "-date $TAKES_ARG $LOCAL_ARG",
	 '-diff-branch-files', "-diff-branch-files $TAKES_ARG $GLOBAL_ARG",
	 '-diff-unit-files',   "-diff-unit-files $TAKES_ARG $GLOBAL_ARG",
	 '-extra-files',       "-extra-files $TAKES_ARG $LOCAL_ARG",
	 '-filename',          "-filename $TAKES_ARG $LOCAL_ARG",
	 '-fn',                "-filename $TAKES_ARG $LOCAL_ARG",
#	 '-forced',            "-forced $NO_ARG $GLOBAL_ARG", # Removed in Hope 1.24
	 '-hard',              "-hard $NO_ARG $LOCAL_ARG -claim",
	 '-help',              "-help $TAKES_ARG $GLOBAL_ARG",
	 '-link',              "-link $NO_ARG $GLOBAL_ARG",
	 '-local',             "-local $NO_ARG $GLOBAL_ARG",
	 '-missing-dir',       "-missing-dir $TAKES_ARG $GLOBAL_ARG",
	 '-modified-files',    "-modified-files $TAKES_ARG $GLOBAL_ARG",
	 '-my-claim',          "-my-claim $TAKES_ARG $GLOBAL_ARG",
	 '-not-ci-date',       "-not-ci-date $NO_ARG $LOCAL_ARG -ci-date",
#	 '-not-forced',        "-not-forced $NO_ARG $GLOBAL_ARG -forced", # Removed in Hope 1.24
	 '-not-rcs',           "-not-rcs $NO_ARG $GLOBAL_ARG -rcs",
	 '-not-recursive',     "-not-recursive $NO_ARG $LOCAL_ARG -recursive",
	 '-other-claim',       "-other-claim $TAKES_ARG $GLOBAL_ARG",
	 '-prompt',            "-prompt $NO_ARG $GLOBAL_ARG",   # not HOPE arg
	 '-rcs',               "-rcs $NO_ARG $GLOBAL_ARG",
	 '-reason',            "-reason $TAKES_ARG $LOCAL_ARG",
	 '-recursive',         "-recursive $NO_ARG $LOCAL_ARG",
	 '-soft',              "-soft $NO_ARG $LOCAL_ARG -claim",
	 '-stale-unit-files',  "-stale-unit-files $TAKES_ARG $GLOBAL_ARG",
	 '-task',              "-task $TAKES_ARG $LOCAL_ARG",
	 '-u',                 "-unit $TAKES_ARG $LOCAL_ARG",
	 '-unit',              "-unit $TAKES_ARG $LOCAL_ARG",
	 '-unmodified-files',  "-unmodified-files $TAKES_ARG $GLOBAL_ARG",
	 '-until-date',        "-until-date $TAKES_ARG $LOCAL_ARG",
	 '-until-version',     "-until-version $TAKES_ARG $LOCAL_ARG",
	 '-user',              "-user $TAKES_ARG $LOCAL_ARG",
	 '-v',                 "-branch $TAKES_ARG $LOCAL_ARG",
	 '-version',           "-branch $TAKES_ARG $LOCAL_ARG",
	 '-working-files',     "-writable-files $TAKES_ARG $LOCAL_ARG",
	 '-writable-files',    "-writable-files $TAKES_ARG $LOCAL_ARG",
	 '-wrong-branch-files',"-diff-branch-files $TAKES_ARG $GLOBAL_ARG",
	 '-wrong-unit-files',  "-diff-unit-files $TAKES_ARG $GLOBAL_ARG",
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
	 '-create',            $ARG_NOT_SEEN,
	 '-diff-branch-files', $ARG_NOT_SEEN,
	 '-diff-unit-files',   $ARG_NOT_SEEN,
#	 '-forced',            $ARG_NOT_SEEN, # Removed in Hope 1.24
	 '-help',              $ARG_NOT_SEEN,
	 '-link',              $ARG_NOT_SEEN,
	 '-local',             $ARG_NOT_SEEN,
	 '-missing-dir',       $ARG_NOT_SEEN,
	 '-modified-files',    $ARG_NOT_SEEN,
	 '-my-claim',          $ARG_NOT_SEEN,
	 '-other-claim',       $ARG_NOT_SEEN,
	 '-prompt',            $ARG_NOT_SEEN, # not HOPE arg
	 '-rcs',               $ARG_NOT_SEEN,
	 '-stale-unit-files',  $ARG_NOT_SEEN,
	 '-unmodified-files',  $ARG_NOT_SEEN,
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
	 '-branch',            $ARG_NOT_SEEN,
	 '-bug-number',        $ARG_NOT_SEEN,
	 '-ci-date',           $ARG_NOT_SEEN,
	 '-claim',             $ARG_NOT_SEEN,
	 '-compound',          $ARG_NOT_SEEN,
	 '-date',              $ARG_NOT_SEEN,
	 '-extra-files',       $ARG_NOT_SEEN,
	 '-filename',          $ARG_NOT_SEEN,
	 '-reason',            $ARG_NOT_SEEN,
	 '-recursive',         $ARG_NOT_SEEN,
	 '-task',              $ARG_NOT_SEEN,
	 '-unit',              $ARG_NOT_SEEN,
	 '-until-date',        $ARG_NOT_SEEN,
	 '-until-version',     $ARG_NOT_SEEN,
	 '-user',              $ARG_NOT_SEEN,
	 '-writable-files',    $ARG_NOT_SEEN,
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
    local ($FULL_ARG, $NEXT_ACTION, $ARG_SCOPE, $ARG_CATEGORY, $ERR_STRING);
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

# Process an object (compound plus units).  Sequence of events:
#   Parse the "HOPE spec" that represents the compound (from -compound
#     or the default compound) into components representing compound and
#     unit.
#   If any units have been specified separately (via -unit or argument
#     not beginning with -), iterate over these units:
#       Parse the HOPE spec that represents the unit into components
#         representing compound and unit.
#       Check whether both -compound and -unit have unit components.
#         HOPE does not allow this; die.
#       Call a routine to fetch necessary information from HOPE, collect
#         the directory, look for corresponding units that represent
#         compressed files, and return the HOPE spec that represents the
#         unit, possibly with a new unit component.
#       Replace this element of the @UNITS array with the HOPE spec
#         returned by the subroutine.
#   If no units have been specified separately (via -unit or argument
#     not beginning with -), use the HOPE spec that represents the
#     compound (from -compound or the default compound):
#       Call a routine to fetch necessary information from HOPE, collect
#         the directory, look for corresponding units that represent
#         compressed files, and return the HOPE spec that represents the
#         compound, possibly with a new unit component.
#       Replace the spec that represents the compound with the HOPE spec
#         returned by the subroutine.
#   Add to @HOPE_ARGS.
#
sub process_object {
    local ($COMPOUND, $FILENAME, $UNTIL_DATE, $UNTIL_VERSION, @UNITS) = @_;
    local ($UNIT, $i);
    local ($UNIT_COMPOUND, $UNIT_UNIT, $UNIT_BRANCH);
    local ($COMP_COMPOUND, $COMP_UNIT, $COMP_BRANCH) =
	&parse_hopespec($COMPOUND, $SPEC_IS_COMPOUND);
    for ($i = 0; $i < @UNITS; $i++) {
	$UNIT = $UNITS[$i];
	($UNIT_COMPOUND, $UNIT_UNIT, $UNIT_BRANCH) =
	    &parse_hopespec($UNIT, $SPEC_IS_UNIT);
# Note: In HOPE version 1.23, specifying a unit in both -c and -u is
# an error.  This might change in the future.  If so, remove the
# following if clause.
	if ($COMP_UNIT && $UNIT_UNIT) {
	    &hope_spec_error
		("Unit specified in both -compound ($COMPOUND) and -unit ($UNIT).");
	    }
	$UNITS[$i] = &match_spec ($UNIT, $FILENAME, $COMP_COMPOUND, $COMP_UNIT,
				  $COMP_BRANCH, $UNIT_COMPOUND, $UNIT_UNIT,
				  $UNIT_BRANCH);
    }
    if (! @UNITS) {
	$COMPOUND = &match_spec ($COMPOUND, $FILENAME, $COMP_COMPOUND,
				 $COMP_UNIT, $COMP_BRANCH, $UNIT_COMPOUND,
				 $UNIT_UNIT, $UNIT_BRANCH);
    }
    &add_object_to_hope_args ($COMPOUND, $FILENAME, $UNTIL_DATE,
			      $UNTIL_VERSION, @UNITS);
    $COMPOUND;
}

# Parse a "HOPE spec" into compound, unit, and branch components and
# return the components.  The arguments are the spec and an integer
# indicating whether the spec represents a compound (from -compound or
# the default compound) or a unit (from -unit).
#
# A HOPE spec can have up to three components, with ! ( ) as separator
# characters, in the following format:
#   compound ! unit ( branch )
#
# Either ! or () or both can be missing.  An initial ! means that the
# compound is the currently selected compound (or, for our script, the
# value of the DEFAULT_COMPOUND environment variable).  If the spec
# contains no ! then the characters preceding the branch portion
# represent the compound component if this spec comes from -compound
# or the default compound, or the unit component if this spec comes
# from -unit.
#
# We make a distinction between *empty* components, for which we return
# an empty string, and *missing* components, for which we return undef.
# This is important for the compound component, where an explicit but
# empty component means to use the default (or selected) compound, but a
# missing component means to use the "prevailing" compound (from the
# last -compound, if any, or else the default compound).  Internally, we
# use a distinguished value (that cannot appear in any component) to
# represent "undefined" so that we don't accidentally cause an undefined
# variable to become defined; in perl you can sometimes do this simply
# by referencing such a variable.
#
sub parse_hopespec {
    local ($SPEC, $SPEC_TYPE) = @_;
    local ($UNDEFINED) = '()';
    local ($COMPOUND) = $UNDEFINED;
    local ($UNIT) = $UNDEFINED;
    local ($BRANCH) = $UNDEFINED;
# Case 1: spec has both ! and ()
    if ($SPEC =~ /^([^\!\(\)]*)\!([^\!\(\)]*)\(([^\!\(\)]*)\)$/) {
	$COMPOUND = $1;
	$UNIT = $2;
	$BRANCH = $3;
# Case 2: spec has ! but no ()
    } elsif ($SPEC =~ /^([^\!\(\)]*)\!([^\!\(\)]*)$/) {
	$COMPOUND = $1;
	$UNIT = $2;
# Case 3: spec has () but no !
    } elsif ($SPEC =~ /^([^\!\(\)]*)\(([^\!\(\)]*)\)$/) {
	if ($SPEC_TYPE == $SPEC_IS_COMPOUND) { $COMPOUND = $1; }
	elsif ($SPEC_TYPE == $SPEC_IS_UNIT) { $UNIT = $1; }
	$BRANCH = $2;
# Case 4: spec has neither ! nor ()
    } elsif ($SPEC =~ /^([^\!\(\)]*)$/) {
	if ($SPEC_TYPE == $SPEC_IS_COMPOUND) { $COMPOUND = $1; }
	elsif ($SPEC_TYPE == $SPEC_IS_UNIT) { $UNIT = $1; }
    } else {
	if ($SPEC_TYPE == $SPEC_IS_COMPOUND) {
	    &hope_spec_error
		("Malformed HOPE spec representing a compound: $SPEC.");
        } elsif ($SPEC_TYPE == $SPEC_IS_UNIT) {
	    &hope_spec_error
		("Malformed HOPE spec representing a unit: $SPEC.");
	} else {
	    &hope_spec_error
		("Malformed HOPE spec representing an unknown component: $SPEC.");
	}
    }
    ($COMPOUND eq $UNDEFINED ? undef : $COMPOUND, 
     $UNIT eq $UNDEFINED ? undef : $UNIT, 
     $BRANCH eq $UNDEFINED ? undef : $BRANCH);
}

# For a given "HOPE spec" that represents a compound or unit, fetch
# necessary information from HOPE, collect the directory, look for
# corresponding units that represent compressed files, and return the
# HOPE spec that represents the compound or unit, possibly with a new
# unit component.  Sequence of events:
#   Find the compound.  If this spec represents a unit and has a
#     compound component, use that (even if the compound component is
#     the empty string; this means to use the default compound).
#     Otherwise, use the compound from -compound or the default
#     compound.  If we can't find the compound, just return.
#   Find the unit.  If this spec represents a unit and has a unit
#     component, use that; otherwise, use the unit component (if any)
#     from -compound.  The unit can be missing; in that case the
#     command operates on the entire compound.
#   If there is a unit and -filename is provided, collect the filename's
#     directory in %DIRS; otherwise, find and collect the directory for
#     the compound.
#   If there is a unit and it does not end in a compressed-file type,
#     we need to look for corresponding units that represent compressed
#     files.  If we find such a unit, we may need to use it instead of
#     the unit component from the command line.  In that case, we
#     substitute the new unit component for the existing unit
#     component of the HOPE spec.
#   Return the HOPE spec, possibly with a new unit component.
#
sub match_spec {
    local ($SPEC, $FILENAME,
	   $COMP_COMPOUND, $COMP_UNIT, $COMP_BRANCH,
	   $UNIT_COMPOUND, $UNIT_UNIT, $UNIT_BRANCH) = @_;
    local ($THIS_COMP, $THIS_UNIT, $NEW_UNIT);
    if (defined ($UNIT_COMPOUND)) {
	$THIS_COMP = &find_compound ($UNIT_COMPOUND);
    } else { $THIS_COMP = &find_compound ($COMP_COMPOUND); }
    if ($THIS_COMP) {
	$THIS_UNIT = $UNIT_UNIT || $COMP_UNIT;
	if ($FILENAME && $THIS_UNIT) { &collect_file_dir ($FILENAME); }
	else {
	    &collect_dir (&find_dir_for_compound ($THIS_COMP, $THIS_UNIT,
						  $FILENAME));
	}
	if ($THIS_UNIT && ($THIS_UNIT !~ /$ZIP_TYPE_PAT/)) {
	    &get_units_for_compound ($THIS_COMP);
	    $NEW_UNIT = &match_units_for_compound ($THIS_COMP, $THIS_UNIT);
	    if ($NEW_UNIT && ($NEW_UNIT ne $THIS_UNIT)) {
		$SPEC = &substitute_unit ($SPEC, $NEW_UNIT);
	    }
	}
    }
    $SPEC;
}

# Given a "HOPE spec" and a new unit component, substitute the new unit
# for the existing unit component of the HOPE spec and return the new
# HOPE spec.
#
sub substitute_unit {
    local ($SPEC, $NEW_UNIT) = @_;
    local ($NEW_SPEC) = $SPEC;
# Case 1: spec has both ! and ()
    ($NEW_SPEC =~ s/^([^\!\(\)]*\!)[^\!\(\)]*(\([^\!\(\)]*\))$/$1$NEW_UNIT$2/) ||
# Case 2: spec has ! but no ()
	($NEW_SPEC =~ s/^([^\!\(\)]*\!)[^\!\(\)]*$/$1$NEW_UNIT/) ||
# Case 3: spec has () but no !
	    ($NEW_SPEC =~ s/^[^\!\(\)]*(\([^\!\(\)]*\))$/$NEW_UNIT$1/) ||
# Case 4: spec has neither ! nor ()
		($NEW_SPEC =~ s/^[^\!\(\)]*$/$NEW_UNIT/) ||
		    (&hope_spec_error ("Malformed HOPE spec: $SPEC."));
    $NEW_SPEC;
}

# Handle an error in a HOPE spec.
# Print error message and die.
#
sub hope_spec_error {
    local ($ERR_STRING) = @_;
    &print_error ($ERR_STRING);
    exit (1);
}

# Get the compound for this object from:
#   Command line (compound component of -compound or -unit "HOPE spec")
#   Otherwise, DEFAULT_COMPOUND environment variable (this is not in
#     HOPE)
#   Otherwise, currently selected compound in HOPE (and cache the
#     compound in the global variable $SELECTED_COMPOUND)
#
# Once found, the compound remains the same until changed by a
# subsequent -compound argument, unless the compound comes from the
# compound component of -unit; in that case it applies only to that
# unit and does not become the prevailing compound.
#
sub find_compound {
    local ($COMPOUND) = @_;
    local ($RETVAL) = '';
    $COMPOUND || $DEFAULT_COMPOUND || do {
	if (defined ($SELECTED_COMPOUND)) { $SELECTED_COMPOUND }
	else {
	    $SELECTED_COMPOUND = '';
	    if (open (HOPEIN, "@HOPE_CMD select |")) {
		print "Getting currently selected compound from HOPE ...\n";
	      hopein: while (<HOPEIN>) {
		  if (/^\s*Compound\s*=\s*\`*([^\s\']*)/) {
		      $SELECTED_COMPOUND = $1;
		      last hopein;
		  }
	      }
		close (HOPEIN);
		if (!$SELECTED_COMPOUND) {
		    print "Cannot find currently selected compound.\n";
		} elsif ($SELECTED_COMPOUND eq 'None') {
		    print "No compound is currently selected.\n";
		    $SELECTED_COMPOUND = '';
		}
	    } else {
		print "Cannot get currently selected compound from HOPE.\n";
	    }
	    $SELECTED_COMPOUND;
	}
    }
}

# Find and return the directory for a compound.  Sequence of events:
#   If there is a -filename argument and we're checking out an entire
#     compound, the filename names the compound's directory.
#   Otherwise, we get the compound's directory from HOPE and follow
#     HOPE's directory-finding rules: If the directory matches the name
#     of the current directory, use the current directory; otherwise, if
#     the directory matches a subdirectory of the current directory, use
#     that; otherwise, fail.
# The return value is a real directory, such as "." or "./src".
#
sub find_dir_for_compound {
    local ($COMPOUND, $UNIT, $FILENAME) = @_;
    local ($COMP_DIR);
    if ($FILENAME && (! $UNIT)) {
	$FILENAME = &canonicalize_pathname ($FILENAME);
	if (! (-d $FILENAME)) {
	    print "Cannot find directory $FILENAME for compound $COMPOUND.\n";
	}
	$FILENAME;
    } elsif ($COMPOUND) {
	$COMP_DIR = &get_dir_for_compound ($COMPOUND);
	if ($COMP_DIR) {
	    if ($COMP_DIR eq &cur_dir) {
		'.';
	    } else {
		if (! (-d $COMP_DIR)) {
		    print "Cannot find directory $COMP_DIR for compound ${COMPOUND}.\n";
		}
		"./$COMP_DIR";
	    }
	} else {
	    print "Cannot find directory for compound ${COMPOUND}.\n";
	    '';
	}
    } else { ''; }
}

# Get the directory for the given compound from HOPE.
# If we already have data for this compound, return the cached directory.
# Otherwise, get compound data from HOPE.
#
sub get_dir_for_compound {
    local ($COMPOUND) = @_;
    if ($COMPOUND) {
	if (! defined ($COMPOUND_DIRS{$COMPOUND})) {
	    &get_data_for_compound ($COMPOUND);
	}
	$COMPOUND_DIRS{$COMPOUND};
    }
}

# Put a pathname into canonical form:
#   Collapse multiple slashes
#   Strip off a trailing slash
#   Prepend "./" if path is relative to the current directory
#
sub canonicalize_pathname {
    local ($PATH) = @_;
    if ($PATH) {
	$PATH =~ s/\/+/\//g;
	$PATH =~ s/(.)\/$/$1/;
	if ($PATH =~ /^\/|^\.$|^\.\.$|^\.\/|^\.\.\// ) { $PATH; }
	else { "./$PATH"; }
    }
}

# Find the name portion of the current directory (the characters
# following the final slash, or "/" if current directory is root).
# We cache this in $CUR_DIR, assuming that the script doesn't change
# directory.
#
sub cur_dir {
    $CUR_DIR || do { 
	$CUR_DIR = `@PWD_CMD`;
	chop ($CUR_DIR);
	$CUR_DIR =~ s/.*\/([^\/]+)$/$1/;
	$CUR_DIR;
	}
}

# Add the given directory to %DIRS.  The value paired with each
# directory is null here.  In &start_checkout, it will be replaced with
# the name of a timestamp file.
#
# This routine assumes that the script will use the "find" command on
# each directory in %DIRS to find files to be compressed or
# uncompressed.  Because "find" is recursive, we store only the highest
# directory level when given a directory and a subdirectory.  Suppose we
# have already added /foo to %DIRS, and we are later given directory
# /foo/bar.  We don't need to store /foo/bar.
#
sub collect_dir {
    local ($TESTDIR) = @_;
    local ($MATCH, $DIR, $DIRLEN, @LDIRS, $i);
    local ($TESTDIRLEN) = length ($TESTDIR);
    if ($TESTDIRLEN) {
	if ($TESTDIR eq '/') {
	    undef (%DIRS);
	    $DIRS{$TESTDIR} = '';
	} elsif (! (defined ($DIRS{'/'}))) {
	    @LDIRS = keys (%DIRS);
	    for ($i = 0; ($i < @LDIRS) && (! $MATCH); $i++) {
		$DIR = $LDIRS[$i];
		$DIRLEN = length ($DIR);
		if ($TESTDIRLEN == $DIRLEN) {
		    if ($TESTDIR eq $DIR) { $MATCH = 1; }
		} elsif ($TESTDIRLEN < $DIRLEN) {
		    if ((substr ($DIR, 0, $TESTDIRLEN + 1) eq "${TESTDIR}/")) {
			delete ($DIRS{$DIR});
			$DIRS{$TESTDIR} = '';
			$MATCH = 1;
		    }
		} elsif ((substr ($TESTDIR, 0, $DIRLEN + 1) eq "${DIR}/")) {
		    $MATCH = 1;
		}
	    }
	    unless ($MATCH) { $DIRS{$TESTDIR} = ''; }
	}
    }
    %DIRS;
}

# Given a file name, add the directory portion to %DIRS.  If the file is
# itself a directory, use that.  Otherwise, strip off anything after the
# last / and use that.
#
sub collect_file_dir {
    local ($FILE) = @_;
    if ($FILE) {
	if (! -d $FILE) {
	    if ($FILE !~ s/(.*\/)[^\/]*$/$1/) {$FILE = '.'; }
	}
	$FILE = &canonicalize_pathname ($FILE);
	&collect_dir ($FILE);
    }
}

# Get units for the given compound from HOPE.
# If we already have data for this compound, return the cached units.
# Otherwise, get compound data from HOPE.
#
sub get_units_for_compound {
    local ($COMPOUND) = @_;
    if ($COMPOUND) {
	if (! defined ($COMPOUND_UNITS{$COMPOUND})) {
	    &get_data_for_compound ($COMPOUND);
	}
	split (' ', $COMPOUND_UNITS{$COMPOUND});
    }
}

# Get the directory and units for this compound from HOPE.
# Store the directory in %COMPOUND_DIRS and the units in %COMPOUND_UNITS.
#
sub get_data_for_compound {
    local ($COMPOUND) = @_;
    local ($IN_COMP, @COMP_UNITS);
    if (open (HOPEIN,
 "@HOPE_CMD status -not-page -compound $COMPOUND -show attributes,units -format program |")) {
	print "Getting data for compound $COMPOUND from HOPE ...\n";
      hopein: while (<HOPEIN>) {
	  if ($IN_COMP) {
	      if (/^\s*unit\s+(\S+)/) {
		  push (@COMP_UNITS, $1);
	      } elsif (/^\s*attribute\s+_Directory\s+(\S+)/) {
		  $COMPOUND_DIRS{$COMPOUND} = $1;
	      } elsif (/^\s*endcompound/) {
		  last hopein;
	      }
	  } elsif (/^\s*startcompound\s+${COMPOUND}\s/) {
	      $IN_COMP = 1;
          }
        }
        close (HOPEIN);
        if ($IN_COMP) { $COMPOUND_UNITS{$COMPOUND} = join (' ', @COMP_UNITS); }
    } else {
        print "Cannot get data for compound $COMPOUND from HOPE.\n";
    }
}

# Examine a unit given on the command line.  If the suffix doesn't match
# a compressed-file type, we try appending each compressed-file suffix
# to the given unit name to see whether a unit ending in that suffix
# exists.  (E.g., if the argument is "foo.doc", we look for a unit
# "foo.doc.gz".)
#
# If a compressed-file suffix has an associated uncompressed-file suffix
# in %ZIP_TYPES, we don't just append the suffix to the unit name.
# Instead, we strip the uncompressed-file suffix from the given unit
# argument, and then we add the compressed-file suffix to the base and
# search for the resulting unit.  For instance, suppose the
# compressed-file suffix ".dgz" is paired with the uncompressed-file
# suffix ".doc", and suppose the given unit argument is "foo.doc".  We
# look for a unit named "foo.dgz".
#
# We collect all such units that exist.  If the base unit (e.g.,
# "foo.doc") also exists, we put that at the front of our list.  We then
# use the first unit in this list.  If necessary we also change the name
# of the unit we will give to the HOPE checkout command, adding the
# appropriate compressed-file suffix.
#
sub match_units_for_compound {
    local ($COMPOUND, $UNIT) = @_;
    local ($UNZ_TYPE, $UNZ_TYPE_PAT, $UNIT_BASE, $SEARCH, $MATCH, $i);
    local (@ZIP_UNITS);
    local ($RETVAL) = $UNIT;
    if ($UNIT !~ /$ZIP_TYPE_PAT/) {
	foreach $Z_TYPE (@ZIP_TYPES) {
	    if ($UNZ_TYPE = $ZIP_TYPES{$Z_TYPE}) {
		($UNZ_TYPE_PAT = $UNZ_TYPE) =~ s/(\W)/\\$1/g;
		$UNZ_TYPE_PAT = "$UNZ_TYPE_PAT\$";
		$UNIT_BASE = $UNIT;
		if ($UNIT_BASE =~ s/$UNZ_TYPE_PAT//) {
		    $SEARCH = "${UNIT_BASE}${Z_TYPE}";
		} else { $SEARCH = ''; }
	    } else { $SEARCH = "${UNIT}${Z_TYPE}"; }
	    if ($SEARCH && &unit_exists ($COMPOUND, $SEARCH)) {
		$MATCH = 0;
		for ($i = 0; ($i < @ZIP_UNITS) && (! $MATCH); $i++) {
		    if ($SEARCH eq $ZIP_UNITS[$i]) { $MATCH = 1; }
		}
		if (! $MATCH) { push (@ZIP_UNITS, $SEARCH); }
	    }
	}
	if (@ZIP_UNITS) {
	    if (&unit_exists ($COMPOUND, $UNIT)) {
		unshift (@ZIP_UNITS, $UNIT);
	    }
	    if (@ZIP_UNITS > 1) {
		print "Found units";
		for ($i = 0; $i < @ZIP_UNITS; $i++) {
		    print " $ZIP_UNITS[$i]";
		    if ((@ZIP_UNITS > 2) && ($i < (@ZIP_UNITS - 1))) {
			print ",";
		    }
		    if ($i == (@ZIP_UNITS - 2)) { print " and"; }
		}
		print " in compound $COMPOUND; trying unit $ZIP_UNITS[0]\n";
	    }
	    $RETVAL = $ZIP_UNITS[0];
	}
    }
    $RETVAL
}

# Return true if the given string names a unit for the current compound;
# otherwise return false.
#
sub unit_exists {
    local ($COMPOUND, $UNIT) = @_;
    foreach $COMP_UNIT (&get_units_for_compound ($COMPOUND)) {
	if ($UNIT eq $COMP_UNIT) { return 1; }
    }
    0;
}

# Add the current object spec (compound plus optional units plus
# optional filename) to @HOPE_ARGS.
# If there is a filename and there are multiple units, we repeat the
# filename for each unit, since -filename in HOPE does not persist
# across -and.  We are making a big assumption here that that was the
# user's intention.
# We do the same with -until-date and -until-version, which also do not
# persist across -and.
sub add_object_to_hope_args {
    local ($COMPOUND, $FILENAME, $UNTIL_DATE, $UNTIL_VERSION, @UNITS) = @_;
    local ($i);
    if ($COMPOUND)      { push (@HOPE_ARGS, "-compound", $COMPOUND); }
    if ($FILENAME)      { push (@HOPE_ARGS, "-filename", $FILENAME); }
    if ($UNTIL_DATE)    { push (@HOPE_ARGS, "-until-date", $UNTIL_DATE); }
    if ($UNTIL_VERSION) { push (@HOPE_ARGS, "-until-version", $UNTIL_VERSION); }
    for ($i = 0; $i < @UNITS; $i++) {
	$UNIT = $UNITS[$i];
	push (@HOPE_ARGS, "-unit", $UNIT);
	if ($i < (@UNITS - 1)) {
	    push (@HOPE_ARGS, "-and");
	    if ($FILENAME) { push (@HOPE_ARGS, "-filename", $FILENAME); }
	    if ($UNTIL_DATE) { push (@HOPE_ARGS, "-until-date", $UNTIL_DATE); }
	    if ($UNTIL_VERSION) {
		push (@HOPE_ARGS, "-until-version", $UNTIL_VERSION);
	    }
	}
    }
}

# Find each entry in the %DIRS assoc array, touch a timestamp file.
# After the checkout, we will compare the modification time of the
# timestamp file with those of candidate files in the directory.  We
# will uncompress candidate files modified later than the timestamp
# file.
#
# In the %DIRS assoc array, for each directory that will have a
# checkout, we replace the entry's value (null) with the name of the
# timestamp file for that directory.  We will use this value later (in
# &end_checkout) to identify the timestamp file.
#
# Before touching a timestamp file, we set up signal handlers to delete
# the timestamp files in the event of user abort.
#
# Note on the timestamp file: HOPE always stamps checked-out files with
# the current time on the machine running the HOPE client, NOT the time
# on the file server (which may be a different machine).  We always
# write the timestamp file in the /tmp directory, which had better be
# local to the client machine (i.e., the machine on which this script is
# running).  If HOPE is changed to take the modification time for
# checked-out files from the file server, the right thing to do is to
# put the timestamp file into the directory in which files are checked
# out.  To do this, comment out the "if ( 0 )" line and uncomment the
# other commented-out lines below.  But note: that directory might not
# exist before the HOPE checkout command is executed.
#
sub start_checkout {
    foreach $DIR (keys (%DIRS)) {
	if ( 0 ) {
#	if ( -w $DIR ) {
	    if ($DIR =~ /\/$/) { $DIRS{$DIR} = "${DIR}${TS_FILE}" }
	    else { $DIRS{$DIR} = "${DIR}/${TS_FILE}" }
	    &set_signal_handlers;
	    system (@TOUCH_CMD, "$DIRS{$DIR}");
	} else {
#	    &print_warning ("Directory $DIR does not exist or is not writable.  You might have to uncompress checked-out files in that directory yourself.");
	    if ( -w $TMP_DIR ) {
		$DIRS{$DIR} = "${TMP_DIR}/${TS_FILE}";
		&set_signal_handlers;
		system (@TOUCH_CMD, "$DIRS{$DIR}");
	    }
	}
    }
}

# For each directory that has had a checkout, look for files to be
# uncompressed and uncompress them.  If a directory has had a checkout,
# the value of the directory's entry in the %DIRS assoc array is the
# name of a timestamp file.  We use "find" to collect all files in each
# directory (and subdirectories, recursively) that match any of the
# compressed-file suffixes.  We then compare the modification times of
# these files to those of the timestamp file.  We assume that if a file
# was modified later than the timestamp file, it was just checked out.
# We add each file to the %FILES assoc array.  For each file, the
# associated value is the compressed-file suffix.
#
# When we have collected all the files, we delete the timestamp files
# and reset the signal handlers that would have deleted them in case of
# user abort.  We then print the files and, if a -prompt argument was
# given, prompt before uncompressing.  Finally, we uncompress any
# necessary files.
#
# Note: It would be simpler and more efficient to use "find -newer
# $TS_FILE" rather than collect all the files and compare modification
# times explicitly.  But I (rom) didn't particularly trust "find" to
# work, mostly because I'm not sure the time granularity is guaranteed
# to be fine enough.  Paranoia, I guess.
#
sub end_checkout {
    local (%FILES);
    while (($DIR, $TS_FILE) = (each (%DIRS))) {
	if ($TS_FILE) {
	    if ( -d $DIR ) {
		local (%LFILES);
		print "Searching for files to uncompress in directory $DIR ...\n";
		foreach $TYPE (@ZIP_TYPES) {
		    if (open (FINDIN,
			      "@FIND_CMD $DIR -name '*$TYPE' -print |")) {
			while (<FINDIN>) {
			    chop;
			    if (! defined ($LFILES{$_})) {
				$LFILES{$_} = $TYPE;
			    }
			}
			close (FINDIN);
		    }
		}
		if (%LFILES) {
		    local ($TSTIME) = (stat ($TS_FILE))[9];
		    if ($TSTIME) {
			while (($FILE, $TYPE) = each (%LFILES)) {
			    local ($FTIME) = (stat ($FILE))[9];
			    if ($FTIME && ($FTIME >= $TSTIME)) {
				if (! defined ($FILES{$FILE})) {
				    $FILES{$FILE} = $TYPE;
				}
			    }
			}
		    }
		}
	    }
	}
    }
# Use "rm -f" to delete files, because the same file might appear more
# than once in the list.
    foreach $TS_FILE (values (%DIRS)) {
	if ($TS_FILE) { system (@RM_CMD, "-f", "$TS_FILE"); }
    }
    &clear_signal_handlers;
    &uncompress_files;
}

# Uncompress files in the %FILES assoc array.  If a -prompt argument was
# given, prompt before uncompressing.  For each file, use the
# compressed-file suffix that is the value of the %FILES entry as the
# suffix for the compressed file.
#
# If the compressed-file suffix is associated (in %ZIP_TYPES) with an
# uncompressed-file suffix, we can't just let gunzip name the
# uncompressed file by stripping the compressed-file type from the
# compressed file's name.  We want the effect to be that the
# compressed-file suffix is first stripped from the name of the
# compressed file, and the name of the uncompressed file is formed by
# adding the uncompressed-file suffix to the resulting base.  We have to
# rename files to make this work.  But if we try to rename the
# compressed file by stripping off the compressed-file suffix, the base
# file might already exist.  To avoid this, we construct a (presumed
# unique) temporary file name, uncompress the temporary file, and rename
# the resulting uncompressed file to have the intended uncompressed-file
# name.
#
# If a -prompt argument is given, the script prompts before overwriting
# a read-only file.  Otherwise, it forces overwriting of a read-only
# file (but prompts before overwriting a writable file).  When possible
# it lets gunzip do the prompting.
#
sub uncompress_files {
    local ($Z_TYPE, $Z_TYPE_PAT, $UNZ_TYPE, $BASE, $UNZ_FILE);
    local ($TMP_FILE, $TMP_ZFILE);
    if (%FILES) {
	&print_files ("u");
	if ($PROMPT) { &prompt_for_files ("u"); }
    }
    if (%FILES) {
	if ($PROMPT) { &print_files ("u"); }
      files: foreach $FILE (sort (keys (%FILES))) {
	  if (( -z $FILE ) || (system (@GUNZIP_CMD, "-t", "-q", "$FILE"))) {
	      &print_warning ("$FILE does not appear to be a properly compressed file; not uncompressing it.");
	      next files;
	  }
	  $Z_TYPE = $FILES{$FILE};
	  ($Z_TYPE_PAT = $Z_TYPE) =~ s/(\W)/\\$1/g;
	  $Z_TYPE_PAT = "$Z_TYPE_PAT\$";
	  ($BASE = $FILE) =~ s/$Z_TYPE_PAT//;
	  if ($UNZ_TYPE = $ZIP_TYPES{$Z_TYPE}) {
	      $UNZ_FILE = "${BASE}${UNZ_TYPE}";
	      if ( -e $UNZ_FILE ) {
		  if ( -d $UNZ_FILE ) {
		      &print_warning ("Directory $UNZ_FILE already exists; not uncompressing $FILE.");
		  } else {
		      local ($DO_RM);
		      if ((! ( -w $UNZ_FILE )) && (! $PROMPT)) {
			  $DO_RM = 1;
			  &print_warning ("Overwriting existing file $UNZ_FILE.");
		      } elsif (&prompt_for_overwrite ($UNZ_FILE)) {
			  $DO_RM = 1;
		      }
		      if ($DO_RM) {
			  system (@RM_CMD, "-f", "$UNZ_FILE");
			  if ( -e $UNZ_FILE ) {
			      &print_error ("@RM_CMD failed to delete $UNZ_FILE; not uncompressing $FILE.");
			  }
		      }
		  }
	      }
	      if (! ( -e $UNZ_FILE )) {
		  $TMP_FILE = "${BASE}$$";
		  $TMP_ZFILE = "${TMP_FILE}${Z_TYPE}";
		  if (! (( -e $TMP_FILE ) || ( -e $TMP_ZFILE ))) {
		      system (@MV_CMD, "$FILE", "$TMP_ZFILE");
		      if ( -e $TMP_ZFILE ) {
			  system (@GUNZIP_CMD, "-n", "-S", "$Z_TYPE", "$TMP_ZFILE");
			  if ( -e $TMP_FILE ) {
			      system (@MV_CMD, "$TMP_FILE", "$UNZ_FILE");
			  } else {
			      &print_error ("@GUNZIP_CMD failed to uncompress temporary file $TMP_ZFILE, renamed from $FILE.  Renaming $TMP_ZFILE to $FILE.");
			      system (@MV_CMD, "$TMP_ZFILE", "$FILE");
			  }
		      } else {
			  &print_error ("@MV_CMD failed to rename $FILE to $TMP_ZFILE; not uncompressing $FILE.");
		      }
		  } else {
		      &print_error ("Cannot rename $FILE to temporary file $TMP_ZFILE because $TMP_FILE or $TMP_ZFILE already exists; not uncompressing $FILE.");
		  }
	      }
	  } else {
	      local ($FORCE);
	      $UNZ_FILE = $BASE;
	      if ( -e $UNZ_FILE ) {
		  if ( -d $UNZ_FILE ) {
		      &print_warning ("Directory $UNZ_FILE already exists; not uncompressing $FILE.");
		      next files;
		  } elsif ((! ( -w $UNZ_FILE )) && (! $PROMPT)) {
		      $FORCE = 1;
		      &print_warning ("Overwriting existing file $UNZ_FILE.");
		  }
	      }
	      if ($FORCE) {
		  system (@GUNZIP_CMD, "-f", "-n", "-S", "$Z_TYPE", "$FILE");
	      } else {
		  system (@GUNZIP_CMD, "-n", "-S", "$Z_TYPE", "$FILE");
	      }
	  }
      }
    }
}

# Print the list of files to be compressed or uncompressed.  The
# argument is "c" to indicate compressing and "u" to indicate
# uncompressing.
#
sub print_files {
    local ($COMPRESS) = @_;
    local (@FILES) = sort (keys (%FILES));
    if ($COMPRESS =~ /^[cC]/) { $COMPRESS = "Compressing"; }
    else { $COMPRESS = "Uncompressing"; }
    print "$COMPRESS the following file";
    if (@FILES > 1) { print "s"; }
    print ":\n";
    foreach $FILE (@FILES) { print "${FILE}\n"; }
}

# Prompt for confirmation of files (in %FILES) to be compressed or
# uncompressed.  The argument is "c" to indicate compressing and "u" to
# indicate uncompressing.  Options are to act on all or no files or to
# selectively pick files to act upon.  In selective prompting, we ask
# for confirmation of each file.  If the user does not want to compress
# or uncompress a file, we delete its entry from %FILES.
#
sub prompt_for_files {
    local ($COMPRESS) = @_;
    local ($SELECTIVE, $COMP_LC);
    local (@FILES) = sort (keys (%FILES));
    if (! @FILES) { return; }
    if ($COMPRESS =~ /^[cC]/) { $COMPRESS = "Compress"; }
    else { $COMPRESS = "Uncompress"; }
    ($COMP_LC = $COMPRESS) =~ tr/CU/cu/;
    if (@FILES == 1) { $SELECTIVE = 1; }
    else {
      prompt: {
	  print "$COMPRESS these files? [y, n, s, ?] ";
	  while (<STDIN>) {
	      chop;
	      if (/^\s*[yY]\s*$|^\s*[yY][eE][sS]\s*$/) { return; }
	      elsif (/^\s*[nN]\s*$|^\s*[nN][oO]\s*$/) {
		  undef %FILES;
		  return;
	      } elsif (/^\s*[sS]\s*$/) {
		  $SELECTIVE = 1;
		  last prompt;
	      } else {
		  print "Possible responses are:\n";
		  print "  y  ($COMP_LC all files)\n";
		  print "  n  (do not $COMP_LC any files)\n";
		  print "  s  (selectively $COMP_LC, prompting for each file)\n";
		  print "  ?  (print this message)\n";
		  redo prompt;
	      }
	  }
      }
    }
    if ($SELECTIVE) {
      file: foreach $FILE (@FILES) {
	prompt: {
	    print "$COMPRESS ", $FILE, "? [y, n, a, ?] ";
	    while (<STDIN>) {
		chop;
		if (/^\s*[yY]\s*$|^\s*[yY][eE][sS]\s*$/) { next file; }
		elsif (/^\s*[nN]\s*$|^\s*[nN][oO]\s*$/) {
		    delete ($FILES{$FILE});
		    next file;
		} elsif (/^\s*[aA]\s*$|^\s*[aA][lL][lL]\s*$/) { return; }
		else {
		    print "Possible responses are:\n";
		    print "  y  ($COMP_LC $FILE)\n";
		    print "  n  (do not $COMP_LC $FILE)\n";
		    print "  a  ($COMP_LC $FILE and all remaining files)\n";
		    print "  ?  (print this message)\n";
		    redo prompt;
		}
	    }
	}
      }
    }
}

# Prompt for confirmation before overwriting an existing file.  Return
# true if the user wants to overwrite; otherwise, return false.
#
sub prompt_for_overwrite {
    local ($FILE) = @_;
  prompt: {
      print "File $FILE already exists; do you wish to overwrite? [y, n, ?] ";
	  while (<STDIN>) {
	      chop;
	      if (/^\s*[yY]\s*$|^\s*[yY][eE][sS]\s*$/) { return (1); }
	      elsif (/^\s*[nN]\s*$|^\s*[nN][oO]\s*$/) { return (0); }
	      else {
		  print "Possible responses are:\n";
		  print "  y  (overwrite $FILE)\n";
		  print "  n  (do not overwrite $FILE)\n";
		  print "  ?  (print this message)\n";
		  redo prompt;
	      }
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

# Catch SIGINT and SIGQUIT (user abort).
#
sub set_signal_handlers {
    $SIG{INT} = 'handle_abort';
    $SIG{QUIT} = 'handle_abort';
}

# Reset SIGINT and SIGQUIT handlers to the default.
#
sub clear_signal_handlers {
    $SIG{INT} = 'DEFAULT';
    $SIG{QUIT} = 'DEFAULT';
}

# In the event of user abort, delete the timestamp files.
#
sub handle_abort {
    local ($SIG_NAME) = @_;
    foreach $TS_FILE (values (%DIRS)) {
	if ($TS_FILE) { system (@RM_CMD, "-f", "$TS_FILE"); }
    }
    exit (0);
}

# Do it!
#
&main;
