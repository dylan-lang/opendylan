#! /usr/local/bin/perl

# This script parses a structured outline of DUIM interfaces and
# generates skeleton reference documentation in FrameMaker MIF format.
#
# You can abbreviate command-line options as in HOPE.  Options are:
#
#  -infile <argument>     Input filename.  Default is STDIN.
#  -outfile <argument>    Output filename.  Default is STDOUT, unless
#                         you specify -split-output.  You cannot specify
#                         both -outfile and -split-output.
#  -split-output          Flag meaning to write a separate output file
#                         for each DUIM library encountered in the
#                         outline.  The name of each file is the library
#                         name, in lower case, and with $OUTPUT_SUFFIX
#                         appended.  An initial file, for anything in
#                         the outline that appears before the first
#                         library, is named $OUTPUT_INITIAL_NAME with
#                         $OUTPUT_SUFFIX appended.  You cannot specify
#                         both -outfile and -split-output.
#  -overwrite             Flag meaning to overwrite any existing output
#                         files.  You cannot specify both -overwrite and
#                         -append.  If neither is specified and an
#                         output file exists, the script tries to prompt
#                         for an action to take.
#  -append                Flag meaning to append to any existing output
#                         files.  You cannot specify both -overwrite and
#                         -append.  If neither is specified and an
#                         output file exists, the script tries to prompt
#                         for an action to take.
#  -skip-internals        Flag meaning to ignore a module (and its
#                         interfaces) if the module has the substring
#                         'internal' in its name.
#  -verbose               Flag meaning to print some extra output (to
#                         STDERR), as when opening and closing files.
#  -help                  Flag meaning to show help.
#  -usage                 Same as -help.


## Main routine.
## Global parameters are defined here.
sub main {
    local ($INSTREAM) = '-';
    local ($OUTSTREAM) = '>-';
    local ($OUTPUT_INITIAL_NAME) = 'duim-initial';
    local ($OUTPUT_SUFFIX) = '-outline.mif';
    local ($SPLIT_OUTPUT) = 0;
    local ($OVERWRITE_OUTPUT) = 0;
    local ($APPEND_OUTPUT) = 0;
    local ($VERBOSE) = 0;
    local ($SKIP_INTERNAL_MODULES) = 0;
    local ($MACRO_CALL_DELIMITERS) = '\s\,\;\=\[\]\(\)\.';
    local ($INDEX_LEADING_CHARS_TO_STRIP) = '\<\$\*\\\\';
    local ($MIF_VERSION) = '5.00';
    local ($LIBRARY_PARA) = 'Chapter';
    local ($MODULE_PARA) = '1Heading';
    local ($INTERFACE_PARA) = 'REntry';
    local ($INTERFACE_TYPE_PARA) = 'REntry-type';
    local ($SUBENTRY_PARA) = 'REntry-head';
    local ($DESCRIPTION_PARA) = 'RDescription';
    local ($DESCRIPTION_ENTRY_PARA) = 'RDescription';
    local ($BODY_INDENT_PARA) = 'Body-Indent';
    local ($BODY_ENTRY_PARA) = 'RBody';
    local ($CODE_BODY_PARA) = 'RCode-body';
    local ($CODE_FIRST_PARA) = 'RCode-first';
    local ($CODE_LAST_PARA) = 'RCode-last';
    local ($CODE_LINE_PARA) = 'RCode-line';
    local ($SIGNATURE_PARA) = 'RSignature';
    local ($CODE_STYLE) = 'Code';
    local ($VARIABLE_STYLE) = 'Variable';
    local ($INDEX_MARKER_TYPE) = '2';
    local ($ONLINE_CONDITION_TAG) = 'WWW';
    local ($MIF_CONDITIONAL_PENDING) = 0;
    local (@MIF_PENDING_CONDITIONS);
    &process_arglist;
    &open_streams;
    &process_outline;
    &close_streams;
}

## Argument processing

sub process_arglist {
    local ($ARG, $NEXT_ARG, $FULL_ARG, $NEXT_ACTION);
    local (%ARG_ARRAY, %ARGS_SEEN);
    local ($NO_ARG) = 0;
    local ($TAKES_ARG) = 1;
    local ($ARG_NOT_SEEN) = 0;
    local ($ARG_SEEN) = 1;
    &fill_arg_array (*ARG_ARRAY);
    &fill_args_seen (*ARGS_SEEN);
    while (@ARGV) {
	$ARG = shift (@ARGV);
	($FULL_ARG, $NEXT_ACTION) = &match_arg ($ARG, *ARG_ARRAY, *ARGS_SEEN);
	if ($FULL_ARG eq '-infile') {
	    if (defined ($NEXT_ARG = &next_arg)) { $INSTREAM = $NEXT_ARG; }
	    else {
		&argument_error ("No argument given for option ${FULL_ARG}.",
				 *ARG_ARRAY);
	    }
	} elsif ($FULL_ARG eq '-outfile') {
	    if ($SPLIT_OUTPUT) {
		&argument_error
		    ("You cannot supply both -outfile and -split-output.",
		     *ARG_ARRAY);
	    } elsif (defined ($NEXT_ARG = &next_arg)) {
		$OUTSTREAM = $NEXT_ARG;
	    } else {
		&argument_error ("No argument given for option ${FULL_ARG}.",
				 *ARG_ARRAY);
	    }
	} elsif ($FULL_ARG eq '-split-output') {
	    if ($OUTSTREAM ne '>-') {
		&argument_error
		    ("You cannot supply both -outfile and -split-output.",
		     *ARG_ARRAY);
	    } else { $SPLIT_OUTPUT = 1; }
	} elsif ($FULL_ARG eq '-overwrite') {
	    if ($APPEND_OUTPUT) {
		&argument_error
		    ("You cannot supply both -append and -overwrite.",
		     *ARG_ARRAY);
	    } else { $OVERWRITE_OUTPUT = 1; }
	} elsif ($FULL_ARG eq '-append') {
	    if ($OVERWRITE_OUTPUT) {
		&argument_error
		    ("You cannot supply both -append and -overwrite.",
		     *ARG_ARRAY);
	    } else { $APPEND_OUTPUT = 1; }
	} elsif ($FULL_ARG eq '-skip-internals') {
	    $SKIP_INTERNAL_MODULES = 1;
	} elsif ($FULL_ARG eq '-verbose') { $VERBOSE = 1; }
	elsif ($FULL_ARG eq '-help') {
	    &show_options (*ARG_ARRAY);
	    exit;
	}
    }
}

sub fill_arg_array {
    local (*ARG_ARRAY) = @_;
    %ARG_ARRAY =
	(
	 '-append',                 "-append $NO_ARG",
	 '-help',                   "-help $NO_ARG",
	 '-infile',                 "-infile $TAKES_ARG",
	 '-outfile',                "-outfile $TAKES_ARG",
	 '-overwrite',              "-overwrite $NO_ARG",
	 '-skip-internals',         "-skip-internals $NO_ARG",
	 '-split-output',           "-split-output $NO_ARG",
	 '-usage',                  "-help $NO_ARG",
	 '-verbose',                "-verbose $NO_ARG",
	 );
}

sub fill_args_seen {
    local (*ARGS_SEEN) = @_;
    %ARGS_SEEN =
	(
	 '-append',            $ARG_NOT_SEEN,
	 '-help',              $ARG_NOT_SEEN,
	 '-infile',            $ARG_NOT_SEEN,
	 '-outfile',           $ARG_NOT_SEEN,
	 '-overwrite',         $ARG_NOT_SEEN,
	 '-skip-internals',    $ARG_NOT_SEEN,
	 '-split-output',      $ARG_NOT_SEEN,
	 '-verbose',           $ARG_NOT_SEEN,
	 );
}

sub match_arg {
    local ($ARG, *ARG_ARRAY, *ARGS_SEEN) = @_;
    local ($FULL_ARG, $NEXT_ACTION);
    local ($ARG_PAT);
    local (@MATCHES, $MATCH, $i, $ERR_STRING);
    if ($ARG !~ /^-/) {
	&argument_error ("Unrecognized option ${ARG}.", *ARG_ARRAY);
    }
    else {
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
    ($FULL_ARG, $NEXT_ACTION) = split (' ', $ARG_ARRAY{$MATCH});
    if ($ARGS_SEEN{$FULL_ARG} == $ARG_SEEN) {
	&argument_error ("Multiple use of option ${FULL_ARG}.", *ARG_ARRAY);
    } else { $ARGS_SEEN{$FULL_ARG} = $ARG_SEEN; }
    ($FULL_ARG, $NEXT_ACTION);
}

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

sub argument_error {
    local ($ERR_STRING, *ARG_ARRAY) = @_;
    print STDERR "Error: ${ERR_STRING}\n";
    &show_options (*ARG_ARRAY);
    exit (1);
}

sub show_options {
    local (*ARG_ARRAY) = @_;
    local (@OPTIONS) = sort (keys (%ARG_ARRAY));
    local ($FULL_ARG, $NEXT_ACTION);
    if (@OPTIONS) {
	print STDERR "Recognized options for this command:\n";
	foreach $OPTION (@OPTIONS) {
	    ($FULL_ARG, $NEXT_ACTION) = split (' ', $ARG_ARRAY{$OPTION});
	    print STDERR (" ", $OPTION);
	    if ($NEXT_ACTION == $TAKES_ARG) { print STDERR " <argument>"; }
	    print STDERR "\n";
	}
    } else { print STDERR "No recognized options for this command.\n"; }
}

## Stream processing

sub open_streams {
    $INSTREAM = &check_instream ($INSTREAM, $OUTSTREAM);
    $OUTSTREAM = &check_outstream ($INSTREAM, $OUTSTREAM);
    &open_instream;
    &open_outstream;
}

sub open_instream {
    local ($NAME);
    unless (open(INSTREAM)) {
	if ($INSTREAM eq '-') { $INSTREAM = 'STDIN'; }
	die "Error: Cannot open $INSTREAM for input.\n";
    }
    if ($VERBOSE) {
	if ($INSTREAM eq '-') { $NAME = 'STDIN'; }
	else { $NAME = $INSTREAM; }
	print STDERR "Opening $NAME for input.\n";
    }
}

sub open_outstream {
    local ($OUTOP) = 'output';
    local ($NAME);
    unless (open(OUTSTREAM)) {
	close (INSTREAM);
	if ($OUTSTREAM eq '>-') { $OUTSTREAM = 'STDOUT'; }
	else {
	    if ($OUTSTREAM =~ /^>>/) { $OUTOP = 'appending'; }
	    $OUTSTREAM =~ s/^>+//;
	}
	die "Error: Cannot open $OUTSTREAM for ${OUTOP}.\n";
    }
    if ($VERBOSE) {
	if ($OUTSTREAM eq '>-') { $NAME = 'STDOUT'; }
	else { ($NAME = $OUTSTREAM) =~ s/^>+//; }
	print STDERR "Opening $NAME for output.\n";
    }
}

sub check_instream {
    local ($IN, $OUT) = @_;
    if ($IN eq '-') { $IN; }
    else {
      test: {
	  unless (-r $IN) {
	      unless (open (TMPIN, "/dev/tty")) {
		  print STDERR "Cannot read input file ${IN}.\n";
		  print STDERR
		      "Cannot open /dev/tty to prompt for action to take.\n";
		  die "Quitting.\n";
	      }
	    prompt: {
		print STDERR "Cannot read input file ${IN}.\n";
		print STDERR "Action? [n, q, ?] ";
		$_ = <TMPIN>;
		chop;
		if (/^\s*[nN]\s*$/) {
		  getnew: {
		      print STDERR "New input file: ";
		      $_ = <TMPIN>;
		      chop;
		      $_ = &string_trim_whitespace ($_);
		      if ($_) {
			  $IN = $_;
			  close (TMPIN);
			  redo test;
		      } else { redo getnew; }
		  }
		} elsif (/^\s*[qQ]\s*$/) {
		    close (TMPIN);
		    die "Quitting.\n";
		} else {
		    print STDERR "Possible responses are:\n";
		    print STDERR "  n  (supply a new filename)\n";
		    print STDERR "  q  (quit)\n";
		    print STDERR "  ?  (print this message)\n";
		    redo prompt;
		}
	    }
	      close (TMPIN);
	  }
      }
	$IN;
    }
}

sub check_outstream {
    local ($IN, $OUT) = @_;
    if ($SPLIT_OUTPUT && ($OUT eq '>-')) {
	$OUT = "${OUTPUT_INITIAL_NAME}${OUTPUT_SUFFIX}";
    }
    if ($OUT eq '>-') { $OUT; }
    else {
      test: {
	  if (-e $OUT) {
	      if ($APPEND_OUTPUT) { $OUT = ">$OUT"; }
	      elsif (! $OVERWRITE_OUTPUT) {
		  unless (open (TMPIN, "/dev/tty")) {
		      print STDERR "Output file $OUT already exists.\n";
		      print STDERR
			  "Cannot open /dev/tty to prompt for action to take.\n";
		      die "Quitting.\n";
		  }
		prompt: {
		    print STDERR "Output file $OUT already exists.\n";
		    print STDERR "Action? [a, a!, n, o, o!, q, ?] ";
		    $_ = <TMPIN>;
		    chop;
		    if (/^\s*[aA](\!?)\s*$/) {
			$OUT = ">$OUT";
			if ($1 eq '!') { $APPEND_OUTPUT = 1; }
			last prompt;
		    } elsif (/^\s*[nN]\s*$/) {
		      getnew: {
			  print STDERR "New output file: ";
			  $_ = <TMPIN>;
			  chop;
			  $_ = &string_trim_whitespace ($_);
			  if ($_) {
			      $OUT = $_;
			      close (TMPIN);
			      redo test;
			  } else { redo getnew; }
		      }
		    } elsif (/^\s*[oO](\!?)\s*$/) {
			if ($1 eq '!') { $OVERWRITE_OUTPUT = 1; }
			last prompt;
		    } elsif (/^\s*[qQ]\s*$/) {
			close (TMPIN);
			die "Quitting.\n";
		    } else {
			print STDERR "Possible responses are:\n";
			print STDERR "  a  (open $OUT for appending)\n";
			print STDERR "  a! (open $OUT and all remaining files for appending)\n";
			print STDERR "  n  (supply a new filename)\n";
			print STDERR "  o! (overwrite $OUT and all remaining files)\n";
			print STDERR "  q  (quit)\n";
			print STDERR "  ?  (print this message)\n";
			redo prompt;
		    }
		}
		  close (TMPIN);
	      }
	  }
      }
	">$OUT";
    }
}

sub close_streams {
    &close_outstream;
    &close_instream;
}

sub close_instream {
    local ($NAME);
    close (INSTREAM);
    if ($VERBOSE) {
	if ($INSTREAM eq '-') { $NAME = 'STDIN'; }
	else { $NAME = $INSTREAM; }
	print STDERR "Closing ${NAME}.\n";
    }
}

sub close_outstream {
    local ($NAME);
    close (OUTSTREAM);
    if ($VERBOSE) {
	if ($OUTSTREAM eq '>-') { $NAME = 'STDOUT'; }
	else { ($NAME = $OUTSTREAM) =~ s/^>+//; }
	print STDERR "Closing ${NAME}.\n";
    }
}

## Main loop to process lines from outline.
## The central data structure is an assoc array called %INTERFACE.
## Array elements hold information about the current interface,
## such as name, type, arguments, values, and miscellaneous
## continued lines.

sub process_outline {
    local ($LIBRARY, $MODULE, %INTERFACE, %ARG_NAME_TYPE_ALIST);
    local ($INSIDE_INTERFACE) = 0;
    local ($MIF_LEVEL) = 0;
    local ($SKIP) = 0;
    &fill_arg_name_type_alist (*ARG_NAME_TYPE_ALIST);
    &initialize_mif;
    while (<INSTREAM>) {
	chop;
	if ($INSIDE_INTERFACE) {
	    if (/^\s/ || /^$/) { &process_continuation (*INTERFACE, $_); }
	    else {
		&finish_interface (*INTERFACE);
		$INSIDE_INTERFACE = 0;
	    }
	}
	if (! $INSIDE_INTERFACE) {
	    if (/^[0-9]+\.\s+(\S+)/) {
		if ($SPLIT_OUTPUT) {
		    &open_new_output_stream (&string_downcase ($1));
		}
		$LIBRARY = &process_library ($1);
		undef ($MODULE);
		$SKIP = 0;
	    }
	    elsif (/^[0-9]+\.[0-9]+\s+(\S+)/) {
		if ($SKIP_INTERNAL_MODULES) {
		    if (($MODULE = &string_downcase ($1)) =~ /internal/) {
			$SKIP = 1;
			undef ($MODULE);
		    } else { $SKIP = 0; }
		}
		unless ($SKIP) { $MODULE = &process_module ($1); }
	    }
	    elsif (/^[^\-]/ && /^(\S+)(.*)\[([^\[\]]*)\]\s*$/) {
		unless ($SKIP) {
		    &process_interface (*INTERFACE, $LIBRARY, $MODULE,
					$1, $2, $3);
		    $INSIDE_INTERFACE = 1;
		}
	    }
	}
    }
    if ($INSIDE_INTERFACE) {
	&finish_interface (*INTERFACE);
	$INSIDE_INTERFACE = 0;
    }
    &finalize_mif;
}

sub open_new_output_stream {
    local ($NAME) = @_;
    &finalize_mif;
    &close_outstream;
    $OUTSTREAM = &check_outstream ($INSTREAM, "${NAME}${OUTPUT_SUFFIX}");
    &open_outstream;
    &initialize_mif;
}

## Note: any new fields in %INTERFACE have to be reset here.
sub reset_interface {
    local (*INTERFACE) = @_;
    undef ($INTERFACE{'CATEGORY'});
    undef ($INTERFACE{'CONTINUATION'});
    undef ($INTERFACE{'DESCRIPTION'});
    undef ($INTERFACE{'EXAMPLE'});
    undef ($INTERFACE{'INIT_KEYWORDS'});
    undef ($INTERFACE{'INIT_VALUE'});
    undef ($INTERFACE{'KEY_ARG_NAMES'});
    undef ($INTERFACE{'KEY_ARG_TYPES'});
    undef ($INTERFACE{'KEY_ARG_VALUES'});
    undef ($INTERFACE{'LIBRARY'});
    undef ($INTERFACE{'MODULE'});
    undef ($INTERFACE{'NAME'});
    undef ($INTERFACE{'REQ_ARG_NAMES'});
    undef ($INTERFACE{'REQ_ARG_TYPES'});
    undef ($INTERFACE{'REST_ARG'});
    undef ($INTERFACE{'SIGNATURE'});
    undef ($INTERFACE{'SUPERCLASSES'});
    undef ($INTERFACE{'TYPE'});
    undef ($INTERFACE{'VALUE_NAMES'});
    undef ($INTERFACE{'VALUE_TYPE'});
    undef ($INTERFACE{'VALUE_TYPES'});
}

sub fill_arg_name_type_alist {
    local (*ARG_NAME_TYPE_ALIST) = @_;
    %ARG_NAME_TYPE_ALIST =
        (
         'angle',               '<real>',
         'arc',                 '<elliptical-arc>',
         'array',               '<array>',
         'ascent',              '<real>',
         'blue',                'limited(<real>, min: 0, max: 1)',
         'boolean',             '<boolean>',
         'bottom',              '<coordinate>',
         'box',                 '<bounding-box>',
         'brush',               '<brush>',
         'button',              '<button>',
         'callback',            '<function>',
         'center-point',        '<point>',
         'center-x',            '<real>',
         'center-y',            '<real>',
         'char',                '<character>',
         'child',               '<sheet>', #at least in the Silica layer
         'collection',          '<collection>',
         'color',               '<color>',
         'command',             '<command>',
         'command-table',       '<command-table>',
         'condition',           '<condition>',
         'continuation',        '<function>',
         'coord-seq',           'limited(<sequence>, of: <coordinate>)',
         'coordinate',          '<coordinate>',
         'count',               '<integer>',
#        'depth',               '<real> or <coordinate>', #depending on context
         'descent',             '<real>',
         'dialog',              '<dialog-frame>',
         'display',             '<display>',
         'drawable',            'type-union(<sheet>, <medium>)',
         'dx',                  '<real>',
         'dy',                  '<real>',
         'ellipse',             '<ellipse>',
         'end-angle',           'false-or(<real>)',
         'event',               '<event>',
         'event-queue',         '<event-queue>',
         'font',                '<object>',
         'format-string',       '<string>',
         'from-x',              '<coordinate>',
         'from-y',              '<coordinate>',
         'function',            '<function>',
         'gadget',              '<gadget>',
         'green',               'limited(<real>, min: 0, max: 1)',
#        'height',              '<real> or <coordinate>', #depending on context
         'hue',                 'limited(<real>, min: 0, max: 1)',
         'image',               '<image>',
         'index',               '<integer>',
         'ink',                 '<ink>',
         'integer',             '<integer>',
         'intensity',           'limited(<real>, min: 0, max: sqrt(3))',
         'k',                   '<integer>',
         'keyword',             '<keyword>',
         'left',                '<coordinate>',
         'line',                '<line>',
         'list',                '<list>',
         'locator',             'type-union(<string>, <locator>)',
         'luminosity',          'limited(<real>, min: 0, max: 1)',
         'mask',                '<integer>',
         'medium',              '<medium>',
         'medium-x',            '<coordinate>',
         'medium-y',            '<coordinate>',
         'menu',                '<menu>',
         'menu-bar',            '<menu-bar>',
         'mxx',                 '<real>',
         'mxy',                 '<real>',
         'myx',                 '<real>',
         'myy',                 '<real>',
         'n',                   '<integer>',
         'number',              '<number>',
         'object',              '<object>',
         'opacity',             'limited(<real>, min: 0, max: 1)',
         'origin',              '<point>',
         'origin-x',            '<real>',
         'origin-y',            '<real>',
         'palette',             '<palette>',
         'pane',                '<sheet>',
         'pattern',             '<pattern>',
         'pen',                 '<pen>',
         'pixmap',              '<pixmap>',
         'pixmap-x',            '<coordinate>',
         'pixmap-y',            '<coordinate>',
         'point',               '<point>',
         'point-1',             '<point>',
         'point-2',             '<point>',
         'point-3',             '<point>',
         'point-seq',           'limited(<sequence>, of: <point>)',
         'polygon',             '<polygon>',
         'polyline',            '<polyline>',
         'port',                '<port>',
         'predicate',           '<function>',
         'radius-1-dx',         '<real>',
         'radius-1-dy',         '<real>',
         'radius-2-dx',         '<real>',
         'radius-2-dy',         '<real>',
         'range',               '<range>',
         'real',                '<real>',
         'rectangle',           '<rectangle>',
         'red',                 'limited(<real>, min: 0, max: 1)',
         'region',              '<region>',
         'region1',             '<region>',
         'region2',             '<region>',
         'right',               '<coordinate>',
         'saturation',          'limited(<real>, min: 0, max: 1)',
         'scale-x',             '<real>',
         'scale-y',             '<real>',
         'sequence',            '<sequence>',
         'sheet',               '<sheet>',
         'space-req',           '<space-requirement>',
         'start-angle',         'false-or(<real>)',
         'stencil',             '<stencil>',
         'stream',              '<stream>',
         'string',              '<string>',
         'table',               '<table>',
         'text',                'type-union(<string>, <character>)',
         'text-style',          '<text-style>',
         'to-x',                '<coordinate>',
         'to-y',                '<coordinate>',
         'top',                 '<coordinate>',
         'transform',           '<transform>',
         'transform1',          '<transform>',
         'transform2',          '<transform>',
         'tx',                  '<real>',
         'ty',                  '<real>',
         'value',               '<object>',
         'vector',              '<vector>',
#        'width',               '<real> or <coordinate>', #depending on context
#        'x',                   '<real> or <coordinate>', #depending on context
         'x1',                  '<real>',
         'x2',                  '<real>',
         'x3',                  '<real>',
#        'y',                   '<real> or <coordinate>', #depending on context
         'y1',                  '<real>',
         'y2',                  '<real>',
         'y3',                  '<real>',
         );
}

## Subroutines for processing outline

sub initialize_mif {
    &write_mif_initial;
}

sub finalize_mif {
    &write_mif_final;
}

sub process_library {
    local ($LIBRARY) = @_;
    &write_library_heading ($LIBRARY);
    &string_downcase ($LIBRARY);
}

sub process_module {
    local ($MODULE) = @_;
    &write_module_heading ($MODULE);
    &string_downcase ($MODULE);
}

sub process_interface {
    local (*INTERFACE, $LIBRARY, $MODULE, $NAME, $CONT, $TYPE) = @_;
    $INTERFACE{'LIBRARY'} = $LIBRARY;
    $INTERFACE{'MODULE'} = $MODULE;
    $NAME = &string_trim_whitespace ($NAME);
    $INTERFACE{'NAME'} = $NAME;
    $CONT = &string_trim_whitespace ($CONT);
    $INTERFACE{'CONTINUATION'} = $CONT;
    $TYPE = &string_trim_whitespace ($TYPE);
    &match_interface_type (*INTERFACE, $TYPE);
    if ($INTERFACE{'CATEGORY'} eq 'MACRO') {
	&fixup_macro_name (*INTERFACE);
    }
    &write_interface_name ($INTERFACE{'NAME'}, $INTERFACE{'TYPE'},
			   $INTERFACE{'LIBRARY'}, $INTERFACE{'MODULE'});
    &write_interface_type ($INTERFACE{'TYPE'});
}

sub match_interface_type {
    local (*INTERFACE, $TYPE) = @_;
    local (%TYPE_ARRAY) =
	(
	 'Type',                'TYPE',
	 'Class',               'CLASS',
         'Function',            'FUNCTION',
	 'Method',              'FUNCTION',
         'Constant',            'CONSTANT',
	 'Variable',            'VARIABLE',
         'Macro',               'MACRO',
	 );
    local ($TYPE_LAST, $CATEGORY);
    $TYPE = &string_initial_cap ($TYPE);
    if ($TYPE eq 'Gf method') { $TYPE = 'G.f. method'; }
    ($TYPE_LAST = $TYPE) =~ s/.*\s(\S+)$/$1/;
    $TYPE_LAST = &string_initial_cap ($TYPE_LAST);
    $CATEGORY = $TYPE_ARRAY{$TYPE_LAST};
    unless ($CATEGORY) {
	print STDERR "Warning: For interface $INTERFACE{'NAME'}, unrecognized type $TYPE.\n";
    }
    $INTERFACE{'TYPE'} = $TYPE;
    $INTERFACE{'CATEGORY'} = $CATEGORY;
}

sub fixup_macro_name {
    local (*INTERFACE) = @_;
    if (($INTERFACE{'NAME'} eq 'define') &&
	($INTERFACE{'CONTINUATION'} =~
	 /^([^${MACRO_CALL_DELIMITERS}]+)(.*)/)) {
	$INTERFACE{'NAME'} = "$INTERFACE{'NAME'} $1";
	$INTERFACE{'CONTINUATION'} = &string_trim_whitespace ($2);
    }
}

sub process_continuation {
    local (*INTERFACE, $LINE) = @_;
    if ($INTERFACE{'CONTINUATION'}) {
	$INTERFACE{'CONTINUATION'} = "$INTERFACE{'CONTINUATION'}\n$LINE";
    } else { $INTERFACE{'CONTINUATION'} = $LINE; }
}

## Routines called at the end of each interface.
## These determine the high-level information layout for the various
## categories of interface.

sub finish_interface {
    local (*INTERFACE) = @_;
    &parse_continuation (*INTERFACE);
    local ($CATEGORY) = $INTERFACE{'CATEGORY'};
    if ($CATEGORY eq 'TYPE') { &finish_type (*INTERFACE); }
    elsif ($CATEGORY eq 'CLASS') { &finish_class (*INTERFACE); }
    elsif ($CATEGORY eq 'FUNCTION') { &finish_function (*INTERFACE); }
    elsif ($CATEGORY eq 'CONSTANT') { &finish_constant (*INTERFACE); }
    elsif ($CATEGORY eq 'VARIABLE') { &finish_variable (*INTERFACE); }
    elsif ($CATEGORY eq 'MACRO') { &finish_macro (*INTERFACE); }
    else { &finish_unrecognized_interface (*INTERFACE); }
    &reset_interface (*INTERFACE);
}

sub parse_continuation {
    local (*INTERFACE) = @_;
    local (@LINES) = split (/^/, $INTERFACE{'CONTINUATION'});
    local ($LINE_DOWN);
    local ($INSIDE_DESCRIPTION) = 0;
    local ($INSIDE_EXAMPLE) = 0;
    foreach $LINE (@LINES) {
	$LINE_DOWN = &string_downcase ($LINE);
	if ($LINE_DOWN =~ /^\s*description:/) {
	    $INSIDE_DESCRIPTION = 1;
	    $INSIDE_EXAMPLE = 0;
	    $LINE =~ s/^\s*description:\s*//i;
	    $LINE = &string_trim_whitespace ($LINE);
	    $INTERFACE{'DESCRIPTION'} = $LINE;
	} elsif ($LINE_DOWN =~ /^\s*example:/) {
	    $INSIDE_EXAMPLE = 1;
	    $INSIDE_DESCRIPTION = 0;
	    $LINE =~ s/^\s*example:\s*//i;
	    $LINE = &string_trim_whitespace ($LINE);
	    $INTERFACE{'EXAMPLE'} = $LINE;
	} elsif (($INTERFACE{'CATEGORY'} eq 'CLASS') &&
		 ($LINE_DOWN =~ /^\s*superclasses:/)) {
	    $INSIDE_DESCRIPTION = 0;
	    $INSIDE_EXAMPLE = 0;
	    $LINE =~ s/^\s*superclasses:\s*//i;
	    $LINE = &string_trim_whitespace ($LINE);
	    $INTERFACE{'SUPERCLASSES'} = $LINE;
	} elsif (($INTERFACE{'CATEGORY'} eq 'CLASS') && 
		 ($LINE_DOWN =~ /^\s*init keywords:/)) {
	    $INSIDE_DESCRIPTION = 0;
	    $INSIDE_EXAMPLE = 0;
	    $LINE =~ s/^\s*init_keywords:\s*//i;
	    $LINE = &string_trim_whitespace ($LINE);
	    $INTERFACE{'INIT_KEYWORDS'} = $LINE;
	} elsif ($INSIDE_DESCRIPTION) {
	    $LINE = &string_trim_whitespace ($LINE);
	    if ($INTERFACE{'DESCRIPTION'}) {
		$INTERFACE{'DESCRIPTION'} = "$INTERFACE{'DESCRIPTION'}\n$LINE";
	    } else { $INTERFACE{'DESCRIPTION'} = $LINE; }
	} elsif ($INSIDE_EXAMPLE) {
	    $LINE =~ s/\s+$//;
	    if ($INTERFACE{'EXAMPLE'}) {
		$INTERFACE{'EXAMPLE'} = "$INTERFACE{'EXAMPLE'}\n$LINE";
	    } else { $INTERFACE{'EXAMPLE'} = $LINE; }
	} else {
	    $LINE = &string_trim_whitespace ($LINE);
	    if ($LINE) {
		if ($INTERFACE{'SIGNATURE'}) {
		    $INTERFACE{'SIGNATURE'} =
			"$INTERFACE{'SIGNATURE'} $LINE";
		} else { $INTERFACE{'SIGNATURE'} = $LINE; }
	    }
	}
    }
}

sub finish_type {
    local (*INTERFACE) = @_;
    &write_interface_summary ('');
    &start_online_conditional;
    &write_interface_library ($INTERFACE{'LIBRARY'});
    &write_interface_module ($INTERFACE{'MODULE'});
    &end_conditional;
    &write_interface_description ($INTERFACE{'DESCRIPTION'});
    &write_interface_example ($INTERFACE{'EXAMPLE'});
}

sub finish_class {
    local (*INTERFACE) = @_;
    &write_interface_summary ('');
    &start_online_conditional;
    &write_interface_library ($INTERFACE{'LIBRARY'});
    &write_interface_module ($INTERFACE{'MODULE'});
    &end_conditional;
    &write_interface_superclasses ($INTERFACE{'SUPERCLASSES'});
    &write_interface_init_keywords ($INTERFACE{'INIT_KEYWORDS'},
				    $INTERFACE{'NAME'});
    &write_interface_description ($INTERFACE{'DESCRIPTION'});
    &write_interface_operations ('');
    &write_interface_example ($INTERFACE{'EXAMPLE'});
}

sub finish_function {
    local (*INTERFACE) = @_;
    &parse_interface_parameters (*INTERFACE);
    &write_interface_summary ('');
    &start_online_conditional;
    &write_interface_library ($INTERFACE{'LIBRARY'});
    &write_interface_module ($INTERFACE{'MODULE'});
    &end_conditional;
    &write_interface_signature
	($INTERFACE{'NAME'}, $INTERFACE{'REQ_ARG_NAMES'},
	 $INTERFACE{'REQ_ARG_TYPES'}, $INTERFACE{'REST_ARG'},
	 $INTERFACE{'KEY_ARG_NAMES'}, $INTERFACE{'KEY_ARG_TYPES'},
	 $INTERFACE{'KEY_ARG_VALUES'},
	 $INTERFACE{'VALUE_NAMES'}, $INTERFACE{'VALUE_TYPES'});
    &write_interface_arguments
	($INTERFACE{'REQ_ARG_NAMES'}, $INTERFACE{'REQ_ARG_TYPES'},
	 $INTERFACE{'REST_ARG'}, $INTERFACE{'KEY_ARG_NAMES'},
	 $INTERFACE{'KEY_ARG_TYPES'}, $INTERFACE{'KEY_ARG_VALUES'});
    &write_interface_values ($INTERFACE{'VALUE_NAMES'},
			     $INTERFACE{'VALUE_TYPES'});
    &write_interface_exceptions ('');
    &write_interface_description ($INTERFACE{'DESCRIPTION'});
    &write_interface_example ($INTERFACE{'EXAMPLE'});
}

sub finish_macro {
    local (*INTERFACE) = @_;
    &parse_interface_macro_parameters (*INTERFACE);
    &write_interface_summary ('');
    &start_online_conditional;
    &write_interface_library ($INTERFACE{'LIBRARY'});
    &write_interface_module ($INTERFACE{'MODULE'});
    &end_conditional;
    &write_interface_macro_call ($INTERFACE{'NAME'},
				 $INTERFACE{'SIGNATURE'});
    &write_interface_arguments
	($INTERFACE{'REQ_ARG_NAMES'}, $INTERFACE{'REQ_ARG_TYPES'},
	 $INTERFACE{'REST_ARG'},
	 $INTERFACE{'KEY_ARG_NAMES'}, $INTERFACE{'KEY_ARG_VALUES'});
    &write_interface_values ($INTERFACE{'VALUE_NAMES'},
			     $INTERFACE{'VALUE_TYPES'});
    &write_interface_description ($INTERFACE{'DESCRIPTION'});
    &write_interface_example ($INTERFACE{'EXAMPLE'});
}

sub finish_constant {
    local (*INTERFACE) = @_;
    &parse_interface_variable_type_init (*INTERFACE);
    &write_interface_summary ('');
    &start_online_conditional;
    &write_interface_library ($INTERFACE{'LIBRARY'});
    &write_interface_module ($INTERFACE{'MODULE'});
    &end_conditional;
    &write_interface_value_type ($INTERFACE{'VALUE_TYPE'});
    &write_interface_description ($INTERFACE{'DESCRIPTION'});
    &write_interface_example ($INTERFACE{'EXAMPLE'});
}

sub finish_variable {
    local (*INTERFACE) = @_;
    &parse_interface_variable_type_init (*INTERFACE);
    &write_interface_summary ('');
    &start_online_conditional;
    &write_interface_library ($INTERFACE{'LIBRARY'});
    &write_interface_module ($INTERFACE{'MODULE'});
    &end_conditional;
    &write_interface_value_type ($INTERFACE{'VALUE_TYPE'});
    &write_interface_description ($INTERFACE{'DESCRIPTION'});
    &write_interface_example ($INTERFACE{'EXAMPLE'});
}

sub finish_unrecognized_interface {
    local (*INTERFACE) = @_;
    &write_interface_summary ('');
    &start_online_conditional;
    &write_interface_library ($INTERFACE{'LIBRARY'});
    &write_interface_module ($INTERFACE{'MODULE'});
    &end_conditional;
    &write_interface_description ($INTERFACE{'DESCRIPTION'});
    &write_interface_example ($INTERFACE{'EXAMPLE'});
}

sub parse_interface_parameters {
    local (*INTERFACE) = @_;
    local ($ARG_STRING, $VALUE_STRING, $REQ_STRING, $REST_STRING, $KEY_STRING);
    $ARG_STRING = $INTERFACE{'SIGNATURE'};
    if ($ARG_STRING =~ /(.*)=>(.*)/) {
	$ARG_STRING = &string_trim_whitespace ($1);
	$VALUE_STRING = &string_trim_whitespace ($2);
	$VALUE_STRING =~ s/^\((.*)\)$/$1/;
    }
    $ARG_STRING =~ s/^\((.*)\)$/$1/;
    $REQ_STRING = $ARG_STRING;
    if ($ARG_STRING =~ /(.*)\#key(.*)/) {
	$REQ_STRING = &string_trim_whitespace ($1);
	$KEY_STRING = &string_trim_whitespace ($2);
    }
    if ($REQ_STRING =~ /(.*)\#rest(.*)/) {
	$REQ_STRING = &string_trim_whitespace ($1);
	$REST_STRING = &string_trim_whitespace ($2);
    }
    &parse_interface_required_parameters (*INTERFACE, $REQ_STRING);
    if (defined ($REST_STRING)) { $INTERFACE{'REST_ARG'} = $REST_STRING;}
    &parse_interface_keyword_parameters (*INTERFACE, $KEY_STRING);
    &parse_interface_values (*INTERFACE, $VALUE_STRING);
}

sub parse_interface_required_parameters {
    local (*INTERFACE, $REQ_STRING) = @_;
    local (@CHUNKS, $CHUNK, @ARG_NAMES, @ARG_TYPES, $i);
    local ($ARG_NO) = -1;
    local ($COLLECTING_TYPE) = 0;
    local ($NPARENS) = 0;
    if (defined ($REQ_STRING)) {
	@CHUNKS = split (' ', $REQ_STRING);
      loop: for ($i = 0; $i < @CHUNKS; $i++) {
	  $CHUNK = $CHUNKS[$i];
	  if ($COLLECTING_TYPE) {
	      $NPARENS = $NPARENS + &count_parens ($CHUNK);
	      if (($NPARENS == 0) && ($CHUNK =~ /\,$/)) {
		  $CHUNK =~ s/\,$//;
		  $COLLECTING_TYPE = 0;
	      } elsif ($NPARENS < 0) {
		  print STDERR "Warning: For interface $INTERFACE{'NAME'} of type $INTERFACE{'TYPE'}, required portion of parameter list did not match expected pattern.\n";
		  print STDERR "Parameter list was: $INTERFACE{'SIGNATURE'}.\n";
		  last loop;
	      }
	      if ($ARG_TYPES[$ARG_NO]) {
		  $ARG_TYPES[$ARG_NO] = "$ARG_TYPES[$ARG_NO] $CHUNK";
	      } else { $ARG_TYPES[$ARG_NO] = $CHUNK; }
	  } else {
	      if (($CHUNK !~ /\,$/) &&
		  ($i < @CHUNKS - 1) && ($CHUNKS[$i + 1] eq '::')) {
		  $ARG_NO++;
		  $ARG_NAMES[$ARG_NO] = $CHUNK;
		  $ARG_TYPES[$ARG_NO] = '';
		  $COLLECTING_TYPE = 1;
		  $NPARENS = 0;
		  $i++;
	      } else {
		  $ARG_NO++;
		  $CHUNK =~ s/\,$//;
		  $ARG_NAMES[$ARG_NO] = $CHUNK;
		  $ARG_TYPES[$ARG_NO] = '';
	      }
	  }
      }
	for ($i = 0; $i < @ARG_TYPES; $i++) {
	    unless ($ARG_TYPES[$i]) {
		$ARG_TYPES[$i] = &find_arg_type_from_name ($ARG_NAMES[$i]);
	    }
	}
	$INTERFACE{'REQ_ARG_NAMES'} = join ('', @ARG_NAMES);
	$INTERFACE{'REQ_ARG_TYPES'} = join ('', @ARG_TYPES);
    }
}

sub find_arg_type_from_name {
    local ($NAME) = @_;
    local ($TYPE) = $ARG_NAME_TYPE_ALIST{$NAME};
    local ($SINGULAR);
    unless ($TYPE) {
	if ($NAME =~ /\?$/) { $TYPE = "<boolean>"; }
	else {
	    $SINGULAR = &string_singularize ($NAME);
	    if ($TYPE = $ARG_NAME_TYPE_ALIST{$SINGULAR}) {
		$TYPE = "limited(<sequence>, of: ${TYPE})";
	    }
	}
    }
    $TYPE;
}

sub parse_interface_keyword_parameters {
    local (*INTERFACE, $KEY_STRING) = @_;
    local (@CHUNKS, $CHUNK, @KEY_NAMES, @KEY_TYPES, @KEY_VALS, $i);
    local ($KEY_NO) = -1;
    local ($COLLECTING_TYPE) = 0;
    local ($COLLECTING_VALUE) = 0;
    local ($NPARENS) = 0;
    local ($PAREN_START) = 0;
    if (defined ($KEY_STRING)) {
	@CHUNKS = split (' ', $KEY_STRING);
      loop: for ($i = 0; $i < @CHUNKS; $i++) {
	  $CHUNK = $CHUNKS[$i];
	  if ($COLLECTING_TYPE) {
	      $NPARENS = $NPARENS + &count_parens ($CHUNK);
	      if ($NPARENS == (0 - $PAREN_START)) {
		  $COLLECTING_TYPE = 0;
		  if ((! $PAREN_START) && ($CHUNK !~ /\,$/) &&
		      ($i < @CHUNKS - 1) && ($CHUNKS[$i + 1] eq '=')) {
		      $COLLECTING_VALUE = 1;
		      $NPARENS = 0;
		      $i++;
		  }
		  $CHUNK =~ s/\,$//;
		  if ($PAREN_START) { $CHUNK =~ s/\)$//; }
	      } elsif (($NPARENS == 0) && $PAREN_START && ($CHUNK !~ /\,$/) &&
		       ($i < @CHUNKS - 1) && ($CHUNKS[$i + 1] eq '=')) {
		  $COLLECTING_TYPE = 0;
		  $COLLECTING_VALUE = 1;
		  $NPARENS = 0;
		  $i++;
	      } elsif ($NPARENS < (0 - $PAREN_START)) {
		  print STDERR "Warning: For interface $INTERFACE{'NAME'} of type $INTERFACE{'TYPE'}, keyword portion of parameter list did not match expected pattern.\n";
		  print STDERR "Parameter list was: $INTERFACE{'SIGNATURE'}.\n";
		  last loop;
	      }
	      if ($KEY_TYPES[$KEY_NO]) {
		  $KEY_TYPES[$KEY_NO] = "$KEY_TYPES[$KEY_NO] $CHUNK";
	      } else { $KEY_TYPES[$KEY_NO] = $CHUNK; }
	  } elsif ($COLLECTING_VALUE) {
	      $NPARENS = $NPARENS + &count_parens ($CHUNK);
	      if ($NPARENS == (0 - $PAREN_START)) {
		  $COLLECTING_VALUE = 0;
		  $CHUNK =~ s/\,$//;
		  if ($PAREN_START) { $CHUNK =~ s/\)$//; }
	      }
	      elsif ($NPARENS < (0 - $PAREN_START)) {
		  print STDERR "Warning: For interface $INTERFACE{'NAME'} of type $INTERFACE{'TYPE'}, keyword portion of parameter list did not match expected pattern.\n";
		  print STDERR "Parameter list was: $ARG_STRING.\n";
		  last loop;
	      }
	      if ($KEY_VALS[$KEY_NO]) {
		  $KEY_VALS[$KEY_NO] = "$KEY_VALS[$KEY_NO] $CHUNK";
	      } else { $KEY_VALS[$KEY_NO] = $CHUNK; }
	  } else {
	      if (($CHUNK =~ /^\(([^\(\)\,]+)\)\,?$/) ||
		  ($CHUNK =~ /^([^\(\)\,]+)\,$/)) {
		  $KEY_NO++;
		  $KEY_NAMES[$KEY_NO] = $1;
		  $KEY_TYPES[$KEY_NO] = '';
		  $KEY_VALS[$KEY_NO] = '';
	      } elsif ($CHUNK =~ /^([^\(\)\,]+)$/) {
		  if (($i < @CHUNKS - 1) && (($CHUNKS[$i + 1] eq '::') ||
					     ($CHUNKS[$i + 1] eq '='))) {
		      if ($CHUNKS[$i + 1] eq '::') { $COLLECTING_TYPE = 1; }
		      else { $COLLECTING_VALUE = 1; }
		      $NPARENS = 0;
		      $PAREN_START = 0;
		      $i++;
		  }
		  $KEY_NO++;
		  $KEY_NAMES[$KEY_NO] = $1;
		  $KEY_TYPES[$KEY_NO] = '';
		  $KEY_VALS[$KEY_NO] = '';
	      } elsif (($CHUNK =~ /^\(([^\(\)\,]+)$/) &&
		       ($i < @CHUNKS - 1) && (($CHUNKS[$i + 1] eq '::') ||
					      ($CHUNKS[$i + 1] eq '='))) {
		  if ($CHUNKS[$i + 1] eq '::') { $COLLECTING_TYPE = 1; }
		  else { $COLLECTING_VALUE = 1; }
		  $NPARENS = 0;
		  $PAREN_START = 1;
		  $i++;
		  $KEY_NO++;
		  $KEY_NAMES[$KEY_NO] = $1;
		  $KEY_TYPES[$KEY_NO] = '';
		  $KEY_VALS[$KEY_NO] = '';
	      } else {
		  print STDERR "Warning: For interface $INTERFACE{'NAME'} of type $INTERFACE{'TYPE'}, keyword portion of parameter list did not match expected pattern.\n";
		  print STDERR "Parameter list was: $ARG_STRING.\n";
		  last loop;
	      }
	  }
      }
	for ($i = 0; $i < @KEY_TYPES; $i++) {
	    unless ($KEY_TYPES[$i]) {
		$KEY_TYPES[$i] = &find_arg_type_from_name ($KEY_NAMES[$i]);
	    }
	}
	$INTERFACE{'KEY_ARG_NAMES'} = join ('', @KEY_NAMES);
	$INTERFACE{'KEY_ARG_TYPES'} = join ('', @KEY_TYPES);
	$INTERFACE{'KEY_ARG_VALUES'} = join ('', @KEY_VALS);
    }
}

sub parse_interface_macro_parameters {
    local (*INTERFACE) = @_;
    local ($ARG_STRING) = $INTERFACE{'SIGNATURE'};
    local (@CHUNKS) = split (/([${MACRO_CALL_DELIMITERS}]+)/, $ARG_STRING);
    local (@ARGS);
    foreach $CHUNK (@CHUNKS) {
	if ($CHUNK &&
	    ($CHUNK !~ /[${MACRO_CALL_DELIMITERS}]|^\#|^end$|^body$/)) {
            push (@ARGS, $CHUNK);
	}
    $INTERFACE{'REQ_ARG_NAMES'} = join ('', @ARGS);
    }
}

sub parse_interface_values {
    local (*INTERFACE, $VALUE_STRING) = @_;
    local (@CHUNKS, $CHUNK, @VALUE_NAMES, @VALUE_TYPES, $i);
    local ($VALUE_NO) = -1;
    local ($COLLECTING_TYPE) = 0;
    local ($NPARENS) = 0;
    if (defined ($VALUE_STRING)) {
	@CHUNKS = split (' ', $VALUE_STRING);
      loop: for ($i = 0; $i < @CHUNKS; $i++) {
	  $CHUNK = $CHUNKS[$i];
	  if ($COLLECTING_TYPE) {
	      $NPARENS = $NPARENS + &count_parens ($CHUNK);
	      if (($NPARENS == 0) && ($CHUNK =~ /\,$/)) {
		  $CHUNK =~ s/\,$//;
		  $COLLECTING_TYPE = 0;
	      } elsif ($NPARENS < 0) {
		  print STDERR "Warning: For interface $INTERFACE{'NAME'} of type $INTERFACE{'TYPE'}, values portion of parameter list did not match expected pattern.\n";
		  print STDERR "Parameter list was: $INTERFACE{'SIGNATURE'}.\n";
		  last loop;
	      }
	      if ($VALUE_TYPES[$VALUE_NO]) {
		  $VALUE_TYPES[$VALUE_NO] = "$VALUE_TYPES[$VALUE_NO] $CHUNK";
	      } else { $VALUE_TYPES[$VALUE_NO] = $CHUNK; }
	  } else {
	      if ($CHUNK =~ /\(/) {
		  $VALUE_NO++;
		  $VALUE_NAMES[$VALUE_NO] = 'value';
		  $VALUE_TYPES[$VALUE_NO] = '';
		  $COLLECTING_TYPE = 1;
		  $NPARENS = 0;
		  $i--;
	      } elsif (($CHUNK !~ /\,$/) &&
		       ($i < @CHUNKS - 1) && ($CHUNKS[$i + 1] eq '::')) {
		  $VALUE_NO++;
		  $VALUE_NAMES[$VALUE_NO] = $CHUNK;
		  $VALUE_TYPES[$VALUE_NO] = '';
		  $COLLECTING_TYPE = 1;
		  $NPARENS = 0;
		  $i++;
	      } else {
		  $VALUE_NO++;
		  $CHUNK =~ s/\,$//;
		  $VALUE_NAMES[$VALUE_NO] = $CHUNK;
		  $VALUE_TYPES[$VALUE_NO] = '';
	      }
	  }
      }
	for ($i = 0; $i < @VALUE_TYPES; $i++) {
	    unless ($VALUE_TYPES[$i] ||
		    (($i > 0) && ($VALUE_NAMES[$i - 1] eq '#rest'))) {
		$VALUE_TYPES[$i] = &find_arg_type_from_name ($VALUE_NAMES[$i]);
	    }
	}
	$INTERFACE{'VALUE_NAMES'} = join ('', @VALUE_NAMES);
	$INTERFACE{'VALUE_TYPES'} = join ('', @VALUE_TYPES);
    }
}

sub count_parens {
    local ($STRING) = @_;
    local ($LEFT) = ($STRING =~ tr/\(/\(/);
    local ($RIGHT) = ($STRING =~ tr/\)/\)/);
    $LEFT - $RIGHT;
}

sub parse_interface_variable_type_init {
    local (*INTERFACE) = @_;
    local ($TYPE_INIT_STRING) = $INTERFACE{'SIGNATURE'};
    local ($TYPE_STRING, $INIT_STRING);
    if ($TYPE_INIT_STRING =~ /::(.*)=(.*)/) {
	$TYPE_STRING = $1;
	$INIT_STRING = $2;
    } elsif ($TYPE_INIT_STRING =~ /::(.*)/) { $TYPE_STRING = $1; }
    elsif ($TYPE_INIT_STRING =~ /=(.*)/) { $INIT_STRING = $1; }
    if (defined ($TYPE_STRING)) {
	$INTERFACE{'VALUE_TYPE'} = &string_trim_whitespace ($TYPE_STRING);
    }
    if (defined ($INIT_STRING)) {
	$INTERFACE{'INIT_VALUE'} = &string_trim_whitespace ($INIT_STRING);
    }
}

## Mid-level routines for writing components of the document.
## These don't do any direct output, but instead communicate
## with the low-level MIF-writing routines.

sub write_library_heading {
    local ($LIBRARY) = @_;
    local (@INDEX_ENTRIES) =
	&generate_mif_literal_index_entries (&string_downcase ($LIBRARY),
					     'library');
    &write_mif_simple_para_with_index ($LIBRARY_PARA, "$LIBRARY Library", '',
				       @INDEX_ENTRIES);
}

sub write_module_heading {
    local ($MODULE) = @_;
    local (@INDEX_ENTRIES) =
	&generate_mif_literal_index_entries (&string_downcase ($MODULE),
					     'module');
    &write_mif_simple_para_with_index ($MODULE_PARA, "$MODULE Module", '',
				       @INDEX_ENTRIES);
}

sub write_interface_name {
    local ($NAME, $TYPE, $LIBRARY, $MODULE) = @_;
    local (@INDEX_ENTRIES);
    $TYPE =~ s/.*\s(\S+)\s+(\S+)$/$1 $2/;
    $TYPE = &string_downcase ($TYPE);
    if ($TYPE ne 'generic function') {
	$TYPE =~ s/.*\s(\S+)$/$1/;
    }
    @INDEX_ENTRIES =
	&generate_mif_interface_index_entries ($NAME, $TYPE, $LIBRARY,
					       $MODULE);
    &write_mif_simple_para_with_index ($INTERFACE_PARA, $NAME, '',
				       @INDEX_ENTRIES);
}

sub write_interface_type {
    local ($TYPE) = @_;
    &write_mif_simple_para ($INTERFACE_TYPE_PARA, $TYPE, '');
}

sub write_interface_summary {
    local ($SUMMARY) = @_;
    &write_mif_subentry ('Summary', '');
    &write_mif_simple_para ($BODY_ENTRY_PARA, $SUMMARY, '');
}

sub write_interface_library {
    local ($LIBRARY) = @_;
    &write_mif_subentry ('Library', '');
    &write_mif_simple_para ($SIGNATURE_PARA, $LIBRARY, '');
}

sub write_interface_module {
    local ($MODULE) = @_;
    &write_mif_subentry ('Module', '');
    &write_mif_simple_para ($SIGNATURE_PARA, $MODULE, '');
}

sub write_interface_description {
    local ($DESCRIPTION) = @_;
    &write_interface_multiline_field ('Description', $DESCRIPTION);
}

sub write_interface_example {
    local ($EXAMPLE) = @_;
    if ($EXAMPLE) { &write_interface_multiline_field ('Example', $EXAMPLE); }
}

sub write_interface_multiline_field {
    local ($FIELD, $TEXT) = @_;
    local (@LINES) = split (/^/, $TEXT);
    local ($LINE, @STRINGS, $STRING, $PARA_TAG, $i, $j);
    local ($STYLE) = '';
    local ($IN_PARA) = 0;
    local ($IN_STYLE) = 0;
    local ($FIRST_LINE) = 1;
    local ($INIT_SPACES) = 0;
    local ($CODE_START) = "@c{";
    local ($VARIABLE_START) = "@v{";
    local ($STYLE_END) = "}";
    local ($CODE_START_PAT, $VAR_START_PAT, $STYLE_END_PAT, $STYLE_PAT);
    ($CODE_START_PAT = $CODE_START) =~ s/(\W)/\\$1/g;
    ($VAR_START_PAT = $VARIABLE_START) =~ s/(\W)/\\$1/g;
    ($STYLE_END_PAT = $STYLE_END) =~ s/(\W)/\\$1/g;
    $STYLE_PAT = "${CODE_START_PAT}|${VAR_START_PAT}|${STYLE_END_PAT}";
    if ($FIELD eq 'Example') { $PARA_TAG = $CODE_LINE_PARA; }
    else { $PARA_TAG = $BODY_ENTRY_PARA; }
    &write_mif_subentry ($FIELD, '');
    for ($i = 0; $i < @LINES; $i++) {
	$LINE = $LINES[$i];
	$LINE =~ s/\n$//;
	if ($LINE) {
	    if ($IN_PARA) { &write_mif_string (' ', ''); }
	    else {
		if ($FIELD eq 'Example') {
		    if (($i == 0) || ($LINES[$i - 1] =~ /^$/)) {
			if (($i == @LINES - 1) || ($LINES[$i + 1] =~ /^$/)) {
			    $PARA_TAG = $CODE_LINE_PARA;
			} else { $PARA_TAG = $CODE_FIRST_PARA; }
		    } elsif (($i == @LINES - 1) || ($LINES[$i + 1] =~ /^$/)) {
			$PARA_TAG = $CODE_LAST_PARA;
		    } else { $PARA_TAG = $CODE_BODY_PARA; }
		}
		($IN_PARA, $IN_STYLE, $STYLE) =
		    &multiline_field_para_start ($FIELD, $PARA_TAG, $IN_PARA,
						$IN_STYLE, $STYLE);
	    }
	    @STRINGS = split (/(${STYLE_PAT})/, $LINE);
            for ($j = 0; $j < @STRINGS; $j++) {
		$STRING = $STRINGS[$j];
		if ($j == 0) {
		    ($IN_PARA, $IN_STYLE, $STYLE, $STRING, $INIT_SPACES) = 
			&multiline_field_line_start ($FIELD, $PARA_TAG, $IN_PARA, $IN_STYLE, $STYLE, $STRING, $FIRST_LINE, $INIT_SPACES);
		}
		if (($STRING eq $STYLE_END) && $IN_STYLE) {
		    $STYLE = '';
		    &write_mif_font ($STYLE);
		    $IN_STYLE = 0;
		} elsif (($STRING eq $CODE_START) ||
			 ($STRING eq $VARIABLE_START)) {
		    if ($IN_STYLE) {
			print STDERR "Warning: Illegal nested style change in $FIELD field.\n";
			print STDERR "Illegal line: $LINE\n";
			&write_mif_font ('');
		    }
		    if ($STRING eq $CODE_START) { $STYLE = $CODE_STYLE; }
		    if ($STRING eq $VARIABLE_START) { $STYLE = $VARIABLE_STYLE; }
		    &write_mif_font ($STYLE);
		    $IN_STYLE = 1;
		} else { &write_mif_string ($STRING, ''); }
	    }
	    ($IN_PARA, $IN_STYLE, $STYLE) = 
		&multiline_field_line_end ($FIELD, $PARA_TAG, $IN_PARA,
					  $IN_STYLE, $STYLE);
            $FIRST_LINE = 0;
        } else {
	    ($IN_PARA, $IN_STYLE, $STYLE) =
		&multiline_field_blank_line ($FIELD, $PARA_TAG, $IN_PARA, $IN_STYLE, $STYLE);
	}
    }
    if ($FIRST_LINE) {
	($IN_PARA, $IN_STYLE, $STYLE) =
	    &multiline_field_para_start ($FIELD, $PARA_TAG, $IN_PARA,
					 $IN_STYLE, $STYLE);
    }
    &multiline_field_para_end ($FIELD, $PARA_TAG, $IN_PARA, $IN_STYLE, $STYLE);
}

sub multiline_field_para_start {
    local ($FIELD, $PARA_TAG, $IN_PARA, $IN_STYLE, $STYLE) = @_;
    if (! $IN_PARA) {
	&write_mif_para_start ($PARA_TAG);
	&write_mif_para_line_start;
	$IN_PARA = 1;
    }
    ($IN_PARA, $IN_STYLE, $STYLE);
}

sub multiline_field_para_end {
    local ($FIELD, $PARA_TAG, $IN_PARA, $IN_STYLE, $STYLE) = @_;
    if ($IN_PARA) {
	if ($IN_STYLE) {
	    print STDERR "Warning: Illegal unterminated style change at end of paragraph in $FIELD field.\n";
	    $STYLE = '';
	    &write_mif_font ($STYLE);
	    $IN_STYLE = 0;
	}
	&write_mif_para_line_end;
	&write_mif_para_end;
	$IN_PARA = 0;
    }
    ($IN_PARA, $IN_STYLE, $STYLE);
}

sub multiline_field_line_start {
    local ($FIELD, $PARA_TAG, $IN_PARA, $IN_STYLE, $STYLE, $STRING,
	   $FIRST_LINE, $INIT_SPACES) = @_;
    local ($WHITE, $NSPACE, $i);
    if ($FIELD eq 'Example') {
	$STRING =~ s/(^[ \t]+)//;
	($WHITE = $1) =~ s/\t/  /g;
	if ($FIRST_LINE) { $INIT_SPACES = length ($WHITE); }
	else {
	    $NSPACE = length ($WHITE) - $INIT_SPACES;
	    for ($i = 0; $i < $NSPACE; $i++) {
		&write_mif_hard_space;
	    }
	}
    }
    ($IN_PARA, $IN_STYLE, $STYLE, $STRING, $INIT_SPACES);
}

sub multiline_field_line_end {
    local ($FIELD, $PARA_TAG, $IN_PARA, $IN_STYLE, $STYLE) = @_;
    if ($FIELD eq 'Example') {
	($IN_PARA, $IN_STYLE, $STYLE) =
	    &multiline_field_para_end ($FIELD, $PARA_TAG, $IN_PARA, $IN_STYLE,
				       $STYLE);
    }
    ($IN_PARA, $IN_STYLE, $STYLE);
}

sub multiline_field_blank_line {
    local ($FIELD, $PARA_TAG, $IN_PARA, $IN_STYLE, $STYLE) = @_;
    ($IN_PARA, $IN_STYLE, $STYLE) =
	&multiline_field_para_end ($FIELD, $PARA_TAG, $IN_PARA, $IN_STYLE,
				   $STYLE);
}

sub write_interface_superclasses {
    local ($SUPERCLASSES_STRING) = @_;
    &write_mif_subentry ('Superclasses', '');
    &write_mif_simple_para ($SIGNATURE_PARA, $SUPERCLASSES_STRING, '');
}

sub write_interface_init_keywords {
    local ($INIT_KEYWORDS_STRING, $CLASS) = @_;
    local (@INIT_KEYWORDS) = split (' ', $INIT_KEYWORDS_STRING);
    local (@INDEX_ENTRIES, $i);
    &write_mif_subentry ('Init-keywords', '');
    if (! defined ($INIT_KEYWORDS_STRING)) {
	&write_mif_description ($DESCRIPTION_ENTRY_PARA, '', '', undef, undef);
    } elsif (! @INIT_KEYWORDS) {
	&write_mif_simple_para ($BODY_ENTRY_PARA, 'None', '');
    } else {
	for ($i = 0; $i < @INIT_KEYWORDS; $i++) {
	    @INDEX_ENTRIES =
		&generate_mif_init_keyword_index_entries ($INIT_KEYWORDS[$i],
							  $CLASS);
	    if ($i == 0) {
		&write_mif_description_with_index
		    ($DESCRIPTION_ENTRY_PARA, $INIT_KEYWORDS[$i], $CODE_STYLE,
		     '', '', @INDEX_ENTRIES);
	    } else {
		&write_mif_description_with_index
		    ($DESCRIPTION_PARA, $INIT_KEYWORDS[$i], $CODE_STYLE,
		     '', '', @INDEX_ENTRIES);
	    }
	}
    }
}

sub write_interface_operations {
    local ($OPERATIONS_STRING) = @_;
    local (@OPERATIONS) = split (//, $OPERATIONS_STRING);
    local ($i);
    &write_mif_subentry ('Operations', '');
    if (! @OPERATIONS) {
	&write_mif_description ($DESCRIPTION_ENTRY_PARA, '', '', undef, undef);
    } else {
	for ($i = 0; $i < @OPERATIONS; $i++) {
	    if ($i == 0) {
		&write_mif_description
		    ($DESCRIPTION_ENTRY_PARA, $OPERATION, $CODE_STYLE, '', '');
	    } else {
		&write_mif_description
		    ($DESCRIPTION_PARA, $OPERATION, $CODE_STYLE, '', '');
	    }
	}
    }
}

sub write_interface_signature {
    local ($NAME, $REQ_ARG_NAMES, $REQ_ARG_TYPE, $REST_ARG, $KEY_ARG_NAMES,
	   $KEY_ARG_TYPES, $KEY_ARG_VALUES, $VALUE_NAMES, $VALUE_TYPES) = @_;
    &write_mif_subentry ('Signature', '');
    &write_mif_para_start ($SIGNATURE_PARA);
    &write_mif_para_line_start;
    &write_mif_string ($NAME, '');
    foreach $REQ_ARG (split (//, $REQ_ARG_NAMES)) {
	&write_mif_string (' ', '');
	&write_mif_string ($REQ_ARG, $VARIABLE_STYLE);
    }
    if ($REST_ARG) {
	&write_mif_string (' #rest ', '');
	&write_mif_string ($REST_ARG, $VARIABLE_STYLE);
    }
    if (defined ($KEY_ARG_NAMES)) {
	&write_mif_string (' #key', '');
	foreach $KEY_ARG (split (//, $KEY_ARG_NAMES)) {
	    &write_mif_string (' ', '');
	    if ($KEY_ARG eq '#all-keys') {
		&write_mif_string ($KEY_ARG, '');
	    } else { &write_mif_string ($KEY_ARG, $VARIABLE_STYLE); }
	}
    }
    &write_mif_string (' =>', '');
    if (defined ($VALUE_NAMES)) {
	if (! $VALUE_NAMES) {
	    &write_mif_string (' ()', '');
	} else {
	    foreach $VALUE (split (//, $VALUE_NAMES)) {
		&write_mif_string (' ', '');
		if ($VALUE eq '#rest') {
		    &write_mif_string ($VALUE, '');
		} else {
		    &write_mif_string ($VALUE, $VARIABLE_STYLE);
		}
	    }
	}
    }
    &write_mif_para_line_end;
    &write_mif_para_end;
}

sub write_interface_macro_call {
    local ($NAME, $SIGNATURE) = @_;
    local (@CHUNKS);
    &write_mif_subentry ('Macro call', '');
    &write_mif_para_start ($SIGNATURE_PARA);
    &write_mif_para_line_start;
    &write_mif_string ($NAME, '');
    if ($SIGNATURE) {
	@CHUNKS = split (/([${MACRO_CALL_DELIMITERS}]+)/, $SIGNATURE);
	&write_mif_string (' ', '');
	foreach $CHUNK (@CHUNKS) {
	    if ($CHUNK =~ /[${MACRO_CALL_DELIMITERS}]|^\#|^end$/) {
		&write_mif_string ($CHUNK, '');
	    } else { &write_mif_string ($CHUNK, $VARIABLE_STYLE); }
	}
    }
    &write_mif_para_line_end;
    &write_mif_para_end;
}

sub write_interface_arguments {
    local ($REQ_ARG_NAMES, $REQ_ARG_TYPES, $REST_ARG, $KEY_ARG_NAMES,
	   $KEY_ARG_TYPES, $KEY_ARG_VALUES) = @_;
    local ($FIRST_ARG) = 1;
    local (@ARG_NAMES, @ARG_TYPES, @KEY_NAMES, @KEY_TYPES, @KEY_VALUES, $i);
    &write_mif_subentry ('Arguments', '');
    if ((! $REQ_ARG_NAMES) && (! $REST_ARG) && (! defined ($KEY_ARG_NAMES))) {
	&write_mif_simple_para ($BODY_ENTRY_PARA, 'None', '');
    } else {
	@ARG_NAMES = split (//, $REQ_ARG_NAMES);
	@ARG_TYPES = split (//, $REQ_ARG_TYPES);
	for ($i = 0; $i < @ARG_NAMES; $i++) {
	    if ($FIRST_ARG) {
		$FIRST_ARG = 0;
		&write_mif_description_start
		    ($DESCRIPTION_ENTRY_PARA, $ARG_NAMES[$i], $VARIABLE_STYLE);
	    } else {
		&write_mif_description_start
		    ($DESCRIPTION_PARA, $ARG_NAMES[$i], $VARIABLE_STYLE);
	    }
	    if ($ARG_TYPES[$i]) {
		&write_mif_string ('An instance of type ', '');
		&write_mif_string ($ARG_TYPES[$i], $CODE_STYLE);
		&write_mif_string ('.', '');
	    }
	    &write_mif_description_end;
	}
	if ($REST_ARG) {
	    if ($FIRST_ARG) {
		$FIRST_ARG = 0;
		&write_mif_description
		    ($DESCRIPTION_ENTRY_PARA, $REST_ARG, $VARIABLE_STYLE, '', '');
	    } else {
		&write_mif_description ($DESCRIPTION_PARA, $REST_ARG,
					$VARIABLE_STYLE, '', '');
	    }
	}
	@KEY_NAMES = split (//, $KEY_ARG_NAMES);
	@KEY_TYPES = split (//, $KEY_ARG_TYPES);
	@KEY_VALUES = split (//, $KEY_ARG_VALUES);
	for ($i = 0; $i < @KEY_NAMES; $i++) {
	    unless ($KEY_NAMES[$i] eq '#all-keys') {
		if ($FIRST_ARG) {
		    $FIRST_ARG = 0;
		    &write_mif_description_start
			($DESCRIPTION_ENTRY_PARA, $KEY_NAMES[$i],
			 $VARIABLE_STYLE);
		} else {
		    &write_mif_description_start
			($DESCRIPTION_PARA, $KEY_NAMES[$i], $VARIABLE_STYLE);
		}
		if ($KEY_TYPES[$i]) {
		    &write_mif_string ('An instance of type ', '');
		    &write_mif_string ($KEY_TYPES[$i], $CODE_STYLE);
		    if ($KEY_VALUES[$i]) { &write_mif_string ('. ', ''); }
		    else { &write_mif_string ('.', ''); }
		}
		if ($KEY_VALUES[$i]) {
		    &write_mif_string ('Default value: ', '');
		    &write_mif_string ($KEY_VALUES[$i], $CODE_STYLE);
		    &write_mif_string ('.', '');
		}
		&write_mif_description_end;
	    }
	}
    }
}

sub write_interface_values {
    local ($VALUE_NAMES, $VALUE_TYPES) = @_;
    local (@NAMES, @TYPES, $i);
    &write_mif_subentry ('Values', '');
    if (! defined ($VALUE_NAMES)) {
	&write_mif_description ($DESCRIPTION_ENTRY_PARA, '', '', undef, undef);
    } elsif (! $VALUE_NAMES) {
	&write_mif_simple_para ($BODY_ENTRY_PARA, 'None', '');
    } else {
	@NAMES = split (//, $VALUE_NAMES);
	@TYPES = split (//, $VALUE_TYPES);
	for ($i = 0; $i < @NAMES; $i++) {
	    unless ($NAMES[$i] eq '#rest') {
		if ($i == 0) {
		    &write_mif_description_start
			($DESCRIPTION_ENTRY_PARA, $NAMES[$i], $VARIABLE_STYLE);
		} else {
		    &write_mif_description_start
			($DESCRIPTION_PARA, $NAMES[$i], $VARIABLE_STYLE);
		}
		if ($TYPES[$i]) {
		    &write_mif_string ('An instance of type ', '');
		    &write_mif_string ($TYPES[$i], $CODE_STYLE);
		    &write_mif_string ('.', '');
		}
		&write_mif_description_end;
	    }
	}
    }
}

sub write_interface_exceptions {
    local ($EXCEPTIONS_STRING) = @_;
    &write_mif_subentry ('Exceptions', '');
    &write_mif_simple_para ($BODY_ENTRY_PARA, $EXCEPTIONS_STRING, '');
}

sub write_interface_value_type {
    local ($TYPE) = @_;
    &write_mif_subentry ('Type', '');
    &write_mif_simple_para ($SIGNATURE_PARA, $TYPE, '');
}

sub write_interface_value {
    local ($VALUE) = @_;
    &write_mif_subentry ('Value', '');
    &write_mif_simple_para ($SIGNATURE_PARA, $VALUE, '');
}

sub write_interface_initial_value {
    local ($INITIAL_VALUE) = @_;
    &write_mif_subentry ('Initial value', '');
    &write_mif_simple_para ($SIGNATURE_PARA, $INITIAL_VALUE, '');
}

sub start_online_conditional {
    &start_mif_conditional ($ONLINE_CONDITION_TAG);
}

sub end_conditional {
    &end_mif_conditional;
}

## Routines to generate index entries.
## This is midway between the middle and bottom layers, since it
## has to know about both Dylan and MIF.

sub generate_mif_interface_index_entries {
    local ($NAME, $TYPE, $LIBRARY, $MODULE) = @_;
    local ($SORT_NAME, $TYPE_HEAD);
    $TYPE_HEAD = &quote_mif_index_contents (&string_pluralize ($TYPE));
    $TYPE = &quote_mif_index_contents ($TYPE);
    if ($NAME =~ /^[${INDEX_LEADING_CHARS_TO_STRIP}]/) {
        ($SORT_NAME = $NAME) =~ s/^.//;
	if ($SORT_NAME) {
	    $SORT_NAME = &quote_mif_index_contents ($SORT_NAME);
	    $SORT_NAME = "[;${SORT_NAME} ${TYPE}]";
	}
    }
    $NAME = &quote_mif_index_contents ($NAME);
    ("<${CODE_STYLE}>${NAME}<Default Para Font> ${TYPE}${SORT_NAME}",
     "<${CODE_STYLE}>${LIBRARY}<Default Para Font> library interfaces:<${CODE_STYLE}>${NAME}<Default Para Font> ${TYPE}",
     "${TYPE_HEAD}:<${CODE_STYLE}>${NAME}<Default Para Font>");
}

sub generate_mif_init_keyword_index_entries {
    local ($KEYWORD, $CLASS) = @_;
    local ($SORT_NAME);
    $CLASS = &quote_mif_index_contents ($CLASS);
    if ($KEYWORD =~ /^[${INDEX_LEADING_CHARS_TO_STRIP}]/) {
        ($SORT_NAME = $KEYWORD) =~ s/^.//;
	if ($SORT_NAME) {
	    $SORT_NAME = &quote_mif_index_contents ($SORT_NAME);
	    $SORT_NAME = "[;${SORT_NAME} init-keyword:for ${CLASS}]";
	}
    }
    $KEYWORD = &quote_mif_index_contents ($KEYWORD);
    ("<${CODE_STYLE}>${KEYWORD}<Default Para Font> init-keyword:for <${CODE_STYLE}>${CLASS}<Default Para Font>${SORT_NAME}",
     "init-keywords:<${CODE_STYLE}>${KEYWORD}<Default Para Font>:for <${CODE_STYLE}>${CLASS}<Default Para Font>");
}

sub generate_mif_literal_index_entries {
    local ($LITERAL, $TYPE) = @_;
    local ($SORT_NAME, $TYPE_HEAD);
    $TYPE_HEAD = &quote_mif_index_contents (&string_pluralize ($TYPE));
    $TYPE = &quote_mif_index_contents ($TYPE);
    if ($LITERAL =~ /^[${INDEX_LEADING_CHARS_TO_STRIP}]/) {
        ($SORT_NAME = $LITERAL) =~ s/^.//;
	if ($SORT_NAME) {
	    $SORT_NAME = &quote_mif_index_contents ($SORT_NAME);
	    $SORT_NAME = "[;${SORT_NAME} ${TYPE}]";
	}
    }
    $LITERAL = &quote_mif_index_contents ($LITERAL);
    ("<${CODE_STYLE}>${LITERAL}<Default Para Font> ${TYPE}${SORT_NAME}",
     "${TYPE_HEAD}:<${CODE_STYLE}>${LITERAL}<Default Para Font>");
}

## Low-level routines that emit MIF output.
## These should be the only routines that print directly to the
## output stream.

sub write_mif_initial {
    &print_mif ("<MIFFile ${MIF_VERSION}>\n");
}

sub write_mif_subentry {
    local ($STRING, $STYLE) = @_;
    &write_mif_simple_para ($SUBENTRY_PARA, $STRING, $STYLE);
}

sub write_mif_description_with_index {
    local ($TAG, $LABEL, $LABEL_STYLE, $TEXT, $TEXT_STYLE, @MARKER_STRINGS) = @_;
    &write_mif_description_start_no_tab ($TAG, $LABEL, $LABEL_STYLE);
    foreach $MARKER_TEXT (@MARKER_STRINGS) {
	&write_mif_marker ($INDEX_MARKER_TYPE, $MARKER_TEXT);
    }
    if (defined ($TEXT)) {
	&write_mif_tab;
	&write_mif_string ($TEXT, $TEXT_STYLE);
    }
    &write_mif_description_end;
}

sub write_mif_description {
    local ($TAG, $LABEL, $LABEL_STYLE, $TEXT, $TEXT_STYLE) = @_;
    &write_mif_description_start_no_tab ($TAG, $LABEL, $LABEL_STYLE);
    if (defined ($TEXT)) {
	&write_mif_tab;
	&write_mif_string ($TEXT, $TEXT_STYLE);
    }
    &write_mif_description_end;
}

sub write_mif_description_start {
    local ($TAG, $LABEL, $LABEL_STYLE) =  @_;
    &write_mif_description_start_no_tab ($TAG, $LABEL, $LABEL_STYLE);
    &write_mif_tab;
}

sub write_mif_description_start_no_tab {
    local ($TAG, $LABEL, $LABEL_STYLE) =  @_;
    &write_mif_para_start ($TAG);
    &write_mif_para_line_start;
    &write_mif_string ($LABEL, $LABEL_STYLE);
}

sub write_mif_description_end {
    &write_mif_para_line_end;
    &write_mif_para_end;
}

sub write_mif_simple_para_with_index {
    local ($TAG, $STRING, $STYLE, @MARKER_STRINGS) = @_;
    &write_mif_para_start ($TAG);
    &write_mif_para_line_start;
    &write_mif_string ($STRING, $STYLE);
    foreach $MARKER_TEXT (@MARKER_STRINGS) {
	&write_mif_marker ($INDEX_MARKER_TYPE, $MARKER_TEXT);
    }
    &write_mif_para_line_end;
    &write_mif_para_end;
}

sub write_mif_simple_para {
    local ($TAG, $STRING, $STYLE) = @_;
    &write_mif_para_start ($TAG);
    &write_mif_para_line_start;
    &write_mif_string ($STRING, $STYLE);
    &write_mif_para_line_end;
    &write_mif_para_end;
}

sub write_mif_para_start {
    local ($TAG) = @_;
    $TAG = &quote_mif_string ($TAG);
    &print_mif ("<Para\n");
    $MIF_LEVEL++;
    &print_mif ("<PgfTag \`${TAG}\'>\n");
}

sub write_mif_para_end {
    $MIF_LEVEL--;
    &print_mif ("> \# end of Para\n");
}

sub write_mif_para_line_start {
    &print_mif ("<ParaLine\n");
    $MIF_LEVEL++;
    if ($MIF_CONDITIONAL_PENDING) {
	&write_mif_conditional (@MIF_PENDING_CONDITIONS);
	$MIF_CONDITIONAL_PENDING = 0;
    }
}

sub write_mif_para_line_end {
    $MIF_LEVEL--;
    &print_mif (">\n");
}

sub write_mif_marker {
    local ($MARKER_TYPE, $MARKER_TEXT) = @_;
    &print_mif ("<Marker\n");
    $MIF_LEVEL++;
    &print_mif ("<MType ${MARKER_TYPE}>\n");
    $MARKER_TEXT = &quote_mif_string ($MARKER_TEXT);
    &print_mif ("<MText \`${MARKER_TEXT}\'>\n");
    $MIF_LEVEL--;
    &print_mif ("> \# end of Marker\n");
}

sub start_mif_conditional {
    local (@CONDS) = @_;
    $MIF_CONDITIONAL_PENDING = 1;
    @MIF_PENDING_CONDITIONS = @CONDS;
}

sub end_mif_conditional {
    $MIF_CONDITIONAL_PENDING = 1;
    @MIF_PENDING_CONDITIONS = ();
}

sub write_mif_conditional {
    local (@CONDS) = @_;
    if (@CONDS) {
	&print_mif ("<Conditional\n");
	$MIF_LEVEL++;
	foreach $COND (@CONDS) {
	    &write_mif_in_condition ($COND);
	}
	$MIF_LEVEL--;
	&print_mif ("> \# end of Conditional\n");
    } else { &write_mif_unconditional; }
}

sub write_mif_in_condition {
    local ($COND) = @_;
    $COND = &quote_mif_string ($COND);
    &print_mif ("<InCondition `${COND}'>\n");
}

sub write_mif_unconditional {
    &print_mif ("<Unconditional >\n");
}

sub write_mif_string {
    local ($STRING, $STYLE) = @_;
    if ($STRING) {
	if ($STYLE) { &write_mif_font ($STYLE); }
	$STRING = &quote_mif_string ($STRING);
	&print_mif ("<String \`${STRING}\'>\n");
	if ($STYLE) { &write_mif_font (''); }
    }
}

sub quote_mif_string {
    local ($STRING) = @_;
    $STRING =~ s/([\>\'\\])/\\$1/g;
    $STRING;
}

sub quote_mif_index_contents {
    local ($STRING) = @_;
    $STRING =~ s/([\<\:\;\[\]])/\\$1/g;
    $STRING;
}

sub write_mif_font {
    local ($FONT) = @_;
    $FONT = &quote_mif_string ($FONT);
    &print_mif ("<Font\n");
    $MIF_LEVEL++;
    &print_mif ("<FTag \`${FONT}\'>\n");
    &print_mif ("<FLocked No>\n");
    $MIF_LEVEL--;
    &print_mif ("> \# end of Font\n");
}

sub write_mif_tab {
    &print_mif ("<Char Tab>\n");
}

sub write_mif_hard_space {
    &print_mif ("<Char HardSpace>\n");
}

sub write_mif_final {
    &print_mif ("\# End of MIFFile\n");
}

sub print_mif {
    local (@ARGS) = @_;
    &write_mif_indent;
    print OUTSTREAM (@ARGS);
}

sub write_mif_indent {
    local ($PAD) = ' ';
    if ($MIF_LEVEL) { printf OUTSTREAM ("%${MIF_LEVEL}s", $PAD); }
}

## Miscellany.

sub string_initial_cap {
    local ($STRING) = @_;
    local ($STRING1, $STRING2);
    ($STRING1 = $STRING) =~ s/^(.).*/$1/;
    ($STRING2 = $STRING) =~ s/^.//;
    $STRING1 =~ tr/a-z/A-Z/;
    $STRING2 =~ tr/A-Z/a-z/;
    "${STRING1}${STRING2}";
}

sub string_downcase {
    local ($STRING) = @_;
    $STRING =~ tr/A-Z/a-z/;
    $STRING;
}

sub string_trim_whitespace {
    local ($STRING) = @_;
    $STRING =~ s/^\s+//;
    $STRING =~ s/\s+$//;
    $STRING;
}

sub string_pluralize {
    local ($STRING) = @_;
    ($STRING =~ s/([^aeiouAEIOU])y$/${1}ies/) ||
    ($STRING =~ s/([^aeiouAEIOU])Y$/${1}IES/) ||
    ($STRING =~ s/([sxz])$/${1}es/) ||
    ($STRING =~ s/([SXZ])$/${1}ES/) ||
    ($STRING =~ s/([a-z])$/${1}s/) ||
    ($STRING =~ s/([A-Z])$/${1}S/);
    $STRING;
}

sub string_singularize {
    local ($STRING) = @_;
    ($STRING =~ s/[iI][eE]s$/y/) ||
	($STRING =~ s/[iI][eE]S$/Y/) ||
	    ($STRING =~ s/([sSxXzZ])[eE][sS]$/$1/) ||
		($STRING =~ s/[sS]$//);
    $STRING;
}

## Start the script.

&main;
