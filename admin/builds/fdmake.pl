#!/usr/local/bin/perl -w

use strict;
use File::Spec;
use Getopt::Long;
use XML::Parser;
use Config;

my $lidfile_line;

my $platform_name = $ENV{'OPEN_DYLAN_PLATFORM_NAME'};

my $library_extension = "so";
if($platform_name =~ m/darwin/) {
    $library_extension = "dylib";
}

my $user_root = $ENV{'OPEN_DYLAN_USER_ROOT'};
my $user_build = $ENV{'OPEN_DYLAN_USER_BUILD'};
my $user_install = $ENV{'OPEN_DYLAN_USER_INSTALL'};
my $user_projects = $ENV{'OPEN_DYLAN_USER_PROJECTS'};
my $user_registries = $ENV{'OPEN_DYLAN_USER_REGISTRIES'};
my $user_sources = $ENV{'OPEN_DYLAN_USER_SOURCES'};

my $build_logs = $ENV{'OPEN_DYLAN_BUILD_LOGS'};

my $verbose;
my $debugger;
my $compiler = 'fdcompile';
my @library_packs;
&GetOptions('verbose' => \$verbose,
	    'debugger' => \$debugger,
	    'compiler=s' => \$compiler,
	    'library-pack=s' => \@library_packs,
	    'library-packs=s' => sub {
		push @library_packs, split(/\W+/, $_[1]);
	    });

my %built;
my %deps;

foreach my $library (@ARGV) {
    if(!&build_library($library)) {
	print STDERR "fdmake: Unable to build library $library\n";
	exit 1;
    }
}
foreach my $pack (@library_packs) {
    &build_library_pack($pack);
}
exit 0;

# build_library($library)
#
# Builds the given libraries from source
#
sub build_library {
    my ($library) = @_;

    return $built{$library} if exists $built{$library};

    my $separator = quotemeta(($Config{'osname'} eq 'MSWin32') ? ';' : ':');
    my $lidfile;
    my $dir;
  REGISTRY:
    foreach my $registry (split /$separator/, $user_registries) {
	open(REGISTRY, '<', "$registry/$platform_name/$library")
	    || open(REGISTRY, '<', "$registry/generic/$library")
	    || next REGISTRY;
	my $line = <REGISTRY>;
	close(REGISTRY);

	my ($volume, $directories, undef) = File::Spec->splitpath($registry, 1);
	my @directories = File::Spec->splitdir($directories);

	lc(pop(@directories)) eq 'registry' or die;

	# abstract://dylan/environment/console/minimal-console-compiler.lid
	$line =~ s|^abstract://dylan/||;
	($dir, $lidfile) = ($line =~ m|(.*)/(.*)|);
	push @directories, File::Spec::Unix->splitdir($dir);
	$dir = File::Spec->catpath($volume,
				   File::Spec->catdir(@directories), '');
	$lidfile = File::Spec->catfile($dir, $lidfile);
	last REGISTRY;
    }

    if (!defined $lidfile) {
	return 0;
    }

    my $header = &parse_lid_file($lidfile);
    
    &scan_lidfile($lidfile, $header, $dir);

    my $needs_rebuild = !-f "$user_root/lib/lib${library}.${library_extension}";

    if(defined $deps{$library}) {
	foreach my $dep (@{$deps{$library}}) {
	    if(&build_library($dep) > 1) {
		$needs_rebuild = 1;
	    }
	}
    }

    if(!$needs_rebuild) {
	my $libdate = (stat "$user_root/lib/lib${library}.${library_extension}")[9];
	foreach my $source (split /\s+/, $$header{'files'}) {
	    unless($source =~ /\.dylan$/) {
		$source = "$source.dylan";
	    }
	    $source = File::Spec->catfile($dir, $source);
	    my $srcdate = (stat $source)[9]
		|| die "Library $library source file '$source' does not exist";
	    if($srcdate > $libdate) {
		print "$source causes rebuild of $library\n";
		$needs_rebuild = 1;
	    }
	}
	if(!$needs_rebuild) {
	    $built{$library} = 1;
	    return 1;
	}
    }

    my $other_files = $$header{'other-files'};
    if(defined $other_files) {
	foreach my $spec (split /\s+/, $other_files) {
	    if($spec =~ /\.spec$/) {
		&invoke_tool($library, $dir, File::Spec->catfile($dir, $spec));
	    }
	}
    }

    print "Building $library... ";

    my $command = $compiler;
    $command .= " -debugger" if($debugger);
    $command .= " $library";

    if(defined $build_logs && !$debugger) {
	$command .= " >$build_logs/compile-$library.txt";
    }

    system $command || die "Couldn't execute $compiler";
    if($? != 0
       || !(-f "$user_root/lib/lib${library}.${library_extension}"
	    || -f "$user_root/bin/$library")) {
	print "\n";
	if(defined $build_logs && !$debugger) {
	    print STDERR
		"fdmake: compile failed ($?), see ",
		"$build_logs/compile-$library.txt\n";
	}
	else {
	    print STDERR "fdmake: compile failed\n";
	}
	exit 1;
    }
    $built{$library} = 2;

    if(defined $build_logs && !$debugger) {
	my $warnings = 0;
	my $serious_warnings = 0;
	my $errors = 0;
	open(LOGFILE, '<', "$build_logs/compile-$library.txt")
	    || die "Couldn't open logfile: $!";
	while(<LOGFILE>) {
	    if(m|There were (\d+) warnings, (\d+) serious warnings and (\d+) errors|) {
		$warnings += $1;
		$serious_warnings += $2;
		$errors += $3;
	    }
	}
	close(LOGFILE);
	print "${warnings} W, ${serious_warnings} SW, ${errors} E\n";
    } else {
	print "\n";
    }

    return 2;
}

# invoke_tool($library, $dir, $spec)
#
# Constructs source files using an external build tool
#
sub invoke_tool {
    my ($library, $dir, $spec) = @_;

    open(SPEC, '<', $spec) || die "Couldn't open $spec: $!";
    $lidfile_line = 1;
    my $header = &parse_header(\*SPEC, $spec);

    my $origin = $$header{'origin'};

    print "Invoking the $origin tool for the $library library\n";

    if($origin =~ /\s*parser\s*/i) {
	&build_library('parser-compiler') || die "Can't build parser-compiler";

	my $source = File::Spec->catfile($dir, $$header{'parser'});
	my $output = File::Spec->catfile($dir, $$header{'output'});
	
	system "$user_root/bin/parser-compiler $source $output";
	if($? !=0 || ! -f $output) {
	    print STDERR "\nfdmake: Unable to build parser file\n";
	    exit 1;
	}
    } else {
	print STDERR "\nfdmake: unknown tool: $origin\n";
	exit 1;
    }
}

# build_library_pack($pack)
#
# Builds the libraries that constitute the given library pack
#
sub build_library_pack {
    my ($lp) = @_;

    my $lclp = lc($lp);

    my $dlpfile = "$user_sources/Library-Packs/$lp/$lclp.dlp";

    if(! -f $dlpfile) {
	print STDERR "fdmake: There is no library pack named $lp\n";
	exit 1;
    }

    my $parser = new XML::Parser(Handlers => {Start => \&handle_dlp_start});
    $parser->parsefile($dlpfile);
}

my $category = 'none';
sub handle_dlp_start {
    my ($parser, $element, %attributes) = @_;

    if($element eq 'library-pack') {
	print "Building $attributes{'title'} library-pack...\n";
    } elsif($element eq 'libraries'
	    || $element eq 'examples'
	    || $element eq 'test-suites') {
	$category = $element;
    } elsif($element eq 'library') {
	if($category eq 'libraries') {
	    if(!&build_library($attributes{'name'})) {
		print STDERR
		    "fdmake: Unable to build library $attributes{'name'}\n";
		exit 1;
	    }
	}
    }
}

# scan_lidfile($lidfile, $dir)
#
# Scan the given LID file and scan the one or more source files named
# therein that contain the Dylan-user module, until we find the
# "define library" form.
#
sub scan_lidfile {
    my ($lidfile, $header, $dir) = @_;

    my $library = lc($$header{'library'});

    if(!defined $library) {
	print STDERR "$lidfile: no `library:' keyword\n";
	return;
    }

    if($library eq 'dylan' || !defined $$header{'files'}) {
	return;
    }

    foreach my $source (split /\s+/, $$header{'files'}) {
	if($source !~ /.*\.dylan$/i) {
	    $source .= ".dylan";
	}
	$source = File::Spec->catfile($dir, $source);
	if(!&scan_dylan_user($source, $dir, $library)) {
	    return;
	}
    }
}

# scan_dylan_user()
#
# "Parse" the given Dylan source file until the "define library" form is
# located.  Returns 1 if the "define library" form hasn't been located
# yet (and there is still some hope of finding it).  When we find the
# definition, we scan it for "use" clauses (and ignore "export" clauses).
#
sub scan_dylan_user {
    my ($source, $dir, $library) = @_;

    my @useds;

    unless(open(SOURCE, '<', $source)) {
	print STDERR "can't open $source";
	return 0;
    }

    my $header = &parse_header(\*SOURCE, $source);
    my $module = $$header{'module'};
    if(!defined $module || $module !~ /dylan-user/i) {
	close(SOURCE);
	return 0;
    }
    #print STDERR "Scanning Dylan-user: $source\n";
    $_ = '';

    while(my $token = &dylan_token(\*SOURCE)) {
	if($token !~ /^define$/i) {
	    print STDERR "$source: didn't expect '$token' here\n";
	    close(SOURCE);
	    return 0;
	}
	$token = &dylan_token(\*SOURCE);
	if($token =~ /^module$/i) {
	    while(($token = &dylan_token(\*SOURCE)) && $token !~ /^end$/i) {
	    }
	    while(($token = &dylan_token(\*SOURCE)) && $token ne ';') {
	    }
	    next;
	} elsif($token !~ /^library$/i) {
	    print STDERR "$source: didn't expect 'define $token' here\n";
	    close(SOURCE);
	    return 0;
	}
	my $deflibrary = &dylan_token(\*SOURCE);
	$deflibrary =~ y/A-Z/a-z/;
	if($deflibrary ne $library) {
	    print STDERR $source, ": did not expect ";
	    print STDERR "define library $deflibrary ";
	    print STDERR "in library $library\n";
	    close(SOURCE);
	    return 0;
	}

	CLAUSE:
	while(($token = &dylan_token(\*SOURCE)) && $token !~ /^end$/i) {
	    if($token =~ /^use$/i) {
		my $used = &dylan_token(\*SOURCE);
		$used =~ y/A-Z/a-z/;
		&use_library($used, $dir, $library);
	    } elsif($token !~ /^export$/i) {
		print STDERR "$source: didn't expect '$token' here\n";
		close(SOURCE);
		return 0;
	    }
	    while(($token = &dylan_token(\*SOURCE)) && $token ne ';') {
		if($token =~ /^end$/i) {
		    last CLAUSE;
		}
	    }
	}
	close(SOURCE);
	return 0;
    }

    close(SOURCE);
    return 1;
}

# dylan_token()
#
# A tokenizer for the subset of Dylan tokens that can appear within
# "define library" and "define module" top-level defining forms.
#
sub dylan_token {
    my ($fh) = @_;
    while(1) {
	s|^\s+||;
	if($_ eq '') {
	    defined($_ = <$fh>) || return undef;
	    chop;
	    next;
	} elsif(s|^//.*||) {
	    next;
	} elsif(s|^/\*||) {
	    my $level = 1;
	    while($level > 0) {
		if($_ eq '') {
		    defined($_ = <$fh>) || return undef;
		    chop;
		    next;
		}
		s%^([^/*]|/[^*]|\*[^/])*%%;
		s%^[/*]$%%;
		if(s%\*/%%) {
		    --$level;
		} elsif(s%/\*%%) {
		    ++$level;
		}
	    }
	    next;
	} elsif(s|^([,;{}])||) {
	    return $1;
	} elsif(s|^(=>)||) {
	    return $1;
	} elsif(s|^\\?(([!&*<>;\|^\$%\@_][-0-9~&*<>\|^\$%\@_+~?/=]*)?[a-zA-Z][-a-zA-Z0-9~&!*<>\|^\$%\@_+~?/=]*:?)||) {
	    return $1;
	} elsif(s|^\\?([0-9][-0-9~&*<>\|^\$%\@_+~?/=]*([a-zA-Z][-0-9~&*<>\|^\$%\@_+~?/=]+)*[a-zA-Z][a-zA-Z][-a-zA-Z0-9~&!*<>\|^\$%\@_+~?/=]*:?)||) {
	    return $1;
	} elsif(s/^\#if\s*\(mindy\)//) {
	    while(!s/^\s*\#endif//) {
		defined($_ = <$fh>) || return undef;
		chop;
	    }
	    next;
	} elsif(s/^\#if\s*\(~mindy\)// || s/^\#endif//) {
	    next;
	} else {
	    die "Unrecognized token '$_'";
	}
    }
}

# use_library($used, $dir, $library)
#
# Record the fact that library $used is imported by $library, which is 
# found in $dir.
#
sub use_library {
    my ($used, $dir, $library) = @_;

    print "$library => $used\n" if($verbose);

    if(!exists $deps{$library}) {
	$deps{$library} = [];
    }
    push @{$deps{$library}}, $used;
}

# parse_lid_file($filename)
#
# Reads in a LID file, and returns an associative array where the
# keys are header keywords (mashed to lower case), and the values are
# the header values.  As a magic special case, the keyword 'files'
# contains all the files in the body of the lid file.
#
sub parse_lid_file {
    my ($lidfile) = @_;

    open(LIDFILE, $lidfile) || die("Can't open $lidfile: $!\n");
    $lidfile_line = 1;

    my $contents = &parse_header(\*LIDFILE, $lidfile);

    
#   # Read filenames
#     while (<LIDFILE>) {
#         $lidfile_line = $lidfile_line + 1;
#
# 	s/\r//g;		# Get rid of cross carriage returns
# 	chop;			# kill newline
# 	$$contents{'files'} .= " $_";
#     }
    close(LIDFILE);

    if(defined $$contents{'files'}) {
	# replace multiple spaces with single spaces
	$$contents{'files'} =~ s/\s+/ /g;
	
	# strip leading whitespace, which tends to screw up other parts of
	# gen-makefile
	$$contents{'files'} =~ s/^\s+//;
    }

    return $contents;
}

# parse_header($fh, $file)
#
# Reads the header (keyword: value, ...) from the filehandle $fh which
# is already open.  Returns a reference to an associative array where
# the keys are header keywords (mashed to lower case), and the values
# are the header values.
#
# Keywords can not appear more than once.  If they do, the last value
# will be used.  Multi-line values are supported, though.
#
sub parse_header {
    my ($fh, $file) = @_;

    my %contents;
    my $last_keyword;		# for multi-line values

    while (<$fh>) {
        $lidfile_line = $lidfile_line + 1;
	s/\r//g;		# Get rid of bogus carriage returns
	
        if (/^\s*$/) {		# if blank line, break out of loop
            return \%contents;
        } elsif (m|^//.*$|) {
            # comment line, ignore
        } elsif (/^\s+(.*)$/) {
            # Continuation line -- part of a multi-line value
            $contents{$last_keyword} .= ' ' . $1;
        } else {
            if(!/^([-A-Za-z0-9_!&*<>|^\$\%\@\?]+):\s*(.*)\s*$/) {
		print STDERR "$file:$lidfile_line: Warning: ",
		             "bad keyword line\n";
		next;
	    }
            my $keyword = lc($1);
            my $value = $2;
            $contents{$keyword} = $value;
            $last_keyword = $keyword;
        }
    }
    return \%contents;
}

