#!/usr/local/bin/perl -w

use strict;
use File::Spec;
use Getopt::Long;
use Config;

my $lidfile_line;

my $platform_name = $ENV{'OPEN_DYLAN_PLATFORM_NAME'};

my $user_root = $ENV{'OPEN_DYLAN_USER_ROOT'};
my $user_registries = $ENV{'OPEN_DYLAN_USER_REGISTRIES'};
my $user_sources = $ENV{'OPEN_DYLAN_USER_SOURCES'};

my $build_logs = $ENV{'OPEN_DYLAN_BUILD_LOGS'};

my $verbose;
my $debugger;
my $compiler = 'dylan-compiler';
&GetOptions('verbose' => \$verbose,
            'debugger' => \$debugger,
            'compiler=s' => \$compiler);

# Names of libraries we already built successfully.
my %built;
my %deps;

foreach my $library (@ARGV) {
    if(!&build_library($library)) {
        print STDERR "fdmake: Unable to build library $library\n";
        exit 1;
    }
}
exit 0;

# build_library($library)
#
# Builds the given libraries from source
#
sub build_library {
    my ($library) = @_;

    if (exists $built{$library}) {
        return $built{$library};
    }

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

        pop(@directories) =~ /\bregistry$/i or die;

        # abstract://dylan/environment/console/dylan-compiler.lid
        $line =~ s|^abstract://dylan/||;
        ($dir, $lidfile) = ($line =~ m|(.*)/(.*)|);
        push @directories, File::Spec::Unix->splitdir($dir);
        $dir = File::Spec->catdir($volume, @directories);
        $lidfile = File::Spec->catfile($dir, $lidfile);
        last REGISTRY;
    }

    if (!defined $lidfile) {
        return 0;
    }

    my $header = &parse_lid_file($lidfile, $dir);

    my $other_files = $header->{'other-files'};
    if (defined $other_files) {
        foreach my $spec (split /\s+/, $other_files) {
            if ($spec =~ /\.spec$/) {
		my $absspec = File::Spec->catfile($dir, $spec);
		my ($specvol, $specdir, undef)
		    = File::Spec->splitpath($absspec);
		my $sdir = File::Spec->catdir($specvol, $specdir);
                &invoke_tool($library, $sdir, $absspec);
            }
        }
    }

    &scan_lidfile($lidfile, $header, $dir);

    my @products = &library_products($library, $header);

    my $needs_rebuild;
    for my $product (@products) {
        $needs_rebuild ||= !-f $product;
    }

    my $dbfile = File::Spec->catfile($user_root, 'databases', "${library}.ddb");
    my $dbdate;
    $needs_rebuild ||= !-f $dbfile;
    if (!$needs_rebuild) {
        $dbdate = (stat $dbfile)[9];
    }

    if(defined $deps{$library}) {
        foreach my $dep (@{$deps{$library}}) {
            my $date = &build_library($dep);
            if (!$needs_rebuild && $date > $dbdate) {
                #print "library $dep causes rebuild of $library\n";
                $needs_rebuild = 1;
            }
        }
    }

    if (!$needs_rebuild && (stat $lidfile)[9] > $dbdate) {
        #print "$lidfile causes rebuild of $library\n";
        $needs_rebuild = 1;
    }

    my %fullpath;
    foreach my $source (split /\s+/, $header->{'files'}) {
        unless($source =~ /\.dylan$/) {
            $source = "$source.dylan";
        }
        $source = File::Spec->catfile($dir, $source);
        my $srcdate = (stat $source)[9]
            || die "Library $library source file '$source' does not exist";
        if(!$needs_rebuild && $srcdate > $dbdate) {
            #print "$source causes rebuild of $library\n";
            $needs_rebuild = 1;
        }

        my (undef, undef, $base) = File::Spec->splitpath($source);
        $fullpath{$base} = $source;
    }

    if(!$needs_rebuild) {
        $built{$library} = $dbdate;
        return $dbdate;
    }

    print "Building $library... ";

    my $command = $compiler;
    if ($debugger) {
	$command .= " -debugger";
    }
    if (exists $header->{'target-type'}) {
	$command .= " -target " . $header->{'target-type'};
    }
    $command .= " $library";

    if ($debugger) {
	system($command) or die "Couldn't execute $command";
	print "\n";
    }
    else {
	open(my $compfd, '-|', $command) or die "Couldn't execute $compiler";

	my $logfd;
	if (defined $build_logs) {
	    my $log = File::Spec->catfile($build_logs, "compile-$library.txt");
	    open($logfd, '>', $log) or die "Couldn't write to $log: $!";
	}

        my $warnings = 0;
        my $serious_warnings = 0;
        my $errors = 0;

	my $printed;
        my $prefix;
        while(<$compfd>) {
	    if (defined $logfd) {
		print $logfd $_;
	    }

            if(m|There were (\d+) warnings, (\d+) serious warnings and (\d+) errors|) {
                $warnings += $1;
                $serious_warnings += $2;
                $errors += $3;
            }
            elsif (m|^Warning at (.+):(\d+(-\d+)?):|) {
		my $source = (exists $fullpath{$1}) ? $fullpath{$1} : $1;
                $prefix = "$source:$2: warning: ";
            }
            elsif (m|^Serious warning at (.+):(\d+(-\d+)?):|) {
		my $source = (exists $fullpath{$1}) ? $fullpath{$1} : $1;
                $prefix = "$source:$2: serious warning: ";
            }
            elsif (m|^Error at (.+):(\d+(-\d+)?):|) {
		my $source = (exists $fullpath{$1}) ? $fullpath{$1} : $1;
                $prefix = "$source:$2: error: ";
            }
            elsif (defined $prefix && !m|^$|) {
		if (!$printed) {
		    print "\n";
		    $printed = 1;
		}
		print $prefix . $_;
                undef $prefix;
            }
        }

        print "${warnings} W, ${serious_warnings} SW, ${errors} E\n";

	if (defined $logfd) {
	    close($logfd);
	}
	close($compfd);
    }
    my $failed = $? != 0;

    foreach my $product ($dbfile, @products) {
        if (!-f $product) {
            print STDERR "fdmake: build product $product missing\n";
            $failed = 1;
        }
    }

    if ($failed) {
        print "\n";
        if(defined $build_logs && !$debugger) {
            print STDERR
                "fdmake: compile failed ($?), see ",
		File::Spec->catfile($build_logs, "compile-$library.txt"), "\n";
        }
        else {
            print STDERR "fdmake: compile failed\n";
        }
        exit 1;
    }

    $dbdate = (stat $dbfile)[9];
    $built{$library} = $dbdate;
    return $dbdate;
}

sub library_products {
    my ($library, $header) = @_;

    my $executable = $library;
    if (defined $header->{'executable'}) {
        $executable = $header->{'executable'};
    }

    my @products;

    if ($platform_name =~ /-win32$/) {
        if (!defined $header->{'target-type'}
            || lc($header->{'target-type'}) eq 'dll') {
	    push(@products,
		 File::Spec->catfile($user_root, 'bin', "${executable}.dll"));
	    push(@products,
		 File::Spec->catfile($user_root, 'lib', "${executable}.lib"));
	}
        if (!defined $header->{'target-type'}
            || lc($header->{'target-type'}) eq 'executable') {
            push(@products,
		 File::Spec->catfile($user_root, 'bin', "${executable}.exe"));
	}
    }
    else {
        my $so = ($platform_name =~ /-darwin$/) ? 'dylib' : 'so';
        push(@products,
             File::Spec->catfile($user_root, 'lib', "lib${executable}.${so}"));

        if (!defined $header->{'target-type'}
            || lc($header->{'target-type'}) eq 'executable') {
            push @products, File::Spec->catfile($user_root, 'bin', $executable);
        }
    }

    return @products;
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

    my $origin = $header->{'origin'};

    if ($origin =~ /\A\s*parser\s*\Z/i) {
        my $source = File::Spec->catfile($dir, $header->{'parser'});
        my $output = File::Spec->catfile($dir, $header->{'output'});

	if (!-f $output || (stat $source)[9] > (stat $output)[9]) {
	    &build_library('parser-compiler')
		or die "Can't build parser-compiler";
	    my $parser_compiler
		= File::Spec->catfile($user_root, 'bin', 'parser-compiler');

	    print "Invoking the $origin tool for the $library library\n";
	    system "$parser_compiler $source >$output";
	    if($? !=0 || ! -f $output) {
		print STDERR "\nfdmake: Unable to build parser file\n";
		exit 1;
	    }
	}
    }
    elsif ($origin =~ /\A\s*omg-idl\s*\Z/i) {
	my $idlfile = File::Spec->catfile($dir, $header->{'idl-file'});
	my ($idlvol, $idldir, undef) = File::Spec->splitpath($idlfile);

	my $needs_rebuild = 0;
	my @options;
	my $prefix = '';
	if (defined $header->{'prefix'}) {
	    push @options, '-prefix:' . $header->{'prefix'};
	    $prefix = $header->{'prefix'} . '-';
	}
	if (defined $header->{'main'}
	    && $header->{'main'} =~ /\A\s*yes\s*\Z/i) {
	    push @options, '-main';
	}

	if (defined $header->{'protocol'}
	    && $header->{'protocol'} =~ /\A\s*yes\s*\Z/i) {
	    push @options, '-protocol';

	    my $name = $prefix . 'protocol';
	    my $genfile
		= File::Spec->catfile($idlvol, $idldir, $name, $name.'.dylan');
	    if (!-f $genfile || (stat $idlfile)[9] > (stat $genfile)[9]) {
		$needs_rebuild = 1;
	    }
	}
	if (defined $header->{'skeletons'}
	    && $header->{'skeletons'} =~ /\A\s*yes\s*\Z/i) {
	    push @options, '-skeletons';

	    my $name = $prefix . 'skeletons';
	    my $genfile
		= File::Spec->catfile($idlvol, $idldir, $name, $name.'.dylan');
	    if (!-f $genfile || (stat $idlfile)[9] > (stat $genfile)[9]) {
		$needs_rebuild = 1;
	    }
	}
	if (defined $header->{'stubs'}
	    && $header->{'stubs'} =~ /\A\s*yes\s*\Z/i) {
	    push @options, '-stubs';

	    my $name = $prefix . 'stubs';
	    my $genfile
		= File::Spec->catfile($idlvol, $idldir, $name, $name.'.dylan');
	    if (!-f $genfile || (stat $idlfile)[9] > (stat $genfile)[9]) {
		$needs_rebuild = 1;
	    }
	}

	if (defined $header->{'include'}) {
	    push @options, '-include';
	    push @options, File::Spec->catdir($dir, $header->{'include'});
	}

	if (defined $header->{'libraries'}) {
	    push @options, '-libraries', $header->{'libraries'};
	}
	if (defined $header->{'modules'}) {
	    push @options, '-modules', $header->{'modules'};
	}

	if ($needs_rebuild) {
	    &build_library('minimal-console-scepter')
		or die "Can't build scepter";
	    my $scepter
		= File::Spec->catfile($user_root,
				      'bin', 'minimal-console-scepter');

	    print "Invoking the $origin tool for the $library library\n";
	    system($scepter, @options, $idlfile);
	    if ($? != 0) {
		print STDERR "\nfdmake: Unable to build $origin\n";
		exit 1;
	    }
	}
    }
    else {
        print STDERR "\nfdmake: unknown tool: $origin\n";
        exit 1;
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

    my $library = lc($header->{'library'});

    if(!defined $library) {
        print STDERR "$lidfile: no `library:' keyword\n";
        return;
    }

    if($library eq 'dylan' || !defined $header->{'files'}) {
        return;
    }

    foreach my $source (split /\s+/, $header->{'files'}) {
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
    my $module = $header->{'module'};
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

# parse_lid_file($filename, $dir)
#
# Reads in a LID file, and returns an associative array where the
# keys are header keywords (mashed to lower case), and the values are
# the header values.  As a magic special case, the keyword 'files'
# contains all the files in the body of the lid file.
#
sub parse_lid_file {
    my ($lidfile, $dir) = @_;

    open(LIDFILE, $lidfile) || die("Can't open $lidfile: $!\n");
    $lidfile_line = 1;

    my $contents = &parse_header(\*LIDFILE, $lidfile);
    
#   # Read filenames
#     while (<LIDFILE>) {
#         $lidfile_line = $lidfile_line + 1;
#
#       s/\r//g;                # Get rid of cross carriage returns
#       chop;                   # kill newline
#       $contents->{'files'} .= " $_";
#     }
    close(LIDFILE);

    if(defined $contents->{'files'}) {
        # replace multiple spaces with single spaces
        $contents->{'files'} =~ s/\s+/ /g;
        
        # strip leading whitespace, which tends to screw up other parts of
        # gen-makefile
        $contents->{'files'} =~ s/^\s+//;
    }

    while (my $lid = delete $contents->{'lid'}) {
        $lid = File::Spec->catfile($dir, $lid);
        my $lid_contents = &parse_lid_file($lid, $dir);
        while (my ($key, $value) = each %$lid_contents) {
            if (!exists $contents->{$key}) {
                $contents->{$key} = $value;
            }
        }
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
    my $last_keyword;           # for multi-line values

    while (<$fh>) {
        $lidfile_line = $lidfile_line + 1;
        s/\r//g;                # Get rid of bogus carriage returns
        
        if (/^\s*$/) {          # if blank line, break out of loop
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
