#!/usr/local/bin/perl

$output = "dylan-tempo.el";

unlink($output); # clear the results file

open(OUTPUT, ">>$output") || die("Error, cannot append to output file \"$output\": $@\n");

# some comments for the top of the file
print OUTPUT ";;;; Module: Dylan Source Templates\n";
print OUTPUT ";;;; Author: Jason Trenouth\n";
print OUTPUT ";;;; Copyright: Copyright 1997 The Harlequin Group Limited.  All rights reserved.\n";
print OUTPUT ";;;; \n";
print OUTPUT ";;;; The contents of this file are automatically generated from\n";
print OUTPUT ";;;; some editor-independent templates.\n";
print OUTPUT ";;;; \n";
print OUTPUT ";;;; See /usr/local/soft/emacs-19.30/run/hqn/sol2.3_sparc/share/emacs/19.30/lisp/tempo.el\n\n";
print OUTPUT "(require 'tempo)\n\n";
print OUTPUT "(defvar *dylan-tempo-tags* \'() \"An association list with tags and corresponding templates,
for Dylan mode.\")\n\n";

foreach $input (@ARGV) {
    ($name) = ($input =~ m+.*/(.*)+); # extract leaf file name
    print OUTPUT "(tempo-define-template\n";
    print OUTPUT " \"$name\"\n";
    print OUTPUT " \'(\n";
    open(INPUT, $input) || die("Error, cannot open input file \"$input\": $@\n");
    $abbrev = <INPUT>;
    chop($abbrev);
    while($line = <INPUT>) {
	chop($line);
	if ($line =~ /---\*\*\*/) { # ignore Hugh's TBD lines
	    next;
	}
	$line =~ s/\`/\\`/g; # escape back quotes
        $line =~ s/\"/\\"/g; # escape double quotes
        $line =~ s/(.*)%e(.*)/\1\" \'p \"\2/g; # spot points of interest
        $line =~ s/(.*)%\(([^\)]*)\)(.*)/\1\2\3/g; # strip out regions
	print OUTPUT "   \"$line\" '> 'n\n";
    };
    close(INPUT);
    print OUTPUT " )\n";
    print OUTPUT " \"$abbrev\"\n";
    print OUTPUT " \"\"\n";
    print OUTPUT " *dylan-tempo-tags*)\n\n";
};
close(OUTPUT);
