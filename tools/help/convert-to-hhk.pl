#!/usr/local/bin/perl

($input, $output) = @ARGV;

open(OUTPUT, ">$output") || die("Error, could not open output file \"$output\": $@\n");

print OUTPUT "<HTML>\n<!-- Sitemap 1.0 -->\n<OBJECT type=\"text/site properties\">\n\t<param name=\"ExWindow Styles\" value=\"0x4200\">\n\t<param name=\"Window Styles\" value=\"0x800027\">\n</OBJECT>\n";

open(INPUT, $input) || die("Error, could not open input file \"$input\": $@\n");
$doing = 0;
$/ = "\n";
while ($line = <INPUT>) {
#    print "DBG: line length: ", length($line), "\n";
    if ($line =~ /<HR>/) {
	$doing = 1 - $doing;
#	print "DBG: doing: ", $doing, "\n";
    };
    next if (! $doing );
    if ($line =~ /^<A /) { # single entry
	($label) = ($line =~ m+>(.*)</A>+);
	($url) = ($line =~ m+HREF=\"(.*)\"+);
	$label = &unescape_html_symbols($label);
	print OUTPUT "\t<LI> <OBJECT type=\"text/sitemap\">\n\t\t<param name=\"Name\" value=\"$label\">\n\t\t<param name=\"Local\" value=\"$url\">\n\t\t<param name=\"FrameName\" value=\"content\">\n\t\t</OBJECT>\n";
    } elsif ($line =~ /^([^<]*)( <A .*)/) { # multiple entry
	$rootlabel = $1;
	@entries = split(m+</A> <A+, $2);
	foreach $entry (@entries) {
	    ($url, $leaflabel) = ($entry =~ m+HREF=\"(.*)\">([^<]*)+);
	    $label = $rootlabel . " " . $leaflabel;
	    $label = &unescape_html_symbols($label);
	    print OUTPUT "\t<LI> <OBJECT type=\"text/sitemap\">\n\t\t<param name=\"Name\" value=\"$label\">\n\t\t<param name=\"Local\" value=\"$url\">\n\t\t<param name=\"FrameName\" value=\"content\">\n\t\t</OBJECT>\n";
	}
    }
}
close(INPUT);

print OUTPUT "</HTML>\n";

close(OUTPUT);

sub unescape_html_symbols {
    local($label) = @_;
    $label =~ s/&gt/>/g;
    $label =~ s/&lt/</g;
    $label =~ s/\\/\\\\/g;
    $label =~ s/&quot/\\\"/g;
    $label;
}
