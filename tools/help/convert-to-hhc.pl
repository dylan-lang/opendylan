#!/usr/local/bin/perl

($input, $output) = @ARGV;

open(OUTPUT, ">$output") || die("Error, could not open output file \"$output\": $@\n");

print OUTPUT "<HTML>\n<!-- Sitemap 1.0 -->\n<OBJECT type=\"text/site properties\">\n\t<param name=\"ExWindow Styles\" value=\"0x4200\">\n\t<param name=\"Window Styles\" value=\"0x800027\">\n</OBJECT>\n<UL>\n\t<LI> <OBJECT type=\"text/sitemap\">\n\t\t<param name=\"Name\" value=\"Dylan Reference Manual\">\n\t\t</OBJECT>\n\t<UL>\n";

open(INPUT, $input) || die("Error, could not open input file \"$input\": $@\n");
$firstheading = 1;
while ($line = <INPUT>) {
    if ($line =~ /<HR>/) {
	$doing = 1 - $doing;
    };
    next if (! $doing);
    if ($line =~ /^<A /) { # heading
	($label1, $label2) = ($line =~ m+<B>(.*)</B>(.*)</A>+);
	$label = $label1 . $label2;
	($url) = ($line =~ m+HREF=\"(.*)\"+);
	unless ($firstheading) {
	    print OUTPUT "\t\t</UL>\n";
	}
	print OUTPUT "\t\t<LI> <OBJECT type=\"text/sitemap\">\n\t\t\t<param name=\"Name\" value=\"$label\">\n\t\t\t</OBJECT>\n\t\t<UL>\n";
	print OUTPUT "\t\t\t<LI> <OBJECT type=\"text/sitemap\">\n\t\t\t\t<param name=\"Name\" value=\"$label\">\n\t\t\t\t<param name=\"Local\" value=\"$url\">\n\t\t\t\t<param name=\"FrameName\" value=\"content\">\n\t\t\t\t</OBJECT>\n";
	$line =~ s/^- //;
	$line =~ s/<BR>//;
	print OUTPUT "\t\t\t\t$line";
	$firstheading = 0;
    } elsif ($line =~ /^- <A /) { # reference
	($url) = ($line =~ m+HREF=\"(.*)\"+);
	($label) = ($line =~ m+>(.*)</A>+);
	print OUTPUT "\t\t\t<LI> <OBJECT type=\"text/sitemap\">\n\t\t\t\t<param name=\"Name\" value=\"$label\">\n\t\t\t\t<param name=\"Local\" value=\"$url\">\n\t\t\t\t<param name=\"FrameName\" value=\"content\">\n\t\t\t\t</OBJECT>\n";
	$line =~ s/^- //;
	$line =~ s/<BR>//;
	print OUTPUT "\t\t\t\t$line";
    }
}
close(INPUT);

print OUTPUT "\t\t</UL>\n\t</UL>\n</UL>\n</HTML>\n";

close(OUTPUT);
