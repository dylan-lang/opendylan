#!/usr/local/bin/perl

######!/u/gts/perl5/bin/perl
$pcpathpart = shift;
$unixpathpart = shift;
$cmd = "@ARGV";
print "cmd = $cmd.\n";
$cmd =~ s#\\#/#g;
$cmd =~ s#$pcpathpart#$unixpathpart#;
print "on unix, pcpart=$pcpathpart, unixpart=$unixpathpart.\n";
print "on unix, doing $cmd...";
$result = `$cmd`;
print "... done ($result).\n";
exit 0;  
