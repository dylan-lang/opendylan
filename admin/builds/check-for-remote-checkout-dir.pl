# usage: perl check-for-remote-checkout-dir.pl remote-dir hope-args
#
# * writes the "continuation" script hope-checkout-continue.bat
# * if find the $remote-dir in the $hope-args string, then do 
#   a remote hope checkout next, 
# * otherwise, do local hope checkout next.
#                                              (gts, 12/97)

$thedir = @ARGV[0];
shift;
$therest = "@ARGV";

open (OUTF, ">hope-checkout-continue.bat");
if ($therest =~ /directory.*$thedir/) {
    print "Will do a remote hope checkout...\n";
    print OUTF "rsh-hope-checkout $therest\n";
}
else {
    print "Will do a local hope checkout...\n";
    print OUTF "hope checkout $therest\n";
}
close OUTF;
exit;
