@echo off
rem rsh-hope-checkout.bat        gts, 11/97
rem change hostname to your liking
set hostname=oroboros.functionalobjects.com
rem should be something like ~dylan/admin/builds/from-pc.pl
set unixscript=/u/dylan/admin/builds/from-pc.pl
set pcpathpart="z:"
set unixpathpart="your-home-directory-here e.g. /u/gts"
set cmd=hope checkout %*
echo doing rsh %hostname% %unixscript% %pcpathpart% %unixpathpart% '%cmd%'
rsh %hostname% %unixscript% %pcpathpart% %unixpathpart% '%cmd%'
