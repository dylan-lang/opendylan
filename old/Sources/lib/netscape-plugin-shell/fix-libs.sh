#!/bin/sh -f

echo "DLIBS = \\" > libs.mak
sed -e "s/.*/	& \\\\/" libs.lnk >> libs.mak
## This line is needed because libs.lnk doesn't end with a newline,
## and that means sed doesn't see the last line
tail -1 libs.lnk >> libs.mak
## make sure there's a newline
echo " " >> libs.mak
