REM  "gema" test script for MS-DOS
REM   $Id: test.bat,v 1.1 2004/03/12 00:42:10 cgay Exp $
erase test.out
erase test2.out
erase test3.out
erase test4.out
gema -version -f testpat.dat testin.dat test.out
fc testout.dat test.out
gema -f testpat.dat -i -idchars "-_" -out test2.out test2.dat
fc test2out.dat test2.out
gema -f testpat.dat -filechars ".,:/\\-_" -out test3.out -in - < test3.dat
fc test3out.dat test3.out
gema -t -f testtok.dat testin.dat test4.out
fc testout.dat test4.out
GEMA -T -W -MATCH -F TESTTW.DAT -ODIR %TEMP% -OTYP .OUT TEST5IN.DAT
fc test5out.dat %TEMP%\test5in.out
