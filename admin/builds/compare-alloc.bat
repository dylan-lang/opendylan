@echo off

rem usage: compare-alloc.bat dir1 dir2


set DIR1=%1%
shift
set DIR2=%1%
shift

echo Comparing allocation between
echo  (1) %DIR1%
echo  (2) %DIR2%

if exist %DIR1%\compiler-alloc.out goto TESTDIR2
echo Collecting allocation data from %DIR1%\compile*.log...
perl compiler-timings.pl -ao %DIR1%\compiler-alloc.out %DIR1%\compile-*.log

:TESTDIR2
if exist %DIR2%\compiler-alloc.out goto DOCOMPS
echo Collecting allocation data from %DIR2%\compile*.log...
perl compiler-timings.pl -ao %DIR2%\compiler-alloc.out %DIR2%\compile-*.log

:DOCOMPS
perl compare-timings.pl %DIR1%\compiler-alloc.out %DIR2%\compiler-alloc.out

