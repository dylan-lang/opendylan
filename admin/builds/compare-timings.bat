@echo off

rem usage: compare-timings.bat dir1 dir2


set DIR1=%1%
shift
set DIR2=%1%
shift

echo Comparing timings between
echo  (1) %DIR1%
echo  (2) %DIR2%

if exist %DIR1%\compiler-data.out goto TESTDIR2
echo Collecting timings from %DIR1%\compile*.log...
call generate-compiler-timings %DIR1%

:TESTDIR2
if exist %DIR2%\compiler-data.out goto DOCOMPS
echo Collecting timings from %DIR2%\compile*.log...
call generate-compiler-timings %DIR2%

:DOCOMPS
perl compare-timings.pl %DIR2%\compiler-data.out
rem -e flag looks for regression in warning counts
perl compare-timings.pl -e %DIR1%\compiler-data.out %DIR2%\compiler-data.out
