@echo off

set DIR=%1%
shift

echo Generating data file %DIR%\compiler-data.out ...
perl compiler-timings.pl -o %DIR%\compiler-data.out %DIR%\compile-*.log

